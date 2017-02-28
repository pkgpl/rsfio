! Program :
! Author  : wansooha@gmail.com
! Date    :

! supported form: native, ascii
! supported type: float, int, complex
module rsfio_mod
    implicit none
    private
    integer,parameter:: STDIN=5,STDOUT=6,STDERR=0
    integer,parameter:: MXNSTR=256

    type Axis_t
        integer:: n=0
        real:: o=0.0,d=0.0
        character(len=:),allocatable:: label,unit
    end type

    type rsf_t
        character(len=:),allocatable:: in
        character(len=:),allocatable:: data_format,filename
        integer:: esize,itr=0,iunit
        type(Axis_t):: axes(9)
        logical:: data_opened=.false.,wrote_header=.false.
        contains
            procedure,public:: write_header => rsf_write_header
            procedure,public:: ntr => rsf_ntr

            procedure,public:: n => get_n
            procedure,public:: o => get_o
            procedure,public:: d => get_d
            procedure,public:: label => get_label
            procedure,public:: unit => get_unit
            procedure,public:: close => rsf_close

            generic,public:: read => rsf_read_i,rsf_read_f,rsf_read_d,rsf_read_c,rsf_read_z, &
                                     rsf_read_i2,rsf_read_f2,rsf_read_d2,rsf_read_c2,rsf_read_z2, &
                                     rsf_read_i3,rsf_read_f3,rsf_read_d3,rsf_read_c3,rsf_read_z3
            generic,public:: write => rsf_write_i,rsf_write_f,rsf_write_d,rsf_write_c,rsf_write_z, &
                                      rsf_write_i2,rsf_write_f2,rsf_write_d2,rsf_write_c2,rsf_write_z2, &
                                      rsf_write_i3,rsf_write_f3,rsf_write_d3,rsf_write_c3,rsf_write_z3

            procedure,private:: form => rsf_form
            procedure,private:: type => rsf_type

            procedure,private:: rsf_read_i
            procedure,private:: rsf_read_f
            procedure,private:: rsf_read_d
            procedure,private:: rsf_read_c
            procedure,private:: rsf_read_z
            procedure,private:: rsf_read_i2
            procedure,private:: rsf_read_f2
            procedure,private:: rsf_read_d2
            procedure,private:: rsf_read_c2
            procedure,private:: rsf_read_z2
            procedure,private:: rsf_read_i3
            procedure,private:: rsf_read_f3
            procedure,private:: rsf_read_d3
            procedure,private:: rsf_read_c3
            procedure,private:: rsf_read_z3

            procedure,private:: rsf_write_i
            procedure,private:: rsf_write_f
            procedure,private:: rsf_write_d
            procedure,private:: rsf_write_c
            procedure,private:: rsf_write_z
            procedure,private:: rsf_write_i2
            procedure,private:: rsf_write_f2
            procedure,private:: rsf_write_d2
            procedure,private:: rsf_write_c2
            procedure,private:: rsf_write_z2
            procedure,private:: rsf_write_i3
            procedure,private:: rsf_write_f3
            procedure,private:: rsf_write_d3
            procedure,private:: rsf_write_c3
            procedure,private:: rsf_write_z3

            procedure,private:: assert_type

    end type

    public:: rsf_t,Axis_t
    public:: rsf_input,rsf_output
    public:: print_axis

contains

    subroutine print_axis(ax,iunit,id)
    class(Axis_t),intent(in):: ax
    integer,intent(in),optional:: iunit,id
    integer un
    character(len=16):: strn,stro,strd
    character:: cid
    if(present(iunit)) then
        un=iunit
    else
        un=STDOUT
    endif
    write(strn,*) ax%n
    write(stro,'(f10.5)') ax%o
    write(strd,'(f10.5)') ax%d
    if(present(id)) then
        write(cid,'(i0)') id
        write(un,'(a)',advance='no') 'n'//cid//'='//trim(adjustl(strn))
        write(un,'(a)',advance='no') ' d'//cid//'='//trim(adjustl(strd))
        write(un,'(a)',advance='no') ' o'//cid//'='//trim(adjustl(stro))
        write(un,'(a)',advance='no') ' label'//cid//'='//trim(adjustl(quote(ax%label)))
        write(un,'(a)') ' unit'//cid//'='//trim(adjustl(quote(ax%unit)))
    else
        write(un,'(a)',advance='no') "n="//trim(adjustl(strn))
        write(un,'(a)',advance='no') " o="//trim(adjustl(stro))
        write(un,'(a)',advance='no') " d="//trim(adjustl(strd))
        write(un,'(a)',advance='no') " label="//quote(ax%label)
        write(un,'(a)') " unit="//quote(ax%unit)
    endif
    end subroutine

! data format
    function rsf_form(sf) result(v)
    class(rsf_t),intent(in):: sf
    character(len=:),allocatable:: v
    integer id
    id=index(sf%data_format,'_')
    v=sf%data_format(1:id-1)
    end function

    function rsf_type(sf) result(v)
    class(rsf_t),intent(in):: sf
    character(len=:),allocatable:: v
    integer id
    id=index(sf%data_format,'_')
    v=sf%data_format(id+1:)
    end function

    function rsf_ntr(sf) result(ntr)
    class(rsf_t),intent(in):: sf
    integer:: ntr,i
    ntr=1
    do i=2,9
        if(sf%axes(i)%n>0) then
            ntr=ntr*sf%axes(i)%n
        endif
    enddo
    end function

    subroutine rsf_data_open(sf,mode)
    class(rsf_t),intent(inout):: sf
    character,intent(in):: mode
    character(len=:),allocatable:: act,stat
    if(sf%data_opened) return
    if(mode=='r') then
        act='read'
        stat='old'
    elseif(mode=='w') then
        act='write'
        stat='replace'
    endif
    sf%iunit=assign_unit()
    if(sf%form().eq.'native') then
        open(sf%iunit,file=sf%in,access='direct',action=act,status=stat,recl=sf%esize*sf%axes(1)%n)
    elseif(sf%form().eq.'ascii') then
        open(sf%iunit,file=sf%in,action=act,status=stat)
    else
        call errexit("Wrong format: "//trim(sf%form()))
    endif
    sf%data_opened=.true.
    sf%itr=0
    end subroutine

    function get_n(sf,id) result(v)
    class(rsf_t),intent(in):: sf
    integer,intent(in):: id
    integer:: v
    v=sf%axes(id)%n
    end function
    function get_o(sf,id) result(v)
    class(rsf_t),intent(in):: sf
    integer,intent(in):: id
    real(kind=4):: v
    v=sf%axes(id)%o
    end function
    function get_d(sf,id) result(v)
    class(rsf_t),intent(in):: sf
    integer,intent(in):: id
    real(kind=4):: v
    v=sf%axes(id)%d
    end function
    function get_label(sf,id) result(v)
    class(rsf_t),intent(in):: sf
    integer,intent(in):: id
    character(len=:),allocatable:: v
    v=sf%axes(id)%label
    end function
    function get_unit(sf,id) result(v)
    class(rsf_t),intent(in):: sf
    integer,intent(in):: id
    character(len=:),allocatable:: v
    v=sf%axes(id)%unit
    end function

! read data
    subroutine rsf_read_i(sf,arr)
    class(rsf_t),intent(inout):: sf
    integer,intent(out):: arr(:)
    integer:: i
    call sf%assert_type('int')
    call rsf_data_open(sf,'r')
    if(sf%form() .eq.'native') then
        sf%itr=sf%itr+1
        if(sf%itr<=sf%ntr()) then
            read(sf%iunit,rec=sf%itr) (arr(i),i=1,sf%axes(1)%n)
        endif
    elseif(sf%form() .eq.'ascii') then
        read(sf%iunit,*,end=997) (arr(i),i=1,sf%axes(1)%n)
    else
        call errexit("Wrong form")
    endif
    return
997 close(sf%iunit)
    end subroutine

    subroutine rsf_read_f(sf,arr)
    class(rsf_t),intent(inout):: sf
    real(kind=4),intent(out):: arr(:)
    integer:: i
    call sf%assert_type('float')
    call rsf_data_open(sf,'r')
    if(sf%form() .eq.'native') then
        sf%itr=sf%itr+1
        if(sf%itr<=sf%ntr()) then
            read(sf%iunit,rec=sf%itr) (arr(i),i=1,sf%axes(1)%n)
        endif
    elseif(sf%form() .eq.'ascii') then
        read(sf%iunit,*,end=997) (arr(i),i=1,sf%axes(1)%n)
    else
        call errexit("Wrong form")
    endif
    return
997 close(sf%iunit)
    end subroutine
    subroutine rsf_read_d(sf,arr)
    class(rsf_t),intent(inout):: sf
    real(kind=8),intent(out):: arr(:)
    real(kind=4):: sarr(size(arr))
    call sf%read(sarr)
    arr=sarr
    end subroutine

    subroutine rsf_read_c(sf,arr)
    class(rsf_t),intent(inout):: sf
    complex(kind=4),intent(out):: arr(:)
    integer:: i
    call sf%assert_type('complex')
    call rsf_data_open(sf,'r')
    if(sf%form() .eq.'native') then
        sf%itr=sf%itr+1
        if(sf%itr<=sf%ntr()) then
            read(sf%iunit,rec=sf%itr) (arr(i),i=1,sf%axes(1)%n)
        endif
    elseif(sf%form() .eq.'ascii') then
        read(sf%iunit,*,end=997) (arr(i),i=1,sf%axes(1)%n)
    else
        call errexit("Wrong form")
    endif
    return
997 close(sf%iunit)
    end subroutine
    subroutine rsf_read_z(sf,arr)
    class(rsf_t),intent(inout):: sf
    complex(kind=8),intent(out):: arr(:)
    complex(kind=4):: sarr(size(arr))
    call sf%read(sarr)
    arr=sarr
    end subroutine

    subroutine rsf_read_i2(sf,arr)
    class(rsf_t),intent(inout):: sf
    integer,intent(out):: arr(:,:)
    integer:: i,i2
    call sf%assert_type('int')
    call rsf_data_open(sf,'r')
    if(sf%form() .eq.'native') then
        do i2=1,sf%n(2)
            read(sf%iunit,rec=i2) (arr(i,i2),i=1,sf%n(1))
        enddo
    elseif(sf%form() .eq.'ascii') then
        do i2=1,sf%n(2)
            read(sf%iunit,*) (arr(i,i2),i=1,sf%n(1))
        enddo
    else
        call errexit("Wrong form")
    endif
    close(sf%iunit)
    end subroutine

    subroutine rsf_read_f2(sf,arr)
    class(rsf_t),intent(inout):: sf
    real(kind=4),intent(out):: arr(:,:)
    integer:: i,i2
    call sf%assert_type('float')
    call rsf_data_open(sf,'r')
    if(sf%form() .eq.'native') then
        do i2=1,sf%n(2)
            read(sf%iunit,rec=i2) (arr(i,i2),i=1,sf%n(1))
        enddo
    elseif(sf%form() .eq.'ascii') then
        do i2=1,sf%n(2)
            read(sf%iunit,*) (arr(i,i2),i=1,sf%n(1))
        enddo
    else
        call errexit("Wrong form")
    endif
    close(sf%iunit)
    end subroutine
    subroutine rsf_read_d2(sf,arr)
    class(rsf_t),intent(inout):: sf
    real(kind=8),intent(out):: arr(:,:)
    real(kind=4):: sarr(size(arr,1),size(arr,2))
    call sf%read(sarr)
    arr=sarr
    end subroutine

    subroutine rsf_read_c2(sf,arr)
    class(rsf_t),intent(inout):: sf
    complex(kind=4),intent(out):: arr(:,:)
    integer:: i,i2
    call sf%assert_type('complex')
    call rsf_data_open(sf,'r')
    if(sf%form() .eq.'native') then
        do i2=1,sf%n(2)
            read(sf%iunit,rec=i2) (arr(i,i2),i=1,sf%n(1))
        enddo
    elseif(sf%form() .eq.'ascii') then
        do i2=1,sf%n(2)
            read(sf%iunit,*) (arr(i,i2),i=1,sf%n(1))
        enddo
    else
        call errexit("Wrong form")
    endif
    close(sf%iunit)
    end subroutine
    subroutine rsf_read_z2(sf,arr)
    class(rsf_t),intent(inout):: sf
    complex(kind=8),intent(out):: arr(:,:)
    complex(kind=4):: sarr(size(arr,1),size(arr,2))
    call sf%read(sarr)
    arr=sarr
    end subroutine

    subroutine rsf_read_i3(sf,arr)
    class(rsf_t),intent(inout):: sf
    integer,intent(out):: arr(:,:,:)
    integer:: i,i2,i3,irec
    call sf%assert_type('int')
    call rsf_data_open(sf,'r')
    if(sf%form() .eq.'native') then
        irec=0
        do i3=1,sf%n(3)
        do i2=1,sf%n(2)
            irec=irec+1
            read(sf%iunit,rec=irec) (arr(i,i2,i3),i=1,sf%n(1))
        enddo
        enddo
    elseif(sf%form() .eq.'ascii') then
        do i3=1,sf%n(3)
        do i2=1,sf%n(2)
            read(sf%iunit,*) (arr(i,i2,i3),i=1,sf%n(1))
        enddo
        enddo
    else
        call errexit("Wrong form")
    endif
    close(sf%iunit)
    end subroutine

    subroutine rsf_read_f3(sf,arr)
    class(rsf_t),intent(inout):: sf
    real(kind=4),intent(out):: arr(:,:,:)
    integer:: i,i2,i3,irec
    call sf%assert_type('float')
    call rsf_data_open(sf,'r')
    if(sf%form() .eq.'native') then
        irec=0
        do i3=1,sf%n(3)
        do i2=1,sf%n(2)
            irec=irec+1
            read(sf%iunit,rec=irec) (arr(i,i2,i3),i=1,sf%n(1))
        enddo
        enddo
    elseif(sf%form() .eq.'ascii') then
        do i3=1,sf%n(3)
        do i2=1,sf%n(2)
            read(sf%iunit,*) (arr(i,i2,i3),i=1,sf%n(1))
        enddo
        enddo
    else
        call errexit("Wrong form")
    endif
    close(sf%iunit)
    end subroutine
    subroutine rsf_read_d3(sf,arr)
    class(rsf_t),intent(inout):: sf
    real(kind=8),intent(out):: arr(:,:,:)
    real(kind=4):: sarr(size(arr,1),size(arr,2),size(arr,3))
    call sf%read(sarr)
    arr=sarr
    end subroutine

    subroutine rsf_read_c3(sf,arr)
    class(rsf_t),intent(inout):: sf
    complex(kind=4),intent(out):: arr(:,:,:)
    integer:: i,i2,i3,irec
    call sf%assert_type('complex')
    call rsf_data_open(sf,'r')
    if(sf%form() .eq.'native') then
        irec=0
        do i3=1,sf%n(3)
        do i2=1,sf%n(2)
            irec=irec+1
            read(sf%iunit,rec=irec) (arr(i,i2,i3),i=1,sf%n(1))
        enddo
        enddo
    elseif(sf%form() .eq.'ascii') then
        do i3=1,sf%n(3)
        do i2=1,sf%n(2)
            read(sf%iunit,*) (arr(i,i2,i3),i=1,sf%n(1))
        enddo
        enddo
    else
        call errexit("Wrong form")
    endif
    close(sf%iunit)
    end subroutine
    subroutine rsf_read_z3(sf,arr)
    class(rsf_t),intent(inout):: sf
    complex(kind=8),intent(out):: arr(:,:,:)
    complex(kind=4):: sarr(size(arr,1),size(arr,2),size(arr,3))
    call sf%read(sarr)
    arr=sarr
    end subroutine


! write data
    subroutine rsf_write_i(sf,arr)
    class(rsf_t),intent(inout):: sf
    integer,intent(in):: arr(:)
    integer i
    call sf%assert_type('int')
    call sf%write_header()
    call rsf_data_open(sf,'w')
    if(sf%form().eq.'native') then
        sf%itr=sf%itr+1
        write(sf%iunit,rec=sf%itr) (arr(i),i=1,sf%n(1))
    elseif(sf%form().eq.'ascii') then
        if(sf%n(2)>0) then
            write(sf%iunit,*) (arr(i),i=1,sf%n(1))
        else
            do i=1,sf%n(1)
                write(sf%iunit,*) arr(i)
            enddo
        endif
    else
        call errexit("Wrong form")
    endif
    end subroutine

    subroutine rsf_write_f(sf,arr)
    class(rsf_t),intent(inout):: sf
    real(kind=4),intent(in):: arr(:)
    integer i
    call sf%assert_type('float')
    call sf%write_header()
    call rsf_data_open(sf,'w')
    if(sf%form().eq.'native') then
        sf%itr=sf%itr+1
        write(sf%iunit,rec=sf%itr) (arr(i),i=1,sf%n(1))
    elseif(sf%form().eq.'ascii') then
        if(sf%n(2)>0) then
            write(sf%iunit,*) (arr(i),i=1,sf%n(1))
        else
            do i=1,sf%n(1)
                write(sf%iunit,*) arr(i)
            enddo
        endif
    else
        call errexit("Wrong form")
    endif
    end subroutine
    subroutine rsf_write_d(sf,arr)
    class(rsf_t),intent(inout):: sf
    real(kind=8),intent(in):: arr(:)
    real(kind=4):: sarr(size(arr))
    sarr=arr
    call sf%write(sarr)
    end subroutine

    subroutine rsf_write_c(sf,arr)
    class(rsf_t),intent(inout):: sf
    complex(kind=4),intent(in):: arr(:)
    integer i
    call sf%assert_type('complex')
    call sf%write_header()
    call rsf_data_open(sf,'w')
    if(sf%form().eq.'native') then
        sf%itr=sf%itr+1
        write(sf%iunit,rec=sf%itr) (arr(i),i=1,sf%n(1))
    elseif(sf%form().eq.'ascii') then
        if(sf%n(2)>0) then
            write(sf%iunit,*) (arr(i),i=1,sf%n(1))
        else
            do i=1,sf%n(1)
                write(sf%iunit,*) arr(i)
            enddo
        endif
    else
        call errexit("Wrong form")
    endif
    end subroutine
    subroutine rsf_write_z(sf,arr)
    class(rsf_t),intent(inout):: sf
    complex(kind=8),intent(in):: arr(:)
    complex(kind=4):: sarr(size(arr))
    sarr=arr
    call sf%write(sarr)
    end subroutine

    subroutine rsf_write_i2(sf,arr)
    class(rsf_t),intent(inout):: sf
    integer,intent(in):: arr(:,:)
    integer i,i2
    call sf%assert_type('int')
    call sf%write_header()
    call rsf_data_open(sf,'w')
    if(sf%form().eq.'native') then
        do i2=1,sf%n(2)
            write(sf%iunit,rec=i2) (arr(i,i2),i=1,sf%n(1))
        enddo
    elseif(sf%form().eq.'ascii') then
        do i2=1,sf%n(2)
            write(sf%iunit,*) (arr(i,i2),i=1,sf%n(1))
        enddo
    else
        call errexit("Wrong form")
    endif
    close(sf%iunit)
    end subroutine

    subroutine rsf_write_f2(sf,arr)
    class(rsf_t),intent(inout):: sf
    real(kind=4),intent(in):: arr(:,:)
    integer i,i2
    call sf%assert_type('float')
    call sf%write_header()
    call rsf_data_open(sf,'w')
    if(sf%form().eq.'native') then
        do i2=1,sf%n(2)
            write(sf%iunit,rec=i2) (arr(i,i2),i=1,sf%n(1))
        enddo
    elseif(sf%form().eq.'ascii') then
        do i2=1,sf%n(2)
            write(sf%iunit,*) (arr(i,i2),i=1,sf%n(1))
        enddo
    else
        call errexit("Wrong form")
    endif
    close(sf%iunit)
    end subroutine
    subroutine rsf_write_d2(sf,arr)
    class(rsf_t),intent(inout):: sf
    real(kind=8),intent(in):: arr(:,:)
    real(kind=4):: sarr(size(arr,1),size(arr,2))
    sarr=arr
    call sf%write(sarr)
    end subroutine

    subroutine rsf_write_c2(sf,arr)
    class(rsf_t),intent(inout):: sf
    complex(kind=4),intent(in):: arr(:,:)
    integer i,i2
    call sf%assert_type('complex')
    call sf%write_header()
    call rsf_data_open(sf,'w')
    if(sf%form().eq.'native') then
        do i2=1,sf%n(2)
            write(sf%iunit,rec=i2) (arr(i,i2),i=1,sf%n(1))
        enddo
    elseif(sf%form().eq.'ascii') then
        do i2=1,sf%n(2)
            write(sf%iunit,*) (arr(i,i2),i=1,sf%n(1))
        enddo
    else
        call errexit("Wrong form")
    endif
    close(sf%iunit)
    end subroutine
    subroutine rsf_write_z2(sf,arr)
    class(rsf_t),intent(inout):: sf
    complex(kind=8),intent(in):: arr(:,:)
    complex(kind=4):: sarr(size(arr,1),size(arr,2))
    sarr=arr
    call sf%write(sarr)
    end subroutine

    subroutine rsf_write_i3(sf,arr)
    class(rsf_t),intent(inout):: sf
    integer,intent(in):: arr(:,:,:)
    integer i,i2,i3,irec
    call sf%assert_type('int')
    call sf%write_header()
    call rsf_data_open(sf,'w')
    if(sf%form().eq.'native') then
        irec=0
        do i3=1,sf%n(3)
        do i2=1,sf%n(2)
            irec=irec+1
            write(sf%iunit,rec=irec) (arr(i,i2,i3),i=1,sf%n(1))
        enddo
        enddo
    elseif(sf%form().eq.'ascii') then
        do i3=1,sf%n(3)
        do i2=1,sf%n(2)
            write(sf%iunit,*) (arr(i,i2,i3),i=1,sf%n(1))
        enddo
        enddo
    else
        call errexit("Wrong form")
    endif
    close(sf%iunit)
    end subroutine
    subroutine rsf_write_f3(sf,arr)
    class(rsf_t),intent(inout):: sf
    real(kind=4),intent(in):: arr(:,:,:)
    integer i,i2,i3,irec
    call sf%assert_type('float')
    call sf%write_header()
    call rsf_data_open(sf,'w')
    if(sf%form().eq.'native') then
        irec=0
        do i3=1,sf%n(3)
        do i2=1,sf%n(2)
            irec=irec+1
            write(sf%iunit,rec=irec) (arr(i,i2,i3),i=1,sf%n(1))
        enddo
        enddo
    elseif(sf%form().eq.'ascii') then
        do i3=1,sf%n(3)
        do i2=1,sf%n(2)
            write(sf%iunit,*) (arr(i,i2,i3),i=1,sf%n(1))
        enddo
        enddo
    else
        call errexit("Wrong form")
    endif
    close(sf%iunit)
    end subroutine
    subroutine rsf_write_d3(sf,arr)
    class(rsf_t),intent(inout):: sf
    real(kind=8),intent(in):: arr(:,:,:)
    real(kind=4):: sarr(size(arr,1),size(arr,2),size(arr,3))
    sarr=arr
    call sf%write(sarr)
    end subroutine
    subroutine rsf_write_c3(sf,arr)
    class(rsf_t),intent(inout):: sf
    complex(kind=4),intent(in):: arr(:,:,:)
    integer i,i2,i3,irec
    call sf%assert_type('complex')
    call sf%write_header()
    call rsf_data_open(sf,'w')
    if(sf%form().eq.'native') then
        irec=0
        do i3=1,sf%n(3)
        do i2=1,sf%n(2)
            irec=irec+1
            write(sf%iunit,rec=irec) (arr(i,i2,i3),i=1,sf%n(1))
        enddo
        enddo
    elseif(sf%form().eq.'ascii') then
        do i3=1,sf%n(3)
        do i2=1,sf%n(2)
            write(sf%iunit,*) (arr(i,i2,i3),i=1,sf%n(1))
        enddo
        enddo
    else
        call errexit("Wrong form")
    endif
    close(sf%iunit)
    end subroutine
    subroutine rsf_write_z3(sf,arr)
    class(rsf_t),intent(inout):: sf
    complex(kind=8),intent(in):: arr(:,:,:)
    complex(kind=4):: sarr(size(arr,1),size(arr,2),size(arr,3))
    sarr=arr
    call sf%write(sarr)
    end subroutine

! input/output
    type(rsf_t) function rsf_input(filename) result(sf)
    character(len=*),intent(in),optional:: filename
    if(present(filename) .and. trim(filename) /= 'stdin') then
        call rsf_parse(sf,trim(filename))
        sf%filename=trim(filename)
    else
        call rsf_parse(sf)
    endif
    end function

    function prepend_pwd(filename,use_abspath) result(val)
    character(len=*),intent(in):: filename
    logical,intent(in):: use_abspath
    character(len=MXNSTR):: val,pwd
    val=trim(filename)
    if(.not.use_abspath) return
    if(filename(1:1)=='/') return ! already abspath
    call get_environment_variable('PWD',pwd)
    val=trim(pwd)//'/'//trim(filename)
    end function

    type(rsf_t) function rsf_output(filename,in,data_format,ax1,ax2,ax3,ax4,ax5,ax6,ax7,ax8,ax9,abspath) result(sf)
    character(len=*),intent(in),optional:: filename,in,data_format
    type(Axis_t),intent(in),optional:: ax1,ax2,ax3,ax4,ax5,ax6,ax7,ax8,ax9
    logical,intent(in),optional:: abspath
    logical:: use_abspath=.true.
    integer:: esize
    if(present(abspath)) use_abspath=abspath
    if(present(filename) .and. trim(filename) /= 'stdout') then
        sf%filename=trim(filename)
        if(present(in)) then
            sf%in=trim(prepend_pwd(in,use_abspath))
        else
            sf%in=trim(prepend_pwd(trim(filename)//'@',use_abspath))
        endif
    else
        if(present(in)) then
            sf%in=trim(prepend_pwd(in,use_abspath))
        else
            call errexit("You must provide 'filename' or 'in'")
        endif
    endif
    if(present(data_format)) then
        sf%data_format=data_format
    else
        sf%data_format="native_float"
    endif
    esize=4
    if(sf%type()=='complex') esize=8
    sf%esize=esize
    if(present(ax1)) sf%axes(1)=ax1
    if(present(ax2)) sf%axes(2)=ax2
    if(present(ax3)) sf%axes(3)=ax3
    if(present(ax4)) sf%axes(4)=ax4
    if(present(ax5)) sf%axes(5)=ax5
    if(present(ax6)) sf%axes(5)=ax6
    if(present(ax7)) sf%axes(5)=ax7
    if(present(ax8)) sf%axes(5)=ax8
    if(present(ax9)) sf%axes(5)=ax9
    end function

! parse rsf to read
    subroutine rsf_parse(sf,filename)
    class(rsf_t),intent(inout):: sf
    character(len=*),intent(in),optional:: filename
    integer:: iunit
    character(len=512):: text
    if(present(filename)) then
        iunit=assign_unit()
        open(iunit,file=filename)
        do
            read(iunit,'(a)',end=999) text
            call parse_text(sf,trim(text))
        enddo
999     close(iunit)
    else
        do
            read(STDIN,'(a)',end=998) text
            call parse_text(sf,trim(text))
        enddo
998     return
    endif
    end subroutine

    subroutine parse_text(sf,text)
    class(rsf_t),intent(inout):: sf
    character(len=*),intent(in):: text
    character(len=128):: key,val,par
    integer:: istart,iend,id,l
    istart=1
    iend=len_trim(text)
    do while(istart < iend)
        call extract_kv(text(istart:iend),par)
        ! parse string
        if(len_trim(par)==0) exit
        call split_kv(par,key,val)
        val=unquote(val)
        if(key.eq.'in') then
            sf%in=val
        elseif(key.eq.'data_format') then
            sf%data_format=val
        elseif(key.eq.'esize') then
            read(val,*) sf%esize
        else ! axis info
            l=len_trim(key)
            read(key(l:l),*) id
            if(key(1:1) .eq.'n') then
                read(val,*) sf%axes(id)%n
            elseif(key(1:1).eq.'o') then
                read(val,*) sf%axes(id)%o
            elseif(key(1:1).eq.'d') then
                read(val,*) sf%axes(id)%d
            elseif(key(1:5).eq.'label') then
                sf%axes(id)%label=val
            elseif(key(1:4).eq.'unit') then
                sf%axes(id)%unit=val
            endif
        endif
        istart=istart+len_trim(par)+1
    enddo
    end subroutine

! output
    subroutine rsf_write_header(sf)
    class(rsf_t),intent(inout):: sf
    integer:: iunit
    if(.not.sf%wrote_header) then
        if(sf%filename=='') then
            call rsf_print_stream(sf,STDOUT)
        else
            iunit=assign_unit()
            open(iunit,file=sf%filename)
            call rsf_print_stream(sf,iunit)
            close(iunit)
        endif
        sf%wrote_header=.true.
    endif
    end subroutine

    subroutine rsf_print_stream(sf,iunit)
    class(rsf_t),intent(in):: sf
    integer,intent(in):: iunit
    integer i
    write(iunit,"('in=',a)") quote(sf%in)
    write(iunit,"('data_format=',a)") quote(sf%data_format)
    write(iunit,"('esize=',i0)") sf%esize
    do i=1,9
        if(sf%axes(i)%n > 0) call print_axis(sf%axes(i),iunit,i)
    enddo
    end subroutine

    subroutine rsf_close(sf)
    class(rsf_t),intent(inout):: sf
    logical:: unit_opened
    inquire(unit=sf%iunit,opened=unit_opened)
    if(unit_opened) close(sf%iunit)
    end subroutine

!! utils
    function assign_unit() result(un)
    logical :: oflag
    integer :: un,i
    do i=99,10,-1
        inquire(unit=i,opened=oflag)
        if(.not.oflag) then
            un=i
            return
        endif
    enddo
    end function

    subroutine errexit(msg)
    character(len=*),intent(in) :: msg
    write(stderr,*) trim(msg)
    stop
    end subroutine

    pure function unquote(str) result(v)
    character(len=*),intent(in):: str
    character(len=:),allocatable:: v
    integer:: l
    if(str(1:1)=='"' .or. str(1:1)=="'") then
        l=len_trim(str)
        v=str(2:l-1)
    else
        v=str
    endif
    end function

    pure function quote(str) result(v)
    character(len=*),intent(in):: str
    character(len=:),allocatable:: v
    if(len_trim(str)==0) then
        v='""'
        return
    endif
    if(str(1:1)=='"'.or.str(1:1)=="'") then
        v=str
    else
        v='"'//trim(str)//'"'
    endif
    end function

! parameter key=val parse
    subroutine split_kv(par,key,val)
    character(len=*),intent(in):: par
    character(len=*),intent(out):: key,val
    character:: eq='='
    integer:: id
    id=index(par,eq)
    key=trim(adjustl(par(1:id-1)))
    val=trim(adjustl(par(id+1:)))
    end subroutine

    subroutine extract_kv(text,par)
    character(len=*),intent(in):: text
    character(len=*),intent(out):: par
    character:: eq="=",sp=' '
    integer:: id,ivalstart,ispace
    integer:: iend
    id=index(text,eq)
    if(id==0) then
        par=''
        return
    endif
    !! consume space before the value
    ivalstart=id+1
    ispace=index(text(ivalstart:),sp)
    do while(ispace == 1)
        ivalstart=ivalstart+ispace
        ispace=index(text(ivalstart:),sp)
    enddo
    if(ispace==0) then
        par=text(1:)
    else
        par=text(1:ivalstart+ispace-1)
    endif
    end subroutine

    subroutine assert_type(sf,typ)
    class(rsf_t),intent(in):: sf
    character(len=*),intent(in):: typ
    if(trim(sf%type())==trim(typ)) return
    write(STDERR,*) 'Type mismatch!'
    write(STDERR,*) '  header:'//trim(sf%type())
    write(STDERR,*) '  data:'//trim(typ)
    stop
    end subroutine

end module

