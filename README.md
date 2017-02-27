# rsfio
A Fortran RSF file input/output module independent from [Madagascar](http://www.ahay.org/wiki/Main_Page)

## Install
```sh
make
make install
```

## Compile
```
your_fortran_compiler -o main.e main.f90 -I/path/to/include -L/path/to/lib -lrsfio
```

## RSF file format

Please read the [Guide to RSF file format](http://www.ahay.org/wiki/Guide_to_RSF_file_format).

### rsfio module

#### Supported formats
- native
- ascii

#### Supported types
- int (fortran integer)
- float (fortran real, single precision)
- complex (fortran complex, single precision)

#### Unsupported features
- XDR file format
- Datapath


## rsfio_mod module

`rsfio_mod` supports upto 9-dimensional data. Two derived data types are used for input/output.

### axis_t type
`axis_t` represents an axis. It contains information of one axis.

```fortran
type axis_t
    integer:: n=0
    real:: o=0.0, d=0.0
    character(len=:),allocatable:: label,unit
end type
```

### rsf_t type
`rsf_t` is a rsf file handler.

```fortran
use rsfio_mod, only: rsf_t,axis_t,rsf_input,rsf_output
type(rsf_t):: sf
type(axis_t):: at
at=axis_t(n=100,o=0.0,d=0.001,label="Time",unit="s")
```

### Input

```fortran
use rsfio_mod, only: rsf_t,axis_t,rsf_input
type(rsf_t):: sf
type(axis_t):: ax1,ax2
real,allocatable:: v(:,:)
integer itr

!! input rsf file: sf contains header information. It does not contain the data itself. Use "read" to read data as shown below.
sf=rsf_input("file_name.rsf")

! we can access axis infomation using arrays: n, o, d, label, and unit with a corresponding dimension number
write(*,*) "Axis 1 info"
write(*,*) sf%n(1), sf%o(1), sf%d(1), sf%label(1), sf%unit(1)

! we can also get the axis information using axis_t type
ax1=sf%axes(1)
ax2=sf%axes(2)

! number of traces = (total number of elements)/n1
write(*,*) "Number of traces:", sf%ntr()

!! read data
allocate(v(sf%n(1),sf%n(2))

! We can read whole data at once
call sf%read(v)

! Or we can read the data trace by trace
do itr=1,sf%n(2)
	call sf%read(v(:,itr))
enddo
```

### Output
```fortran
use rsfio_mod, only: rsf_t,axis_t,rsf_output
type(rsf_t):: sf
type(axis_t):: ax1,ax2
integer,parameter:: n1=300, n2=100
real:: sm(n1,n2)
real:: dt=0.001,h=0.01

! set sm array
sm(:,:) = ...

! set axis information
ax1=axis_t(n=n1,o=0.0,d=dt,label="Time",unit="s")
ax2=axis_t(n=n2,o=0.0,d=h,label="Offset",unit="km")

! rsf file handler
sf=rsf_output("output_file.rsf",data_format='native_float',ax1=ax1,ax2=ax2)
! write data. If the header is not written yet, write the header file, too.
call sf.write(sm)

! no finalization of the handler is required.
```
