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

## Quick reference
```fortran
use rsfio_mod, only: rsf_t, axis_t, rsf_input, rsf_output
type(rsf_t):: sf, so
type(axis_t):: at, ax
real,allocatable:: arr(:,:)
integer:: n1,n2, itr

! open: read
sf = rsf_input("input.rsf")

! access axis information
at = sf%axes(1)
ax = sf%axes(2)
n1 = sf%n(1)
n2 = sf%n(2)

! access (read/write) header infomation
print*, sf%in, sf%esize, sf%data_format

! read data
allocate(arr(n1,n2))
call sf%read(arr)

! read data trace by trace
do itr=1,sf%ntr()
    call sf%read(arr(:,itr))
enddo

! open: write
so = rsf_output("output.rsf",in="output.rsf@", &
                 data_format="native_float", &
                 ax1=at,ax2=ax,abspath=.true.)

! write data
call so%write(arr)

! write data trace by trace
do itr=1,so%ntr()
    call so%write(arr(:,itr))
enddo
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
- Datapath: The module writes data and its header in a same directory unless we specify the data path.
- Header + data in one file


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
`rsf_t` is a rsf file handle.

```fortran
use rsfio_mod, only: rsf_t,axis_t,rsf_input,rsf_output
type(rsf_t):: sf
type(axis_t):: at
at=axis_t(n=100,o=0.0,d=0.001,label="Time",unit="s")
```

### API: Input

We can parse rsf file header as

```fortran
type(rsf_t):: sf
sf=rsf_input(filename)
```

- `filename` is optional. If we omit `filename`, the module reads the header from standard input.

`sf` contains header information only and we use `sf%read(array)` to read data.

```fortran
call sf%read(array)
```

- RSF data is single precision; however, we can use double precision array to read/write data. There is an internal conversion process between single and double precision. The array can be

    - integer:: array(n1), array(n1,n2), array(n1,n2,n3)
    - real(kind=4):: array(n1), array(n1,n2), array(n1,n2,n3)
    - real(kind=8):: array(n1), array(n1,n2), array(n1,n2,n3)
    - complex(kind=4):: array(n1), array(n1,n2), array(n1,n2,n3)
    - complex(kind=8):: array(n1), array(n1,n2), array(n1,n2,n3)

- If we use 1d array for n-dimensional data, we are accessing the data trace by trace.

There are two methods to read the axis information.

```fortran
!! method 1: use axis_t type
! sf%axes(1:9): axis_t array contains axis information
! the array index indicates the axis number
n2=sf%axes(2)%n
o3=sf%axes(3)%o
! We can use this method to write the axis information
sf%axes(3)%o=o3

!! method 2: for convenience, only for read
d2=sf%d(2) 
label1=sf%label(1)
unit3=sf%unit(3)
```

Here is an example.


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

! We can read/write whole data at once for 1d, 2d, and 3d data for convenience
! Although rsf data is single precision, we can use double precision real/complex array to read/write data.
! The module takes care of the conversion
call sf%read(v)

! Or we can read the data trace by trace
do itr=1,sf%n(2)
	call sf%read(v(:,itr))
enddo
```

### API: Output

We can generate a rsf file handle as

```fortran
type(rsf_t):: sf
sf=rsf_output(filename,in,data_format,ax1,ax2,ax3,ax4,ax5,ax6,ax7,ax8,ax9,abspath)
```

All arguments are optional.

- If we omit `filename`, the module writes the header to standard output. The module writes the header at the first time we call `sf%write` to write data.
- `in` specifies the data path. If we omit `in`, the output data name will be "filename"//"@". The function requires at least one of "filename" and "in".
- `data_format` is the rsf data format. Supported data formats are
    - native_int
    - native_float
    - native_complex
    - ascii_int
    - ascii_float
    - ascii_complex
- `ax1` to `ax9` are axes information: type(axis_t) explained above. We can change the axes information after calling `rsf_output` and before calling `rsf%write`.
- `abspath` is a logical value (default=.true.). The module writes `in="/absolute/data/path"` in the header unless `abspath` is false. If it is false, the module uses relative data path.

We use `call sf%write` to write header and data.

```fortran
call sf%write(array)
```

Supported array types are same as those of `sf%read`. Here is an example.


```fortran
use rsfio_mod, only: rsf_t,axis_t,rsf_output
type(rsf_t):: sf
type(axis_t):: ax1,ax2
integer,parameter:: n1=300, n2=100
real:: sm(n1,n2)
real:: dt=0.001,h=0.01
itneger:: itr

! set sm array
sm(:,:) = ...

! set axis information
ax1=axis_t(n=n1,o=0.0,d=dt,label="Time",unit="s")
ax2=axis_t(n=n2,o=0.0,d=h,label="Offset",unit="km")

! rsf file handle
sf=rsf_output("output_file.rsf",data_format='native_float',ax1=ax1,ax2=ax2)

! write data. If the header is not written yet, write the header file, too.
call sf%write(sm)

! Or we can write the data trace by trace, too.
do itr=1,sf%n(2)
    call sf%write(sm(:,itr))
enddo

! no finalization of the handle is required.
```
