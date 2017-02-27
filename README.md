# rsfio
A Fortran RSF file input/output module independent from [Madagascar](http://www.ahay.org/wiki/Main_Page)


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