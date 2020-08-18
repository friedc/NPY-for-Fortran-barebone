# NPY for Fortran
This Fortran module allows to save numerical Fortran arrays in Numpy's .npy format. Currently supported are:
```fortran
1. integer(8)
2. real(8)
```
### *.npy files
Saving an array into a .npy-file is simply done by calling:
```fortran
call save_npy("filename.npy", array)
```
