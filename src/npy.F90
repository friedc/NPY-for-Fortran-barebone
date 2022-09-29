module m_npy
    implicit none
    private
    public :: save_npy
    interface save_npy
        module procedure write_int64_vec, write_int64_mtx, write_dbl_vec, write_dbl_mtx
    end interface save_npy
contains
#ifndef NPY_UNIT
#define NPY_UNIT 100
#endif
#define GENSUB(SNAME,PTYPE,PNAME,PSHAPE,PSTR) \
    subroutine SNAME(filename, PNAME);\
        character(len=*), intent(in) :: filename;\
        PTYPE, intent(in) :: PNAME PSHAPE;\
        character(len=:), allocatable :: header;\
        integer(4) :: header_len;\
        header = dict_str(PSTR, shape(PNAME));\
        header_len = len(header);\
        open(NPY_UNIT, file=filename, form="unformatted", access="stream");\
        write (NPY_UNIT) achar(int(Z'93')) // "NUMPY", achar(2) // achar(0), header_len, header, PNAME ;\
        close(NPY_UNIT);\
    end subroutine SNAME
    GENSUB(write_int64_mtx,integer(8),int64_mtx,(:,:),"<i8")
    GENSUB(write_int64_vec,integer(8),int64_vec,(:),"<i8")
    GENSUB(write_dbl_mtx,real(8),dbl_mtx,(:,:),"<f8")
    GENSUB(write_dbl_vec,real(8),dbl_vec,(:),"<f8")
#undef GENSUB
    function dict_str(var_type, var_shape) result(str)
        character(len=*), intent(in)  :: var_type
        integer, intent(in)           :: var_shape(:)
        character(len=:), allocatable :: str
        character(len=13)             :: small_str
        integer                       :: i
        str = "{'descr':'" // var_type // "','fortran_order':True,'shape':("
        do i = 1, size(var_shape)
            write (small_str, "(I13)") var_shape(i)
            str = str // trim(adjustl(small_str))
            if (size(var_shape).eq.1 .or. i.lt.size(var_shape)) str = str // ","
        enddo
        str = str // ")}"  // repeat(" ", mod(16-mod(6+1+1+4+len(str)+2+1, 16), 16)) // achar(10)
    end function dict_str
end module m_npy
