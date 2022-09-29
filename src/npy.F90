module m_npy
    implicit none
    private
    public :: save_npy
#ifdef __UNIT__
    integer, parameter :: u = __UNIT__
#else
    integer, parameter :: u = 100
#endif
    character(len=6), parameter :: magic_str = achar(int(Z'93')) // "NUMPY"
    character(len=2), parameter :: major_minor = achar(2) // achar(0)
    interface save_npy
        module procedure write_int64_vec, write_int64_mtx, &
                         write_dbl_vec,   write_dbl_mtx
    end interface save_npy
contains
#define GENSUB(SNAME,PTYPE,PNAME,PSHAPE,PSTR) \
    subroutine SNAME(filename, PNAME);\
        implicit none;\
        character(len=*), intent(in) :: filename;\
        PTYPE, intent(in) :: PNAME PSHAPE;\
        character(len=:), allocatable :: header;\
        integer(4) :: header_len;\
        header = dict_str(PSTR, shape(PNAME));\
        header_len = len(header);\
        open(u, file=filename, form="unformatted", access="stream");\
        write (u) magic_str, major_minor, header_len, header, PNAME ;\
        close(u);\
    end subroutine SNAME
    GENSUB(write_int64_mtx,integer(8),int64_mtx,(:,:),"<i8")
    GENSUB(write_int64_vec,integer(8),int64_vec,(:),"<i8")
    GENSUB(write_dbl_mtx,real(8),dbl_mtx,(:,:),"<f8")
    GENSUB(write_dbl_vec,real(8),dbl_vec,(:),"<f8")
#undef GENSUB
    function dict_str(var_type, var_shape) result(str)
        implicit none
        character(len=*), intent(in)  :: var_type
        integer, intent(in)           :: var_shape(:)
        character(len=:), allocatable :: str
        integer                       :: var_shape_dims, num_spaces, i
        character(len=13)             :: small_str
        character(len=:), allocatable :: shape_str
        str = "{'descr':'" // var_type // "','fortran_order':True,"
        var_shape_dims = size(var_shape)
        shape_str = " "
        do i = 1, var_shape_dims
            write (small_str, "(I13)") var_shape(i)
            shape_str = shape_str // trim(adjustl(small_str))
            if (var_shape_dims.eq.1 .or. i.lt.var_shape_dims) then
              shape_str = shape_str // ","
            endif
        enddo
        shape_str = trim(adjustl(shape_str))
        str = str // "'shape':(" // shape_str // ")}"
        num_spaces = mod(16-mod(6+1+1+4+len(str)+1,16),16)
        do i = 1, num_spaces
            str = str // " "
        enddo
        str = str // achar(10)
    end function dict_str
end module m_npy
