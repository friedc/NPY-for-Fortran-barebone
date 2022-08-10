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
    subroutine write_int64_mtx(filename, int64_mtx)
        implicit none
        character(len=*), intent(in)  :: filename
        integer(8), intent(in)        :: int64_mtx(:,:)
        character(len=:), allocatable :: header
        integer(4)                    :: header_len
        header = dict_str("<i8", shape(int64_mtx))
        header_len = len(header)
        open(u, file=filename, form="unformatted", access="stream")
        write (u) magic_str, major_minor, header_len, header, int64_mtx
        close(u)
    end subroutine write_int64_mtx
    subroutine  write_int64_vec(filename, int64_vec)
        implicit none
        character(len=*), intent(in)  :: filename
        integer(8), intent(in)        :: int64_vec(:)
        character(len=:), allocatable :: header
        integer(4)                    :: header_len
        header = dict_str("<i8", shape(int64_vec))
        header_len = len(header)
        open(u, file=filename, form="unformatted", access="stream")
        write (u) magic_str, major_minor, header_len, header, int64_vec
        close(u)
    end subroutine write_int64_vec
    subroutine  write_dbl_mtx(filename, dbl_mtx)
        implicit none
        character(len=*), intent(in)  :: filename
        real(8), intent(in)           :: dbl_mtx(:,:)
        character(len=:), allocatable :: header
        integer(4)                    :: header_len
        header = dict_str("<f8", shape(dbl_mtx))
        header_len = len(header)
        open(u, file=filename, form="unformatted", access="stream")
        write (u) magic_str, major_minor, header_len, header, dbl_mtx
        close(u)
    end subroutine write_dbl_mtx
    subroutine  write_dbl_vec(filename, dbl_vec)
        implicit none
        character(len=*), intent(in)  :: filename
        real(8), intent(in)           :: dbl_vec(:)
        character(len=:), allocatable :: header
        integer(4)                    :: header_len
        header = dict_str("<f8", shape(dbl_vec))
        header_len = len(header)
        open(u, file=filename, form="unformatted", access="stream")
        write (u) magic_str, major_minor, header_len, header, dbl_vec
        close(u)
    end subroutine write_dbl_vec
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
