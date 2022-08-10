module m_npy
    implicit none

    private
    public :: save_npy

    integer(4), parameter       :: p_un      = 23
    character, parameter        :: magic_num = achar(147) ! x93
    character, parameter        :: major     = achar(2)   !major *.npy version
    character, parameter        :: minor     = achar(0)   !minor *.npy version
    character(len=*), parameter :: magic_str = "NUMPY"

    interface save_npy
        module procedure write_int64_vec, write_int64_mtx, &
                         write_dbl_vec,   write_dbl_mtx
    end interface save_npy
contains
    subroutine write_int64_mtx(filename, mtx)
        implicit none
        character(len=*), intent(in) :: filename
        integer(8), intent(in)       :: mtx(:,:)

        character(len=*), parameter  :: var_type = "<i8"
        integer(4)                   :: header_len, s_mtx(2), i, j

        s_mtx = shape(mtx)
        header_len = len(dict_str(var_type, s_mtx))
        open(unit=p_un, file=filename, form="unformatted", access="stream")
        write (p_un) magic_num, magic_str, major, minor
        write (p_un) header_len
        write (p_un) dict_str(var_type, s_mtx)
        write (p_un) mtx
        close(unit=p_un)
    end subroutine write_int64_mtx

    subroutine  write_int64_vec(filename, vec)
        implicit none
        character(len=*), intent(in) :: filename
        integer(8), intent(in)       :: vec(:)
        character(len=*), parameter  :: var_type = "<i8"
        integer(4)                   :: header_len, s_vec(1), i

        s_vec = shape(vec)
        header_len = len(dict_str(var_type, s_vec))
        open(unit=p_un, file=filename, form="unformatted", access="stream")
        write (p_un) magic_num, magic_str, major, minor
        write (p_un) header_len
        write (p_un) dict_str(var_type, s_vec)
        write (p_un) vec
        close(unit=p_un)
    end subroutine write_int64_vec

    subroutine  write_dbl_mtx(filename, mtx)
        implicit none
        character(len=*), intent(in) :: filename
        real(8), intent(in)          :: mtx(:,:)
        character(len=*), parameter  :: var_type = "<f8"
        integer(4)                   :: header_len, s_mtx(2), i, j

        s_mtx = shape(mtx)
        header_len = len(dict_str(var_type, s_mtx))
        open(unit=p_un, file=filename, form="unformatted", access="stream")
        write (p_un) magic_num, magic_str, major, minor
        write (p_un) header_len
        write (p_un) dict_str(var_type, s_mtx)
        write (p_un) mtx
        close(unit=p_un)
    end subroutine write_dbl_mtx

    subroutine  write_dbl_vec(filename, vec)
        implicit none
        character(len=*), intent(in) :: filename
        real(8), intent(in)          :: vec(:)
        character(len=*), parameter  :: var_type = "<f8"
        integer(4)                   :: header_len, s_vec(1), i
        s_vec = shape(vec)
        header_len = len(dict_str(var_type, s_vec))
        open(unit=p_un, file=filename, form="unformatted", access="stream")
        write (p_un) magic_num, magic_str, major, minor
        write (p_un) header_len
        write (p_un) dict_str(var_type, s_vec)
        write (p_un) vec
        close(unit=p_un)
    end subroutine write_dbl_vec

    function dict_str(var_type, var_shape) result(str)
        implicit none
        character(len=*), intent(in)  :: var_type
        integer(4), intent(in)        :: var_shape(:)
        character(len=:), allocatable :: str
        integer(4)                    :: cnt

        cnt =  len("{'descr': '")
        cnt =  cnt + len(var_type)
        cnt =  cnt + len("', 'fortran_order': True, 'shape': (")
        cnt =  cnt + len(shape_str(var_shape))
        cnt =  cnt + len(",), }")
        do while(mod(cnt + 10, 16) /= 0)
            cnt =  cnt + 1
        enddo

        allocate(character(cnt) :: str)

        str = "{'descr': '" // var_type // &
              "', 'fortran_order': True, 'shape': (" // &
              shape_str(var_shape) //  "), }"

        do while(mod(len(str) + 11, 16) /= 0)
            str = str // " "
        enddo

        str = str // achar(10)

    end function dict_str

    function shape_str(var_shape) result(fin_str)
        implicit none
        integer(4), intent(in)        :: var_shape(:)
        character(len=:), allocatable :: str, small_str, fin_str
        integer(4)                    :: i, length, start, halt

        length = 14 * size(var_shape)
        allocate(character(length) :: str)
        allocate(character(14)     :: small_str)
        str =  " "

        do i = 1, size(var_shape)
            start = (i-1) * length + 1
            halt  = i     * length + 1
            write (small_str, "(I13,A)") var_shape(i), ","
            str = trim(str) // adjustl(small_str)
        enddo

        fin_str =  trim(str)
    end function shape_str
end module m_npy
