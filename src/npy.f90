module  m_npy  
    implicit none

    integer(4), parameter               :: p_un      = 23
    character, parameter                :: magic_num = achar(147) ! x93
    character, parameter                :: major     = achar(2)   !major *.npy version 
    character, parameter                :: minor     = achar(0)   !minor *.npy version
    
    character(len=*), parameter         :: magic_str =  "NUMPY"

    interface save_npy
        module procedure write_int64_vec,     write_int64_mtx, &
                         write_int32_vec,     write_int32_mtx, &
                         write_int16_vec,     write_int16_mtx, &
                         write_int8_vec,      write_int8_mtx, &
                         write_dbl_vec,       write_dbl_mtx,   &
                         write_sng_vec,       write_sng_mtx,   &
                         write_cmplx_sgn_vec, write_cmplx_sgn_mtx, &
                         write_cmplx_dbl_vec, write_cmplx_dbl_mtx

    end interface save_npy
    interface add_npz
        module procedure addrpl_int8_vec,      addrpl_int8_mtx,      &
                         addrpl_int16_vec,     addrpl_int16_mtx,     &
                         addrpl_int32_vec,     addrpl_int32_mtx,     &
                         addrpl_int64_vec,     addrpl_int64_mtx,     &
                         addrpl_sng_vec,       addrpl_sng_mtx,       &
                         addrpl_dbl_vec,       addrpl_dbl_mtx,       &
                         addrpl_cmplx_dbl_vec, addrpl_cmplx_dbl_mtx, &
                         addrpl_cmplx_sng_vec, addrpl_cmplx_sng_mtx
    end interface add_npz

contains
    subroutine addrpl_cmplx_sng_vec(zipfile, var_name, vec)
        implicit none
        complex(4), intent(in)           :: vec(:)
        character(len=*), intent(in)     :: zipfile, var_name
        character(len=:), allocatable    :: npy_name
        integer                          :: succ

        npy_name =  var_name // ".npy"

        call save_npy(npy_name, vec)
        ! just store and be quite while zipping
        call execute_command_line("zip -0q " // zipfile &
                                 // " " // npy_name, cmdstat=succ)
        if(succ /=  0) then
            write (*,*) "Can't execute zip command"
        endif

        call execute_command_line("rm " // npy_name, cmdstat=succ)
        if(succ /=  0) then
            write (*,*) "Can't execute rm command"
        endif
    end subroutine addrpl_cmplx_sng_vec
    
    subroutine addrpl_cmplx_sng_mtx(zipfile, var_name, mtx)
        implicit none
        complex(4), intent(in)           :: mtx(:,:)
        character(len=*), intent(in)     :: zipfile, var_name
        character(len=:), allocatable    :: npy_name
        integer                          :: succ

        npy_name =  var_name // ".npy"

        call save_npy(npy_name, mtx)
        ! just store and be quite while zipping
        call execute_command_line("zip -0q " // zipfile &
                                 // " " // npy_name, cmdstat=succ)
        if(succ /=  0) then
            write (*,*) "Can't execute zip command"
        endif

        call execute_command_line("rm " // npy_name, cmdstat=succ)
        if(succ /=  0) then
            write (*,*) "Can't execute rm command"
        endif
    end subroutine addrpl_cmplx_sng_mtx
    
    subroutine addrpl_cmplx_dbl_vec(zipfile, var_name, vec)
        implicit none
        complex(8), intent(in)           :: vec(:)
        character(len=*), intent(in)     :: zipfile, var_name
        character(len=:), allocatable    :: npy_name
        integer                          :: succ

        npy_name =  var_name // ".npy"

        call save_npy(npy_name, vec)
        ! just store and be quite while zipping
        call execute_command_line("zip -0q " // zipfile &
                                 // " " // npy_name, cmdstat=succ)
        if(succ /=  0) then
            write (*,*) "Can't execute zip command"
        endif

        call execute_command_line("rm " // npy_name, cmdstat=succ)
        if(succ /=  0) then
            write (*,*) "Can't execute rm command"
        endif
    end subroutine addrpl_cmplx_dbl_vec
    
    subroutine addrpl_cmplx_dbl_mtx(zipfile, var_name, mtx)
        implicit none
        complex(8), intent(in)           :: mtx(:,:)
        character(len=*), intent(in)     :: zipfile, var_name
        character(len=:), allocatable    :: npy_name
        integer                          :: succ

        npy_name =  var_name // ".npy"

        call save_npy(npy_name, mtx)
        ! just store and be quite while zipping
        call execute_command_line("zip -0q " // zipfile &
                                 // " " // npy_name, cmdstat=succ)
        if(succ /=  0) then
            write (*,*) "Can't execute zip command"
        endif

        call execute_command_line("rm " // npy_name, cmdstat=succ)
        if(succ /=  0) then
            write (*,*) "Can't execute rm command"
        endif
    end subroutine addrpl_cmplx_dbl_mtx
    
    subroutine addrpl_dbl_vec(zipfile, var_name, vec)
        implicit none
        real(8), intent(in)           :: vec(:)
        character(len=*), intent(in)     :: zipfile, var_name
        character(len=:), allocatable    :: npy_name
        integer                          :: succ

        npy_name =  var_name // ".npy"

        call save_npy(npy_name, vec)
        ! just store and be quite while zipping
        call execute_command_line("zip -0q " // zipfile &
                                 // " " // npy_name, cmdstat=succ)
        if(succ /=  0) then
            write (*,*) "Can't execute zip command"
        endif

        call execute_command_line("rm " // npy_name, cmdstat=succ)
        if(succ /=  0) then
            write (*,*) "Can't execute rm command"
        endif
    end subroutine addrpl_dbl_vec
    
    subroutine addrpl_dbl_mtx(zipfile, var_name, mtx)
        implicit none
        real(8), intent(in)           :: mtx(:,:)
        character(len=*), intent(in)     :: zipfile, var_name
        character(len=:), allocatable    :: npy_name
        integer                          :: succ

        npy_name =  var_name // ".npy"

        call save_npy(npy_name, mtx)
        ! just store and be quite while zipping
        call execute_command_line("zip -0q " // zipfile &
                                 // " " // npy_name, cmdstat=succ)
        if(succ /=  0) then
            write (*,*) "Can't execute zip command"
        endif

        call execute_command_line("rm " // npy_name, cmdstat=succ)
        if(succ /=  0) then
            write (*,*) "Can't execute rm command"
        endif
    end subroutine addrpl_dbl_mtx
    
    subroutine addrpl_sng_vec(zipfile, var_name, vec)
        implicit none
        real(4), intent(in)           :: vec(:)
        character(len=*), intent(in)     :: zipfile, var_name
        character(len=:), allocatable    :: npy_name
        integer                          :: succ

        npy_name =  var_name // ".npy"

        call save_npy(npy_name, vec)
        ! just store and be quite while zipping
        call execute_command_line("zip -0q " // zipfile &
                                 // " " // npy_name, cmdstat=succ)
        if(succ /=  0) then
            write (*,*) "Can't execute zip command"
        endif

        call execute_command_line("rm " // npy_name, cmdstat=succ)
        if(succ /=  0) then
            write (*,*) "Can't execute rm command"
        endif
    end subroutine addrpl_sng_vec
    
    subroutine addrpl_sng_mtx(zipfile, var_name, mtx)
        implicit none
        real(4), intent(in)           :: mtx(:,:)
        character(len=*), intent(in)     :: zipfile, var_name
        character(len=:), allocatable    :: npy_name
        integer                          :: succ

        npy_name =  var_name // ".npy"

        call save_npy(npy_name, mtx)
        ! just store and be quite while zipping
        call execute_command_line("zip -0q " // zipfile &
                                 // " " // npy_name, cmdstat=succ)
        if(succ /=  0) then
            write (*,*) "Can't execute zip command"
        endif

        call execute_command_line("rm " // npy_name, cmdstat=succ)
        if(succ /=  0) then
            write (*,*) "Can't execute rm command"
        endif
    end subroutine addrpl_sng_mtx
    
    subroutine addrpl_int8_vec(zipfile, var_name, vec)
        implicit none
        integer(1), intent(in)           :: vec(:)
        character(len=*), intent(in)     :: zipfile, var_name
        character(len=:), allocatable    :: npy_name
        integer                          :: succ

        npy_name =  var_name // ".npy"

        call save_npy(npy_name, vec)
        ! just store and be quite while zipping
        call execute_command_line("zip -0q " // zipfile &
                                 // " " // npy_name, cmdstat=succ)
        if(succ /=  0) then
            write (*,*) "Can't execute zip command"
        endif

        call execute_command_line("rm " // npy_name, cmdstat=succ)
        if(succ /=  0) then
            write (*,*) "Can't execute rm command"
        endif
    end subroutine addrpl_int8_vec
    
    subroutine addrpl_int8_mtx(zipfile, var_name, mtx)
        implicit none
        integer(1), intent(in)           :: mtx(:,:)
        character(len=*), intent(in)     :: zipfile, var_name
        character(len=:), allocatable    :: npy_name
        integer                          :: succ

        npy_name =  var_name // ".npy"

        call save_npy(npy_name, mtx)
        ! just store and be quite while zipping
        call execute_command_line("zip -0q " // zipfile &
                                 // " " // npy_name, cmdstat=succ)
        if(succ /=  0) then
            write (*,*) "Can't execute zip command"
        endif

        call execute_command_line("rm " // npy_name, cmdstat=succ)
        if(succ /=  0) then
            write (*,*) "Can't execute rm command"
        endif
    end subroutine addrpl_int8_mtx
    
    subroutine addrpl_int16_vec(zipfile, var_name, vec)
        implicit none
        integer(2), intent(in)           :: vec(:)
        character(len=*), intent(in)     :: zipfile, var_name
        character(len=:), allocatable    :: npy_name
        integer                          :: succ

        npy_name =  var_name // ".npy"

        call save_npy(npy_name, vec)
        ! just store and be quite while zipping
        call execute_command_line("zip -0q " // zipfile &
                                 // " " // npy_name, cmdstat=succ)
        if(succ /=  0) then
            write (*,*) "Can't execute zip command"
        endif

        call execute_command_line("rm " // npy_name, cmdstat=succ)
        if(succ /=  0) then
            write (*,*) "Can't execute rm command"
        endif
    end subroutine addrpl_int16_vec
    
    subroutine addrpl_int16_mtx(zipfile, var_name, mtx)
        implicit none
        integer(2), intent(in)           :: mtx(:,:)
        character(len=*), intent(in)     :: zipfile, var_name
        character(len=:), allocatable    :: npy_name
        integer                          :: succ

        npy_name =  var_name // ".npy"

        call save_npy(npy_name, mtx)
        ! just store and be quite while zipping
        call execute_command_line("zip -0q " // zipfile &
                                 // " " // npy_name, cmdstat=succ)
        if(succ /=  0) then
            write (*,*) "Can't execute zip command"
        endif

        call execute_command_line("rm " // npy_name, cmdstat=succ)
        if(succ /=  0) then
            write (*,*) "Can't execute rm command"
        endif
    end subroutine addrpl_int16_mtx
    
    subroutine addrpl_int32_vec(zipfile, var_name, vec)
        implicit none
        integer(4), intent(in)           :: vec(:)
        character(len=*), intent(in)     :: zipfile, var_name
        character(len=:), allocatable    :: npy_name
        integer                          :: succ

        npy_name =  var_name // ".npy"

        call save_npy(npy_name, vec)
        ! just store and be quite while zipping
        call execute_command_line("zip -0q " // zipfile &
                                 // " " // npy_name, cmdstat=succ)
        if(succ /=  0) then
            write (*,*) "Can't execute zip command"
        endif

        call execute_command_line("rm " // npy_name, cmdstat=succ)
        if(succ /=  0) then
            write (*,*) "Can't execute rm command"
        endif
    end subroutine addrpl_int32_vec
    
    subroutine addrpl_int32_mtx(zipfile, var_name, mtx)
        implicit none
        integer(4), intent(in)           :: mtx(:,:)
        character(len=*), intent(in)     :: zipfile, var_name
        character(len=:), allocatable    :: npy_name
        integer                          :: succ

        npy_name =  var_name // ".npy"

        call save_npy(npy_name, mtx)
        ! just store and be quite while zipping
        call execute_command_line("zip -0q " // zipfile &
                                 // " " // npy_name, cmdstat=succ)
        if(succ /=  0) then
            write (*,*) "Can't execute zip command"
        endif

        call execute_command_line("rm " // npy_name, cmdstat=succ)
        if(succ /=  0) then
            write (*,*) "Can't execute rm command"
        endif
    end subroutine addrpl_int32_mtx
    
    subroutine addrpl_int64_vec(zipfile, var_name, vec)
        implicit none
        integer(8), intent(in)           :: vec(:)
        character(len=*), intent(in)     :: zipfile, var_name
        character(len=:), allocatable    :: npy_name
        integer                          :: succ

        npy_name =  var_name // ".npy"

        call save_npy(npy_name, vec)
        ! just store and be quite while zipping
        call execute_command_line("zip -0q " // zipfile &
                                 // " " // npy_name, cmdstat=succ)
        if(succ /=  0) then
            write (*,*) "Can't execute zip command"
        endif

        call execute_command_line("rm " // npy_name, cmdstat=succ)
        if(succ /=  0) then
            write (*,*) "Can't execute rm command"
        endif
    end subroutine addrpl_int64_vec
    
    subroutine addrpl_int64_mtx(zipfile, var_name, mtx)
        implicit none
        integer(8), intent(in)           :: mtx(:,:)
        character(len=*), intent(in)     :: zipfile, var_name
        character(len=:), allocatable    :: npy_name
        integer                          :: succ

        npy_name =  var_name // ".npy"

        call save_npy(npy_name, mtx)
        ! just store and be quite while zipping
        call execute_command_line("zip -0q " // zipfile &
                                 // " " // npy_name, cmdstat=succ)
        if(succ /=  0) then
            write (*,*) "Can't execute zip command"
        endif

        call execute_command_line("rm " // npy_name, cmdstat=succ)
        if(succ /=  0) then
            write (*,*) "Can't execute rm command"
        endif
    end subroutine addrpl_int64_mtx

    Subroutine write_cmplx_sgn_mtx(filename, mtx)
        Implicit None
        character(len=*), intent(in)     :: filename
        complex(4), intent(in)           :: mtx(:,:)
        character(len=*), parameter      :: var_type =  "<c8"
        integer(4)                       :: header_len, s_mtx(2)

        s_mtx = shape(mtx)
        header_len =  len(dict_str(var_type, s_mtx))
        
        open(unit=p_un, file=filename, form="unformatted",access="stream")
        write (p_un) magic_num, magic_str, major, minor
        write (p_un) header_len
        write (p_un) dict_str(var_type, s_mtx)
        write (p_un) mtx
        close(unit=p_un)
    End Subroutine write_cmplx_sgn_mtx
    
    Subroutine  write_cmplx_sgn_vec(filename, vec)
        Implicit None
        character(len=*), intent(in)     :: filename
        complex(4), intent(in)           :: vec(:)
        character(len=*), parameter      :: var_type =  "<c8"
        integer(4)                       :: header_len, s_vec(1)

        s_vec = shape(vec)
        header_len =  len(dict_str(var_type, s_vec))
        
        open(unit=p_un, file=filename, form="unformatted",access="stream")
        write (p_un) magic_num, magic_str, major, minor
        write (p_un) header_len
        write (p_un) dict_str(var_type, s_vec)
        write (p_un) vec
        close(unit=p_un)
    End Subroutine write_cmplx_sgn_vec

    Subroutine  write_cmplx_dbl_mtx(filename, mtx)
        Implicit None
        character(len=*), intent(in)     :: filename
        complex(8), intent(in)           :: mtx(:,:)
        character(len=*), parameter      :: var_type =  "<c16"
        integer(4)                       :: header_len, s_mtx(2)

        s_mtx = shape(mtx)
        header_len =  len(dict_str(var_type, s_mtx))
        
        open(unit=p_un, file=filename, form="unformatted",access="stream")
        write (p_un) magic_num, magic_str, major, minor
        write (p_un) header_len
        write (p_un) dict_str(var_type, s_mtx)
        write (p_un) mtx
        close(unit=p_un)
    End Subroutine write_cmplx_dbl_mtx
    
    Subroutine  write_cmplx_dbl_vec(filename, vec)
        Implicit None
        character(len=*), intent(in)     :: filename
        complex(8), intent(in)           :: vec(:)
        character(len=*), parameter      :: var_type =  "<c16"
        integer(4)                       :: header_len, s_vec(1)

        s_vec = shape(vec)
        header_len =  len(dict_str(var_type, s_vec))
        
        open(unit=p_un, file=filename, form="unformatted",access="stream")
        write (p_un) magic_num, magic_str, major, minor
        write (p_un) header_len
        write (p_un) dict_str(var_type, s_vec)
        write (p_un) vec
        close(unit=p_un)
    End Subroutine write_cmplx_dbl_vec

    Subroutine  write_sng_mtx(filename, mtx)
        Implicit None
        character(len=*), intent(in)     :: filename
        real(4), intent(in)              :: mtx(:,:)
        character(len=*), parameter      :: var_type =  "<f4"
        integer(4)                       :: header_len, s_mtx(2)

        s_mtx = shape(mtx)
        header_len =  len(dict_str(var_type, s_mtx))
        
        open(unit=p_un, file=filename, form="unformatted",access="stream")
        write (p_un) magic_num, magic_str, major, minor
        write (p_un) header_len
        write (p_un) dict_str(var_type, s_mtx)
        write (p_un) mtx
        close(unit=p_un)
    End Subroutine write_sng_mtx
    
    Subroutine  write_sng_vec(filename, vec)
        Implicit None
        character(len=*), intent(in)     :: filename
        real(4), intent(in)              :: vec(:)
        character(len=*), parameter      :: var_type =  "<f4"
        integer(4)                       :: header_len, s_vec(1)

        s_vec = shape(vec)
        header_len =  len(dict_str(var_type, s_vec))
        
        open(unit=p_un, file=filename, form="unformatted",access="stream")
        write (p_un) magic_num, magic_str, major, minor
        write (p_un) header_len
        write (p_un) dict_str(var_type, s_vec)
        write (p_un) vec
        close(unit=p_un)
    End Subroutine write_sng_vec
    
    Subroutine  write_dbl_mtx(filename, mtx)
        Implicit None
        character(len=*), intent(in)     :: filename
        real(8), intent(in)              :: mtx(:,:)
        character(len=*), parameter      :: var_type =  "<f8"
        integer(4)                       :: header_len, s_mtx(2)

        s_mtx = shape(mtx)
        header_len =  len(dict_str(var_type, s_mtx))
        
        open(unit=p_un, file=filename, form="unformatted",access="stream")
        write (p_un) magic_num, magic_str, major, minor
        write (p_un) header_len
        write (p_un) dict_str(var_type, s_mtx)
        write (p_un) mtx
        close(unit=p_un)
    End Subroutine write_dbl_mtx
    
    Subroutine  write_dbl_vec(filename, vec)
        Implicit None
        character(len=*), intent(in)     :: filename
        real(8), intent(in)              :: vec(:)
        character(len=*), parameter      :: var_type =  "<f8"
        integer(4)                       :: header_len, s_vec(1)

        s_vec = shape(vec)
        header_len =  len(dict_str(var_type, s_vec))
        
        open(unit=p_un, file=filename, form="unformatted",access="stream")
        write (p_un) magic_num, magic_str, major, minor
        write (p_un) header_len
        write (p_un) dict_str(var_type, s_vec)
        write (p_un) vec
        close(unit=p_un)
    End Subroutine write_dbl_vec
    
    Subroutine  write_int64_mtx(filename, mtx)
        Implicit None
        character(len=*), intent(in)     :: filename
        integer(8), intent(in)           :: mtx(:,:)
        character(len=*), parameter      :: var_type =  "<i8"
        integer(4)                       :: header_len, s_mtx(2)

        s_mtx = shape(mtx)
        header_len =  len(dict_str(var_type, s_mtx))
        
        open(unit=p_un, file=filename, form="unformatted",access="stream")
        write (p_un) magic_num, magic_str, major, minor
        write (p_un) header_len
        write (p_un) dict_str(var_type, s_mtx)
        write (p_un) mtx
        close(unit=p_un)
    End Subroutine write_int64_mtx
    
    Subroutine  write_int64_vec(filename, vec)
        Implicit None
        character(len=*), intent(in)     :: filename
        integer(8), intent(in)           :: vec(:)
        character(len=*), parameter      :: var_type =  "<i8"
        integer(4)                       :: header_len, s_vec(1)

        s_vec = shape(vec)
        header_len =  len(dict_str(var_type, s_vec))
        
        open(unit=p_un, file=filename, form="unformatted",access="stream")
        write (p_un) magic_num, magic_str, major, minor
        write (p_un) header_len
        write (p_un) dict_str(var_type, s_vec)
        write (p_un) vec
        close(unit=p_un)
    End Subroutine write_int64_vec


    Subroutine  write_int32_mtx(filename, mtx)
        Implicit None
        character(len=*), intent(in)     :: filename
        integer(4), intent(in)           :: mtx(:,:)
        character(len=*), parameter      :: var_type =  "<i4"
        integer(4)                       :: header_len, s_mtx(2)

        s_mtx = shape(mtx)
        header_len =  len(dict_str(var_type, s_mtx))
        
        open(unit=p_un, file=filename, form="unformatted",access="stream")
        write (p_un) magic_num, magic_str, major, minor
        write (p_un) header_len
        write (p_un) dict_str(var_type, s_mtx)
        write (p_un) mtx
        close(unit=p_un)
    End Subroutine write_int32_mtx
    
    Subroutine  write_int32_vec(filename, vec)
        Implicit None
        character(len=*), intent(in)     :: filename
        integer(4), intent(in)           :: vec(:)
        character(len=*), parameter      :: var_type =  "<i4"
        integer(4)                       :: header_len, s_vec(1)

        s_vec = shape(vec)
        header_len =  len(dict_str(var_type, s_vec))
        
        open(unit=p_un, file=filename, form="unformatted",access="stream")
        write (p_un) magic_num, magic_str, major, minor
        write (p_un) header_len
        write (p_un) dict_str(var_type, s_vec)
        write (p_un) vec
        close(unit=p_un)
    End Subroutine write_int32_vec
    
    Subroutine  write_int16_mtx(filename, mtx)
        Implicit None
        character(len=*), intent(in)     :: filename
        integer(2), intent(in)           :: mtx(:,:)
        character(len=*), parameter      :: var_type =  "<i2"
        integer(4)                       :: header_len, s_mtx(2)

        s_mtx = shape(mtx)
        header_len =  len(dict_str(var_type, s_mtx))
        
        open(unit=p_un, file=filename, form="unformatted",access="stream")
        write (p_un) magic_num, magic_str, major, minor
        write (p_un) header_len
        write (p_un) dict_str(var_type, s_mtx)
        write (p_un) mtx
        close(unit=p_un)
    End Subroutine write_int16_mtx
    
    Subroutine  write_int16_vec(filename, vec)
        Implicit None
        character(len=*), intent(in)     :: filename
        integer(2), intent(in)           :: vec(:)
        character(len=*), parameter      :: var_type =  "<i2"
        integer(4)                       :: header_len, s_vec(1)

        s_vec = shape(vec)
        header_len =  len(dict_str(var_type, s_vec))
        
        open(unit=p_un, file=filename, form="unformatted",access="stream")
        write (p_un) magic_num, magic_str, major, minor
        write (p_un) header_len
        write (p_un) dict_str(var_type, s_vec)
        write (p_un) vec
        close(unit=p_un)
    End Subroutine write_int16_vec
    
    Subroutine  write_int8_mtx(filename, mtx)
        Implicit None
        character(len=*), intent(in)     :: filename
        integer(1), intent(in)           :: mtx(:,:)
        character(len=*), parameter      :: var_type =  "<i1"
        integer(4)                       :: header_len, s_mtx(2)

        s_mtx = shape(mtx)
        header_len =  len(dict_str(var_type, s_mtx))
        
        open(unit=p_un, file=filename, form="unformatted",access="stream")
        write (p_un) magic_num, magic_str, major, minor
        write (p_un) header_len
        write (p_un) dict_str(var_type, s_mtx)
        write (p_un) mtx
        close(unit=p_un)
    End Subroutine write_int8_mtx
    
    Subroutine  write_int8_vec(filename, vec)
        Implicit None
        character(len=*), intent(in)     :: filename
        integer(1), intent(in)           :: vec(:)
        character(len=*), parameter      :: var_type =  "<i1"
        integer(4)                       :: header_len, s_vec(1)

        s_vec = shape(vec)
        header_len =  len(dict_str(var_type, s_vec))
        
        open(unit=p_un, file=filename, form="unformatted",access="stream")
        write (p_un) magic_num, magic_str, major, minor
        write (p_un) header_len
        write (p_un) dict_str(var_type, s_vec)
        write (p_un) vec
        close(unit=p_un)
    End Subroutine write_int8_vec

    function dict_str(var_type, var_shape) result(str)
        implicit none
        character(len=*), intent(in)   :: var_type 
        integer(4), intent(in)         :: var_shape(:)
        character(len=:), allocatable  :: str
        integer(4)                     :: cnt

        cnt =  len("{'descr': '")
        cnt =  cnt + len(var_type)
        cnt =  cnt +  len("', 'fortran_order': True, 'shape': (")
        cnt =  cnt +  len(shape_str(var_shape))
        cnt =  cnt +  len(",), }")
        do while(mod(cnt +  10, 16) /= 0)
            cnt =  cnt +  1
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
        
        do i =  1, size(var_shape)
            start = (i-1) * length + 1
            halt  = i     * length +  1
            write (small_str, "(I13,A)") var_shape(i), ","
            str =  trim(str) // adjustl(small_str)
        enddo
        
        allocate(character(len(trim(str))) :: fin_str)
        fin_str =  trim(str)
    end function shape_str 
end module  m_npy
