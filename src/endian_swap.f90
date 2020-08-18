module endian_swap
    implicit none

    PRIVATE
    PUBLIC :: Big_Endian
    PUBLIC :: Swap_Endian

    INTERFACE Swap_Endian
        module procedure SWAP_I8
        module procedure SWAP_F8 
    END INTERFACE Swap_Endian

    CONTAINS
        FUNCTION Big_Endian()

            LOGICAL :: Big_Endian

            Big_Endian = ichar(transfer(1,'a')) == 0

        END FUNCTION Big_Endian

        function SWAP_I8(input) result(output)
            implicit none
            integer(4), parameter  :: b_sz =  8
            integer(b_sz), intent(in) :: input 
            integer(b_sz)             :: output 

            integer(1) :: byte_arr(b_sz), byte_arr_tmp(b_sz)
            integer(1) :: i

            byte_arr_tmp =  transfer(input, byte_arr_tmp)

            do i = 1,b_sz
                byte_arr(i) =  byte_arr_tmp(1 + b_sz - i)
            enddo

            output =  transfer(byte_arr, output)
        end function SWAP_I8

        function SWAP_F8(input) result(output)
            implicit none
            integer(4), parameter  :: b_sz =  8
            real(b_sz), intent(in) :: input 
            real(b_sz)             :: output 

            integer(1) :: byte_arr(b_sz), byte_arr_tmp(b_sz)
            integer(1) :: i

            byte_arr_tmp =  transfer(input, byte_arr_tmp)

            do i = 1,b_sz
                byte_arr(i) =  byte_arr_tmp(1 + b_sz - i)
            enddo

            output =  transfer(byte_arr, output)
        end function SWAP_F8 
END module endian_swap
