program ejercicio18
    implicit none

    integer :: numero,i
    integer(8) :: factorial

    factorial = 1

    do numero = 2, 20
        do i = 2, numero
            factorial = factorial*i
        end do
    
        print*,factorial
        factorial = 1
    end do
    
end program ejercicio18