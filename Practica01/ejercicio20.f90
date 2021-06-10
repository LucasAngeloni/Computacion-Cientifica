program ejercicio20
    implicit none

    real(4) :: serie

    print *,serie(0.2,100000)
    print *,log(1+0.2)

end program ejercicio20

real(4) function serie(x,N) result(suma)
    implicit none
    integer :: N,i
    real(4) :: x

    suma = 0
    do i = 1, N
        suma = suma + ((-1.)**(i-1.)/i)*(x**i)
    end do

end function