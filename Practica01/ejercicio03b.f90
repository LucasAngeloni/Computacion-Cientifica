program ejercicio03b
    implicit none

    integer :: n=15,i
    real,dimension(:),allocatable :: valores
    real :: pi,rango,tamanoIntervalo

    allocate(valores(n))
    pi = 4*atan(1.0)

    rango = 2*pi - pi
    tamanoIntervalo = rango/(n-1)

    do i = 1, n
        valores(i) = pi + tamanoIntervalo*(i-1)
    end do

    print *,valores
    
end program ejercicio03b