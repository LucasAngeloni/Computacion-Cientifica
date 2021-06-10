program ejercicio16
    implicit none

    integer :: M,N,S,i

    S = 0

    write(*,*)"Ingresar valor de M y N:"
    read(*,*)M,N
    
    do i = M, N
        S = S + i
    end do

    print *,S    
end program ejercicio16