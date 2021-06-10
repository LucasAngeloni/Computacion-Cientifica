program ejercicio09
    implicit none

    real :: a,b

    !write(*,*)"Ingresar dos valores:"
    !read(*,*)a,b

    open(unit=1,file="valores.txt")
    read(1,*)a,b
    close(1)
    
    call intercambiar(a,b)
    write(*,*)a,b
    
end program ejercicio09

subroutine intercambiar(n1,n2)
    implicit none
    real, intent(inout) :: n1,n2
    real :: aux

    aux = n1
    n1 = n2
    n2 = aux
end subroutine intercambiar