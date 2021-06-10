program ejercicio10
    implicit none

    real,dimension(:),allocatable :: a,b
    integer :: cont = 0,status
    real :: x

    open(unit=1,file="valores.txt")
    do
        read(1,*,IOSTAT=status) x
        if(status /= 0) exit
        cont = cont + 1
    enddo
    close(1)

    allocate(a(cont),b(cont))
    open(unit=2,file="valores.txt")
    read(2,*)a,b
    close(2)

    write(*,*)a,b
    
end program ejercicio10

subroutine intercambiar(n1,n2)
    implicit none
    real, intent(inout) :: n1,n2
    real :: aux

    aux = n1
    n1 = n2
    n2 = aux
end subroutine intercambiar