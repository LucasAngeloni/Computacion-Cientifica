program ejercicio14
    use kinds
    implicit none
    real(kind=ikind8),dimension(:),allocatable :: A,B,C
    real :: x,y
    integer :: status,cont,i

    cont = 0

    open(unit=1,file="valores.txt")

    do
        read(1,*,IOSTAT=status)x,y
        if(status /=0) exit

        cont = cont + 1
    enddo
    close(1)
    allocate(A(cont),B(cont),C(cont))
    
    open(unit=1,file="valores.txt")
    read(1,*)A,B
    close(1)

    !do i = 1, cont
     !   C(i) = A(i) + B(i)
    !end do

    C = A+B
    print *,C
    
end program ejercicio14