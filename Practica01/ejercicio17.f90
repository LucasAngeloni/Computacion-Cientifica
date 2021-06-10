program ejercicio17
    implicit none

    real(4) :: f
    integer(kind=8) :: i,n
    real :: valor_real,error_porcentual
    real,parameter :: Pi = 3.14159

    valor_real = (Pi**4)/90

    n=10000
    f=0
    do i = n, 1,-1
       f = f + 1.0/(i**4) 
    end do

    error_porcentual = abs(f-valor_real)/valor_real*100
    print*,"f= ",f
    print*,"Valor real= ",valor_real
    print*,"Error porcentual = %",error_porcentual
    
    
end program ejercicio17