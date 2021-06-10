program ej4
    use metodos_aproximacion
    !use kinds
    implicit none
    integer,parameter :: n
    real, dimension(n) :: Y
    real :: x,h,xf

    open(unit=1,file="datos.dat",status="old")
    read(1,*) x
    read(1,*) Y
    read(1,*) xf
    read(1,*) h
    close(1)

    do
        if(x >= xf) exit

        call metodo_Euler(Y,x,h)
    end do

    print *,"Valor de x: ",x
    print *,"Valor de Y(x): ",Y
    
end program ej4