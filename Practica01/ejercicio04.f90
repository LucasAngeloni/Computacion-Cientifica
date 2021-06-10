program ejercicio04
    implicit none
    real :: funcion,valor

    write(*,*)"Ingresar valor: "
    read(*,*) valor

    print *, funcion(valor)
end program ejercicio04

real function funcion(x)
    implicit none
    real :: x

    funcion = exp(-x)*sin(2*x)

end function