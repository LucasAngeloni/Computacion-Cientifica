program ej2
    use metodos_aproximacion
    use kinds
    use my_funcs
    implicit none

    real(rkind8) :: y0(order),x_inf,h,x_sup
    real(rkind8) :: x,Y(order)
    integer :: n_pasos,i

    open(unit=1,file="datos.dat",status="old")
    read(1,*) x_inf
    read(1,*) y0
    read(1,*) x_sup
    read(1,*) h
    close(1)

    n_pasos = (x_sup - x_inf)/h
    !seteo condicione iniciales
    x = x_inf
    Y = y0
    open(unit=2,file="resultados.dat")
    write(2,*) x,Y
    do i = 1, n_pasos
        call metodo_RungeKutta_orden4(Y,x,h)
        write(2,*) "Valores de x:", x
        write(2,*) "Valores de Y:", Y
    end do

    close(2)
    
end program ej2