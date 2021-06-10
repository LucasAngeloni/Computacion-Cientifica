program pendulo_doble
    use metodos_aproximacion
    use kinds
    use my_funcs
    implicit none

    real(rkind8) :: y0(order),x_inf,h,x_sup
    real(rkind8),parameter :: Pi = 4*atan(1.0)
    real(rkind8) :: x,Y(order)
    integer :: n_pasos,i

    open(unit=1,file="pendulo_doble.dat",status="old")
    read(1,*) x_inf
    read(1,*) x_sup
    read(1,*) y0
    read(1,*) h
    close(1)

    n_pasos = (x_sup - x_inf)/h + 1
    !seteo condicione iniciales
    x = x_inf
    Y(1) = y0(1)*Pi/180
    Y(2) = y0(2)
    Y(3) = y0(3)*Pi/180
    Y(4) = y0(4)
    
    open(unit=2,file="resultados.dat")
    write(2,*) x,Y
    do i = 1, n_pasos
        call metodo_RungeKutta_orden4(Y,x,h)
        write(2,*) x,Y
        write(*,*) x,Y
    end do

    close(2)
    
end program pendulo_doble