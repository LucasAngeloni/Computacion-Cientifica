module errores
    use kinds
    use my_funcs
    implicit none

    contains

    function error_truncamiento_global(x,valor_aproximado)

        real(rkind8),intent(in) :: x,valor_aproximado(order)
        real(rkind8) :: error_truncamiento_global(order)

        error_truncamiento_global = abs(valor_aproximado - calculo_valor_real(x))
    end function

    function calculo_valor_real(x)

        real(rkind8) :: x
        real(rkind8) :: calculo_valor_real(order)

        calculo_valor_real(1) = -0.5*x**4 + 4*x**3 - 10*x**2 +8.5*x + 1
        calculo_valor_real(2) = -2*x**3 + 12*x**2 - 20*x + 8.5
    end function
end module

program ej3
    use metodos_aproximacion
    use errores
    use my_funcs
    implicit none

    real(rkind8) :: y0(order),x_inf,h,x_sup,Y(order)
    real(rkind8) :: x
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
    do i = 1, n_pasos
        call metodo_Euler(Y,x,h)
    end do

    print *,"Error de truncamiento global: ",error_truncamiento_global(x,Y)

    call system("gnuplot -p grafica.plt")
    write(*,*) "Ejecucion finalizada"
    write(*,*) "Se grafico el histograma en histograma.pdf"
    
end program ej3