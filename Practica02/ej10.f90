program ej10
    use metodos_aproximacion
    use kinds
    use my_funcs
    implicit none

    real(rkind8) :: y0(order),x_inf,h,y_final
    real(rkind8),parameter :: Pi = 4*atan(1.0)
    real(rkind8) :: x,Y(order)
    integer :: i

    open(unit=1,file="ej10.dat",status="old")
    read(1,*) x_inf
    read(1,*) y_final
    read(1,*) y0
    read(1,*) h
    close(1)

    !seteo condicione iniciales
    x = x_inf
    Y(1) = y0(1) !posicion en el eje x
    Y(2) = y0(2) !posicion en el eje y
    Y(3) = y0(3)*cos(y0(4)*Pi/180) !velocidad en el eje x
    Y(4) = y0(3)*sin(y0(4)*Pi/180) !velocidad en el eje y
    
    open(unit=2,file="resultados.dat")
    write(2,*) x,Y
    write(*,*) sqrt(Y(3)**2 + Y(4)**2)
    do
        call metodo_RungeKutta_orden4(Y,x,h)
        write(2,*) x,Y
        write(*,*) sqrt(Y(3)**2 + Y(4)**2)
        if ( Y(2) <= 0 ) then
            exit
        end if
    end do
    close(2)

    call system("gnuplot -p grafica.plt")
    write(*,*) "Ejecucion finalizada"
    write(*,*) "Se grafico el histograma en histograma.pdf"

end program ej10