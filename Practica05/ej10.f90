program ej10
    use kinds
    use aj_dat_lin
    
    implicit none
    real(rkind8),allocatable :: datos(:,:)
    real(rkind8) :: valor_max,valor_min,paso,x
    integer :: n_datos,i,num_valores

    open(unit=10,file="p5_ej10.dat")
    read(10,*) n_datos

    allocate(datos(n_datos,2))

    do i = 1, n_datos
        read(10,*) datos(i,:)
    end do
    close(10)

    valor_min = minval(datos(:,1))
    valor_max = maxval(datos(:,1))

    num_valores = 101
    paso = (valor_max-valor_min)/(num_valores-1)

    open(unit=11,file="puntos_obtenidos.dat")
    do i = 1, num_valores
        x = valor_min + paso*(i-1)
        
        write(11,*) x,interpolacion_Lagrange(datos,x)
    end do
    close(11)

    call system("gnuplot graficar.gp")

end program ej10