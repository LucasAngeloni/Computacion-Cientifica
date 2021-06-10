program ej4
    use aj_dat_lin
    use my_funcs
    implicit none

    real(rkind8) :: a(n_param),e
    real(rkind8),allocatable :: datos(:,:)
    integer :: n,m,i

    open(unit=1,file="p5_ej04.dat")
    read(1,*) n,m
    allocate(datos(n,m))

    do i = 1, n
        read(1,*) datos(i,:)
    end do
    close(1)

    call minimos_cuadrados(datos,a,e)
    
    print *,"Valores de los parametros optimos:",a
    print *,"Error residual:",e
end program ej4