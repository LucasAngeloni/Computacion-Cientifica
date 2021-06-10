program ej5
    use kinds
    use aj_dat_lin
    use my_funcs
    implicit none

    real(rkind8),allocatable :: datos(:,:)
    real(rkind8) :: a(n_param),e
    integer :: n_datos,i

    open(unit=10,file="p5_ej05.dat")
    read(10,*) n_datos

    allocate(datos(n_datos,2))

    do i = 1, n_datos
        read(10,*) datos(i,:)
    end do

    call minimos_cuadrados(datos,a,e)

    print *,"Valores de los parametros optimos:",a
    print *,"Error residual:",e

end program ej5