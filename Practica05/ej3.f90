program ej3
    use aj_dat_lin
    use my_funcs
    implicit none

    real(rkind8) :: a(n_param),e
    real(rkind8),allocatable :: datos(:,:)
    integer :: n,m

    open(unit=1,file="ej3.dat")
    read(1,*) n,m
    allocate(datos(n,m))

    read(1,*) datos(:,1)
    read(1,*) datos(:,2)

    close(1)

    call minimos_cuadrados(datos,a,e)
    
    print *,"Valores de los parametros optimos:",a
    print *,"Error residual:",e
end program ej3