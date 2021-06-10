program ej7
    use kinds
    use my_funcs
    use aj_dat_lin
    implicit none

    real(rkind8),allocatable :: x(:),y(:)
    real(rkind8) :: a(n_param),e,tol
    integer :: n_datos,i,num_iter

    open(unit=10,file="ej7.dat")
    read(10,*) n_datos

    allocate(x(n_datos),y(n_datos))

    do i = 1, n_datos
        read(10,*) x(i),y(i)
    end do
    read(10,*) a
    read(10,*) num_iter
    read(10,*) tol
    close(10)
    print *,a

    call minimos_cuadrados_nolineal(x,y,a,e,num_iter,tol)

    print *,"Valores de los parametros optimos:",a
    print *,"Error residual:",e

    deallocate(x,y)

end program ej7