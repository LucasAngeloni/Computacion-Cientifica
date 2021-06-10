program se_nl
    use kinds
    use se_nolineales
    use my_funcs
    implicit none
    
    real(rkind8) :: x(order),tol
    integer :: n_iteraciones

    open(unit=10,file="se_nl.dat")
    read(10,*) tol
    read(10,*) n_iteraciones
    read(10,*) x
    close(10)

    call metodo_newton_raphson(x,n_iteraciones,tol)

    print *,x
end program se_nl