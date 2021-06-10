program ejercicio6
    use kinds
    use se_nolineales

    implicit none
    real(rkind8) :: X(order)
    real(rkind8) :: tol
    integer :: iter

    open(unit=10,file="ejercicio6.dat")

    read(10,*) tol
    read(10,*) X
    read(10,*) iter
    close(10)

    print *,X

    call metodo_gauss_seidel(X,iter,tol)
    print *,X
    
end program ejercicio6