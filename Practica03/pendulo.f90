program pendulo
    use kinds
    use metodos_iterativos
    implicit none

    real(rkind8) :: O0,tol,sol,error,theta

    open(unit=10,file="pendulo.dat")
    read(10,*) O0
    read(10,*) tol
    close(10)


    theta = O0*Pi/180
    call metodo_newton(theta,tol,sol,error)

    print *,sol*180/Pi,error
    
end program pendulo