program plano_inclinado
    use metodos_iterativos
    use kinds
    implicit none
    real(rkind8) :: tol,x0,x1,t=1

    x0 = 0
    tol = 10E-7

    do while(t < 1000)
        call metodo_newton(x0,tol,x1,t)
        t = t + 1
    end do

    print *,"x1: ",x1
    print *,"t: ",t
    
end program plano_inclinado