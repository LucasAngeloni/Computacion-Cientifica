program ej3
    use metodos_iterativos
    use kinds
    implicit none
    real(rkind8) :: a,b,tol,x0,solucion,error

    a = 1E-16
    b = 20
    tol = 10E-9
    x0 = 0.3

    call metodo_newton(x0,tol,solucion,error)

    print *,solucion,error
    
end program ej3