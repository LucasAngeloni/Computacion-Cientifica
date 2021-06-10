program ejercicio8
    use kinds
    use se_nolineales
    implicit none

    real(rkind8) :: tensiones(3),angulos(3),angulos_radianes(3),y(6)
    real(rkind8) :: Pi = 4*atan(1.0),tol
    integer :: iter
    
    open(unit=10,file="ejercicio8.dat")
    read(10,*) tensiones,angulos
    read(10,*) iter
    read(10,*) tol
    close(10)

    angulos_radianes = angulos*Pi/180

    y(:3) = tensiones
    y(4:) = angulos_radianes

    print *,y

    call metodo_newton_raphson(y,iter,tol)

    print *,y

    angulos = y(4:)*180/Pi
    print *,angulos 
end program ejercicio8