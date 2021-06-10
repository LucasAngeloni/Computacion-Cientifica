module metodos_iterativos
    use kinds
    use metodos_aproximacion
    use my_funcs
    implicit none

    contains

        subroutine metodo_biseccion(Xa,Xb,tol,c,error)
            implicit none
            real(rkind8),intent(inout) :: Xa,Xb,tol
            real(rkind8),intent(out) :: c,error
            
            real(rkind8) :: Fc
            integer :: i

            if(F(Xa) == sign(F(Xa),F(Xb))) then
                print *,"Los extremos son del mismo signo, no se puede aplicar el metodo"
                return
            end if

            i = 1
            error = abs(Xb - Xa)
            do
                c = (Xa + Xb)/2

                Fc = F(c)
                if (abs(Fc) < tol) then
                    error = error/(2**i)
                    return
                else if(F(Xa) == sign(F(Xa),Fc)) then
                    Xa = c
                else
                    Xb = c
                end if
                i = i + 1
            end do
            
        end subroutine

        subroutine metodo_falsa_posicion(Xa,Xb,tol,c,error)
            implicit none
            real(rkind8) :: Xa,Xb,tol,Fc
            real(rkind8) :: c,error

            integer :: i

            if(F(Xa) == sign(F(Xa),F(Xb))) then
                print *,"Las funciones en los valores extremos son del mismo signo, no se puede aplicar el metodo"
                return
            end if

            i = 1
            error = abs(Xa - Xb)
            do
                c = Xb - (F(Xb)*(Xb-Xa))/(F(Xb)-F(Xa))

                Fc = F(c)
                if ((abs(c - Xa) < tol .or. abs(c - Xb) < tol) .and. abs(Fc) < tol ) then
                    error = error/(2**i)
                    return
                else if(F(Xa) == sign(F(Xa),Fc)) then
                    Xa = c
                else
                    Xb = c
                end if

                i = i + 1
            end do

        end subroutine

        subroutine metodo_newton(x0,tol,x1,error,converge)
            implicit none
            real(rkind8),intent(inout) :: x0,tol
            real(rkind8),intent(inout) :: x1,error
            logical :: converge
            real(rkind8) :: x(order)
            real(rkind8) :: h
            integer :: i

            h = 0.01
            converge = .true.
            do i = 1,30000
                !t = i
                !do while(t < i + 1)
                !    call metodo_RungeKutta_orden4(x,t,h)
                !end do

                if(derivF(x0) /= 0) then
                    x1 = x0 - F(x0)/derivF(x0)
                else
                    print *,"La derivada de la funcion no puede ser igual a 0"
                    converge = .false.
                    return
                end if

                if(abs(x1-x0) < tol .and. abs(F(x1)) < tol) then
                    error = abs(F(x1))
                    !print *,i
                    return
                else
                    x0 = x1
                end if
            end do

            print *,"El proceso no converge"
            converge = .false.
        end subroutine

        subroutine metodo_secante(x0,x1,tol)
            implicit none
            real(rkind8) :: x0,x1,tol,x2

            do
                x2 = x1 - (F(x1)*(x1-x0))/(F(x1)-F(x2))
                print *,x2

                if(abs(x2-x1) < tol .and. abs(F(x2)) < tol) then
                    return
                else
                    x0 = x1
                    x1 = x2
                end if
            end do    

        end subroutine
end module
