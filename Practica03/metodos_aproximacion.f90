module metodos_aproximacion
    use kinds
    use my_funcs
    implicit none
    contains

    subroutine metodo_Euler(Y,x,h)
        
        implicit none
        real(rkind8),intent(inout) :: Y(order),x
        real(rkind8),intent(in) ::  h

        Y = Y + h*DERIV(x,Y)
        x = x + h
    
    end subroutine metodo_Euler

    subroutine metodo_PuntoMedio(Y,x,h)
        implicit none
        real(rkind8),intent(inout) :: Y(order),x
        real(rkind8),intent(in) ::  h
        real(rkind8) :: Y_ast(order)

        Y_ast = Y + h*DERIV(x,Y)/2

        Y = Y + h*DERIV(x+h/2, Y_ast)
        x = x + h
    
    end subroutine metodo_PuntoMedio

    subroutine metodo_Heun(Y,x,h)
        implicit none
        real(rkind8),intent(inout) :: Y(order),x
        real(rkind8),intent(in) ::  h
        real(rkind8) :: P(order),Y_ast(order)

        Y_ast = Y + h*DERIV(x,Y)

        P = (DERIV(x,Y) + DERIV(x+h, Y_ast))/2

        Y = Y +h*P
        x = x + h
    
    end subroutine metodo_Heun

    subroutine metodo_RungeKutta_orden4(Y,x,h)
        implicit none
        real(rkind8),intent(inout) :: x
        real(rkind8),dimension(:),intent(inout) :: Y(order)
        real(rkind8),intent(in) ::  h
        real(rkind8) :: Alfa(3),Gama(4),Beta(3,3)
        integer :: i

        Alfa(1) = 0.5
        Alfa(2) = 0.5
        Alfa(3) = 1

        Gama(1) = 1./6
        Gama(2) = 2./6
        Gama(3) = 2./6
        Gama(4) = 1./6

        Beta(1,1) = 0.5
        Beta(1,2) = 0
        Beta(1,3) = 0
        Beta(2,1) = 0
        Beta(2,2) = 0.5
        Beta(2,3) = 0
        Beta(3,1) = 0.33
        Beta(3,2) = 0
        Beta(3,3) = 1

        Y = Y + h*P(Gama,Alfa,Beta,x,Y,h,i)        
        x = x + h
    
    end subroutine metodo_RungeKutta_orden4

    function P(G,A,B,x,Y,h,n_funcion)
        implicit none
        real(rkind8),intent(in) :: Y(order),x,h
        real(rkind8),dimension(:),intent(in) :: G,A
        real(rkind8),intent(in) :: B(:,:)
        real(rkind8) :: P(order)
        integer,intent(in) :: n_funcion
        integer :: i

        P = 0
        do i = 1, size(G)
            P = P + G(i)*K(i,A,B,x,Y,h,n_funcion)
        end do
    end function

    recursive function K(j,A,B,x,Y,h,n_funcion) result(K_ast)
        implicit none
        real(rkind8),intent(in) :: Y(order),x,h
        real(rkind8), dimension(:),intent(in) :: A
        real(rkind8),intent(in) :: B(:,:)
        integer, intent(in):: j,n_funcion
        real(rkind8) :: K_ast(order),Y_ast(order),suma(order)
        integer :: l


        if(j == 1) then
            K_ast = DERIV(x,Y)
        else
            suma = 0
            do l = 1, j - 1
                suma = suma + B(j-1,l)*K(l,A,B,x,Y,h,n_funcion)
            end do
            Y_ast = Y + h*suma

            K_ast = DERIV(x+A(j-1)*h,Y_ast)
        end if

    end function

end module