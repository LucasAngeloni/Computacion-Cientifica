module my_funcs
    use kinds
    implicit none
    integer, parameter :: order = 2
    contains
    
    function DERIV(x,Y)
        implicit none
        real(rkind8),intent(in) :: x,Y(order)
        real(rkind8) :: DERIV(order)

        !Declaracion de variables internas
        real(rkind8),parameter :: b=2,m=0.25,g=9.8

        DERIV(1) = Y(3) !Cambio de posicion en el eje x
        DERIV(2) = Y(4) !Cambio de posicion en el eje y
        DERIV(3) = -b/m*Y(3) !Aceleración en el eje x        
        DERIV(4) = -g - b/m*Y(4) !Aceleración en el eje y
    end function
    
    function pendulo_doble(x,Y)
        implicit none
        real(rkind8),intent(in) :: x,Y(order)
        real(rkind8) :: pendulo_doble(order)

        !Declaracion de variables internas
        real(rkind8),parameter :: m1=0.5,m2=0.5,L1=0.5,L2=0.5,g=9.8
        real(rkind8) :: numerador,numerador2,denominador,denominador2

        pendulo_doble(1) = Y(2)
        numerador = -g*(2*m1+m2)*sin(Y(1)) - m2*g*sin(Y(1)-2*Y(3)) - 2*sin(Y(1)-Y(3))*m2*((Y(4)**2)*L2 + Y(2)**2*L1*cos(Y(1)-Y(3)))
        denominador = L1*(2*m1+m2-m2*cos(2*Y(1)-2*Y(3))) 
        pendulo_doble(2) = numerador/denominador
        pendulo_doble(3) = Y(4)
        numerador2 = 2*sin(Y(1)-2*Y(3))*((Y(2)**2)*L1*(m1+m2)+g*(m1+m2)*cos(Y(1)) + m2*(Y(4)**2)*L2*cos(Y(1)-Y(3)))
        denominador2 = L2*(2*m1+m2 - m2*cos(2*Y(1)-2*Y(3)))
        pendulo_doble(4) = numerador2/denominador2
    end function
    
    function pendulo_simple(x,Y)
        implicit none
        real(rkind8),intent(in) :: x,Y(order)
        real(rkind8) :: pendulo_simple(order)

        !Declaracion de variables internas
        real(rkind8) :: k=1,m=1,b=0

        pendulo_simple(1) = Y(2)
        pendulo_simple(2) = -b/m*Y(2) - k/m*Y(1)
    end function

end module