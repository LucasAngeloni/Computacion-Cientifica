module my_funcs
    use kinds
    implicit none

    integer,parameter :: order=2
    real(rkind8), parameter :: Pi = 4*atan(1.0)
    contains

    function DERIV(t,x)
        implicit none

        real(rkind8) :: DERIV(order),x(order)
        real(rkind8) :: constante,ang_radianes,t
        real(rkind8),parameter :: m=0.5,ud=0.12,ang=30,g=9.8

        ang_radianes = ang*Pi/180
        constante = ud*g*cos(ang_radianes) + g*sin(ang_radianes)

        DERIV(1) = 500*exp(-0.2*t)/m - constante
        DERIV(2) = -100*exp(-0.2*t)/m - constante

    end function

    function F(x)
        implicit none
        real(rkind8) :: F
        real(rkind8),intent(in) :: x
        real(rkind8) :: g=9.8,L=0.2,theta_ini=30*Pi/180

        F = sin(log(2*x))
        !F = (x-1)*(x-sqrt(2.))*(x-2*Pi)
        !F = g/L*sin(theta) - theta_ini
    end function

    function derivF(x)
        implicit none
        real(rkind8) :: derivF
        real(rkind8) :: x,t
        real(rkind8) :: g=9.8,L=0.2

        derivF = cos(log(2*x))/x
        !derivF = g/L*cos(theta)
    end function
end module
