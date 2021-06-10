module my_funcs
    use kinds
    implicit none

    integer,parameter :: n_param = 2

    contains

    function f(x)
        implicit none
        real(rkind8) :: f(n_param)
        real(rkind8),intent(in) :: x
    
        f(1) = 1
        f(2) = cos(x)
        !f(3) = log(x)
    end function

    function derivf(x,a)
        implicit none
        real(rkind8) :: derivf(n_param)
        real(rkind8),intent(in) :: x,a(n_param)

        real(rkind8) :: d=2.8

        derivf(1) = x*d/(log(x*d)+a(2))
        derivf(2) = -a(1)*x*d/((log(x*d)+a(2))**2)

    end function

    function Est(x,a)
        implicit none
        real(rkind8) :: Est
        real(rkind8),intent(in) :: x,a(n_param)

        real(rkind8) :: d=2.8
        
        Est = a(1)*x*d/(log(x*d)+a(2))

    end function
end module