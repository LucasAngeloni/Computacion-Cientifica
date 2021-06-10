program pruebas
    implicit none
    integer :: a,b

    a = -5
    b = -3

    print *, sign(a,b)

    if(a == sign(a,b)) then
        print *,"Son del mismo signo"
    else
        print *,"Son de signos diferentes"
    end if
    
end program pruebas