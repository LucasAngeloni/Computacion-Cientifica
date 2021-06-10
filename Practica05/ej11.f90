program ej11
    use kinds
    use aj_dat_lin
    implicit none

    real(rkind8),allocatable :: x(:),y(:),a(:)
    real(rkind8) :: paso,p,deriv
    integer :: n_datos,i,num_valores

    open(unit=10,file="p5_ej04.dat")
    read(10,*) n_datos

    allocate(x(n_datos),y(n_datos),a(n_datos-1))
    do i = 1, n_datos
        read(10,*) x(i),y(i)
    end do
    close(10)

    call ordenar(x,y,n_datos)

    print *,x
    num_valores = 101
    paso = (x(n_datos) - x(1))/(num_valores-1)

    deriv = 0
    do i = 1, size(x)-1
        a(i) = (y(i+1) - y(i))/(x(i+1) - x(i))**2 - deriv/(x(i+1) - x(i))
        deriv = deriv*x(i+1) + 2*a(i)*x(i+1)*(x(i+1)-x(i))
    end do

    open(unit=11,file="spline_cuadratico.dat")
    do i = 1, num_valores
        p = x(1) + paso*(i-1)
        write(11,*) p,spline_cuadratico(x,y,p,a)
    end do
    close(11)

    call system("gnuplot graficar.gp")
    deallocate(x,y)
end program ej11

subroutine ordenar(x,y,n)
    use kinds
    implicit none
    real(rkind8),intent(inout) :: x(n),y(n)
    integer :: i,j,n
    real(rkind8) :: x_aux,y_aux

    do i = 1, n
        do j = i+1,n
            if(x(j) <= x(i)) then
                x_aux = x(j)
                x(j) = x(i)
                x(i) = x_aux
                y_aux = y(j)
                y(j) = y(i)
                y(i) = y_aux
            end if
        end do
        if ( i > 1 .and. x(i) == x(i-1) ) then
            print *,"Hay dos abcisas iguales"
            stop
        end if
    end do

end subroutine ordenar