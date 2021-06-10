program ejercicio08
    implicit none

    real,dimension(15) :: A
    real :: valMin

    A = [23.33, 8.08, 44.99, 21.95, 44.93, 29.82, 41.08,28.57, 4.39, 43.79, 20.02, 26.59, 41.13, 39.32, 26.85]

    print *,"El valor minimo de la lista es: ",valMin(A,15)
    print *,"El valor minimo de la lista es: ",minval(A)
    
end program ejercicio08

real function valMin(lista,cant) result(minval)
    implicit none
    integer :: cant,i
    real,dimension(cant) :: lista

    minval = lista(1)
    do i = 2, cant
        if(lista(i) < minval) then
            minval = lista(i)
        endif
    end do
    
end function valMin