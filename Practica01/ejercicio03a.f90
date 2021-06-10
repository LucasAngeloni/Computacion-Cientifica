program ejercicio03a
    implicit none

    real :: h,inicio,fin,rango
    real, dimension(:),allocatable :: listaNumeros
    integer :: nroPuntos,i

    h= 1
    inicio = 3
    fin = 20

    rango = fin - inicio
    nroPuntos = rango/h + 1

    allocate(listaNumeros(nroPuntos))

    do i = 1, nroPuntos
        listaNumeros(i) = inicio + h*(i-1)
    end do

    print *,"Lista de numeros:",listaNumeros
    deallocate(listaNumeros)
    
end program ejercicio03a