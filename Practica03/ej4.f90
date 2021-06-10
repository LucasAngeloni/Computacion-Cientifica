program ej4
    use metodos_iterativos
    use kinds
    implicit none
    real(rkind8) :: a,b,tol,solucion,error
    real(rkind8) :: a0,b0,paso,x0
    real(rkind8),allocatable :: raices(:)
    integer :: raices_encontradas,n_raices,i,num_intervalos=1000
    logical :: convergencia

    open(unit=10,file="ej4.dat")
    read(10,*) n_raices
    read(10,*) a0
    read(10,*) b0
    read(10,*) tol
    close(10)

    allocate(raices(n_raices))

    paso = (b0 - a0)/(num_intervalos-1)
    raices_encontradas = 1
    do i = 1,num_intervalos
        !a = a0 + paso*(i-1)
        !b = b0
        x0 = a0 + paso*(i-1)
        call metodo_newton(x0,tol,solucion,error,convergencia)
        if(convergencia) then
            solucion = nint(solucion*10E6)/(10E6)
            if (.not.any(solucion == raices)) then
                raices(raices_encontradas) = solucion
                raices_encontradas = raices_encontradas + 1
                if(raices_encontradas == 3) then
                    exit
                end if
            end if
        end if 
    end do

    print *,raices

    deallocate(raices)
    
end program ej4