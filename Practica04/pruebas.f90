program pruebas
    use kinds
    use sistemas_ecuaciones_lineales
    implicit none

    real(rkind8),allocatable :: A(:,:),B(:),X(:)
    integer :: n,i

    open(unit=10,file="datos.dat")
    read(10,*) n
    allocate(A(n,n),B(n),X(n))

    do i = 1, n
        read(10,*) A(i,:),B(i)
    end do
    close(10)

    call eliminacion_gaussiana_pivoteo_parcial(A,B,X)

    print *,"X=",X
    deallocate(A,B,x)
    
end program pruebas