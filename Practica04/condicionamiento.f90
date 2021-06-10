program condicionamiento
    use kinds
    use sistemas_ecuaciones_lineales
    implicit none

    real(rkind8),allocatable :: A(:,:),B(:),X(:)
    integer :: n,i
    real(rkind8) :: tol
    real(rkind8), parameter :: epsilon = 0.119209E-6
    logical :: status

    open(unit=10,file="datos.dat")
    read(10,*) n
    allocate(A(n,n),B(n),X(n))

    do i = 1, n
        read(10,*) A(i,:),B(i)
    end do
    read(10,*) tol
    read(10,*) X
    close(10)

    print *,numero_condicion(A)*epsilon
    deallocate(A,B,X)
    
end program condicionamiento