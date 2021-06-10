program ejercicio3
    use kinds
    use sistemas_ecuaciones_lineales
    implicit none

    real(rkind8),allocatable :: A(:,:),B(:),X(:)
    real(rkind8) :: tol
    integer(ikind4) :: i,n
    logical :: estado

    open(unit=10,file="ejercicio3.dat")
    read(10,*) n

    allocate(A(n,n),B(n),X(n))
    do i = 1, n
        read(10,*) A(i,:), B(i)
    end do

    read(10,*) tol
    read(10,*) X
    close(10)

    call convert_diag_dom(A,estado)
    if (estado) then
        print *,"Se convirtio a diagonalmente dominante"
    else
        print *,"No se puede convertir a diagonalmente dominante"
    end if

    call eliminacion_gauss_jordan(A,B,X)
    print *,X

    print *,matmul(A,X)

end program ejercicio3