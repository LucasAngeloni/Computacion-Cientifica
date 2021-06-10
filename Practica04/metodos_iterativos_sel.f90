program metodos_iterativos_sel
    use kinds
    use sistemas_ecuaciones_lineales
    implicit none

    real(rkind8),allocatable :: A(:,:),B(:),X(:)
    integer :: n,i
    real(rkind8) :: tol
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

    if ( estrictamente_dominante(A) ) then
        print *,"Es estrictamente dominante por filas, por lo tanto, el proceso iterativo converge"
    else
        print *,"No es estrictamente dominante por filas, por lo tanto, el proceso iterativo no se sabe si converge"
    end if

    call convert_diag_dom(A,status)

    if(status) then
        print *,"Es estrictamente dominante"
    else
        print *,"No se puede transformar a una matriz estrictamente dominante"
    end if

    print *,"X=",X

    do i = 1, n
        print *,sum(A(i,:)*X)
    end do
    deallocate(A,B,X)
    
end program metodos_iterativos_sel