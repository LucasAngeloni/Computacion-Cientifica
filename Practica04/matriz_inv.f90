program matriz_inv
    use kinds
    use sistemas_ecuaciones_lineales
    implicit none

    real(rkind8),allocatable :: A(:,:),Inv(:,:)
    integer :: n,i

    open(unit=10,file="datos.dat")
    read(10,*) n
    allocate(A(n,n),Inv(n,n))

    do i = 1, n
        read(10,*) A(i,:)
    end do
    close(10)

    print *,A

    call matriz_inversa(A,Inv)

    print *,"Inversa :",Inv
    deallocate(A,Inv)
    
end program matriz_inv