program ejercicio7
    use kinds
    use se_nolineales

    implicit none
    real(rkind8),allocatable :: A(:,:),AInv(:,:)
    integer :: n
    real(rkind8),dimension(3,3) :: identidad = &

    reshape((/1,0,0,0,1,0,0,0,1/),(/3,3/))
    print *,identidad

    open(unit=10,file="ejercicio7.dat")

    read(10,*) n
    allocate(A(n,n),AInv(n,n))

    read(10,*) A
    close(10)

    print *,A

    call matriz_inversa(A,AInv)
    print *,AInv

    print *,matmul(A,AInv) - identidad
    print *,matmul(AInv,A) - identidad
    
end program ejercicio7