module se_nolineales
    use kinds
    use my_funcs
    implicit none

    contains

    real(rkind8) function norma(vector)
        implicit none
        real(rkind8),intent(in) :: vector(:)
        
        real(rkind8) :: suma
        integer :: i

        suma = 0
        do i = 1, size(vector)
            suma = suma + vector(i)**2
        end do

        norma = dsqrt(suma)
    end function

    logical function es_divergente(P)
        implicit none
        real(rkind8) :: P(:)
        real(rkind8) :: dG(size(P),size(P))

        real(rkind8) :: suma
        integer :: i

        dG = deriv_parciales(P)

        es_divergente = .true.
        do i = 1, size(P)
            suma = sum(abs(dG(i,:)))
            if (suma < 1) then
                es_divergente = .false.
            end if
        end do
    end function

    subroutine metodo_punto_fijo(P,num_iteraciones,tol)
        implicit none
        real(rkind8),intent(inout) :: P(:)
        real(rkind8),intent(in) :: tol
        integer,intent(in) :: num_iteraciones

        integer :: i
        real(rkind8) :: P1(size(P))

        do i = 1, num_iteraciones
            P1 = G(P)
            !if ( es_divergente(P1) ) then
            !    print *,"El metodo diverge"
            !    return
            !end if
            print *,P1
            if ( norma(P1 - P) < tol ) then
                P = P1
                return
            else
                P = P1
            end if
        end do
        print *,"El metodo no converge"

    end subroutine metodo_punto_fijo

    subroutine metodo_gauss_seidel(P,num_iteraciones,tol)
        implicit none
        real(rkind8),intent(inout) :: P(:)
        real(rkind8),intent(in) :: tol
        integer,intent(in) :: num_iteraciones

        integer :: i
        real(rkind8) :: P1(size(P))

        do i = 1, num_iteraciones
            P1 = G_gauss_seidel(P)
            print *,P1
            if ( norma(P1 - P) < tol ) then
                P = P1
                return
            else
                P = P1
            end if
        end do
        print *,"El metodo no converge"

    end subroutine metodo_gauss_seidel

    subroutine metodo_newton_raphson(x,num_iteraciones,tol)
        implicit none
        real(rkind8),intent(inout) :: x(:)
        real(rkind8),intent(in) :: tol
        integer,intent(in) :: num_iteraciones

        integer :: i,j
        real(rkind8) :: x1(size(x)),JInv(size(x),size(x)),dx(size(x))

        do i = 1, num_iteraciones
            call matriz_inversa(jacobiano(x(:3),x(4:)),JInv)
            do j = 1, size(x)
                dx(j) = sum(JInv(j,:)*F(x(:3),x(4:)))
            end do
            
            x1 = x - dx
            if ( norma(x1 - x) < tol ) then
                x = x1
                return
            end if
            x = x1
        end do
    
    end subroutine metodo_newton_raphson

    subroutine matriz_inversa(A,Inv)
        implicit none
        !Declaración parametros de entrada y salida
        real(rkind8),intent(in) :: A(:,:)
        real(rkind8),intent(out),dimension(:,:) :: Inv

        !Declaración variables de la subrutina
        real(rkind8),allocatable,dimension(:,:) :: A_amp
        real(rkind8) :: m
        integer :: n,i,j

        n = size(A(1,:))
        allocate(A_amp(n,2*n)) !Notar que el numero de columnas es el doble ya que va a estar conformada por dos matrices cuadradas

        !Creación de la matriz ampliada
        A_amp(:,:n) = A(:,:)

        do i = 1, n
            do j = 1, n
                if(i==j) then
                    A_amp(i,n+j) = 1
                else
                    A_amp(i,n+j) = 0
                end if
            end do
        end do
        
        !Reducción de la matriz mediante el método de Gauss Jordan
        do i = 1, n
            if ( A_amp(i,i) == 0 ) then
                call permutacion_filas(A_amp,i)
            end if
            A_amp(i,:) = A_amp(i,:)/A_amp(i,i)
            do j = i+1,n
                m = A_amp(j,i) !Múltiplo por el cual se va a restar la fila
                A_amp(j,:) = A_amp(j,:) - m*A_amp(i,:)
            end do
        end do
        
        !Transformación a la matriz identidad
        do i = n,2,-1
            do j = 1,i-1
                m = A_amp(j,i)
                A_amp(j,i) = A_amp(j,i) - m        
                A_amp(j,n+1:) = A_amp(j,n+1:) - m*A_amp(i,n+1:)
            end do
        end do

        Inv = A_amp(:,n+1:)
        deallocate(A_amp)
    end subroutine

    subroutine permutacion_filas(A_amp,fila)
        implicit none
        real(rkind8),intent(inout) :: A_amp(:,:)
        integer,intent(in) ::  fila

        real(rkind8),allocatable :: aux(:)
        integer :: n_columnas,i,n_filas

        n_columnas = size(A_amp(1,:))
        n_filas = size(A_amp(:,1))
        allocate(aux(n_columnas))

        do i = fila+1, n_filas
            if(A_amp(i,fila) /= 0) then
                aux = A_amp(fila,:)
                A_amp(fila,:) = A_amp(i,:)
                A_amp(i,:) = aux
                deallocate(aux)
                exit
            elseif(i == n_filas) then
                write(*,*) "Error, pivote igual a 0"
                deallocate(aux)
                stop
            end if
        end do
    end subroutine permutacion_filas

end module

