module sistemas_ecuaciones_lineales
    use kinds
    implicit none
    
    contains

    real(rkind8) function numero_condicion(A)
        implicit none
        real(rkind8) :: A(:,:)

        real(rkind8) :: AInv(size(A,1),size(A,2))

        call matriz_inversa(A,AInv)

        numero_condicion = norma_matriz(A)*norma_matriz(AInv)

    end function

    real(rkind8) function norma_matriz(A)
        implicit none
        real(rkind8) :: A(:,:)

        real(rkind8) :: suma
        integer :: i

        norma_matriz = 0
        do i = 1,size(A,1)
            suma = sum(abs(A(i,:)))
            if ( norma_matriz < suma ) then
                norma_matriz = suma
            end if
        end do
    end function

    logical function diferentes(indices)
        implicit none
        integer :: indices(:),i

        diferentes = .true.
        do i = 1, size(indices)-1
            if ( any(indices(i) == indices(i+1:size(indices))) ) then 
                diferentes = .false.
                return
            end if
        end do
    end function

    logical function dominantes(A,indices)
        implicit none

        real(rkind8) :: A(:,:)
        integer :: indices(:),suma,i,j

        dominantes = .true.
        do i = 1, size(A,1)
            suma = 0
            do j = 1,size(A,2)
                if (j /= indices(i)) then
                    suma = suma + abs(A(i,j))
                end if
            end do
            if ( abs(A(i,indices(i))) <= suma ) then
                dominantes = .false.
                return
            end if
        end do
    end function

    subroutine convert_diag_dom(A,status)
        implicit none
        real(rkind8),intent(inout) :: A(:,:)
        logical,intent(out) :: status

        integer :: i,t
        integer :: max_indices(size(A,1))

        do i = 1, size(A,1)
            max_indices(i) = maxloc(abs(A(i,:)),t) !Obtiene los indices donde se encuentran los maximos valores de cada fila
        end do

        status = .true.
        if(diferentes(max_indices) .and. dominantes(A,max_indices)) then
            call swap_arr(A,max_indices)
        else
            status = .false.
            return
        end if

    end subroutine

    subroutine swap_arr(matriz,indices)
        implicit none
        real(rkind8),intent(inout) :: matriz(:,:)
        integer,intent(in) ::  indices(:)

        real(rkind8) :: aux(size(matriz,1),size(matriz,2))
        integer :: i

        do i = 1, size(indices)
            aux(indices(i),:) = matriz(i,:)
        end do   

        matriz = aux
    end subroutine swap_arr

    logical function estrictamente_dominante(A) result(salida)
        implicit none
        real(rkind8) :: A(:,:) !Matriz

        integer :: suma,i,j

        salida = .true.
        do i = 1, size(A(:,1))
            suma = 0
            do j = 1, size(A(1,:))
                if ( i /= j ) then
                    suma = suma + abs(A(i,j))
                end if
            end do
            if ( abs(A(i,i)) < suma ) then
                salida = .false.
                return
            end if
        end do
    end function

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

    subroutine filas_mas_grande(A,fila)
        implicit none
        real(rkind8),intent(inout) :: A(:,:)
        integer,intent(in) :: fila
        
        integer :: fila_mayor
        real(rkind8),allocatable :: aux(:)
        integer :: n_columnas,i

        fila_mayor = fila
        n_columnas = size(A(1,:))
        do i = fila+1, n_columnas-1
            if(abs(A(i,fila)) > abs(A(fila_mayor,fila))) then
                fila_mayor = i
            end if
        end do
        
        if (fila_mayor /= fila) then    
            allocate(aux(n_columnas))
            aux = A(fila_mayor,:)
            A(fila_mayor,:) = A(fila,:)
            A(fila,:) = aux
            deallocate(aux)
        end if

    end subroutine filas_mas_grande

    subroutine eliminacion_gaussiana(A,B,x)
        implicit none
        !Declaración parametros de entrada y salida
        real(rkind8),intent(in) :: A(:,:),B(:)
        real(rkind8),intent(out),dimension(:) :: x

        !Declaración variables de la subrutina
        real(rkind8),allocatable,dimension(:,:) :: A_amp
        real(rkind8) :: m
        integer :: n,i,j

        n = size(A(1,:))
        allocate(A_amp(n,n+1))

        !Creación de la matriz ampliada
        A_amp(:,:n) = A(:,:)
        A_amp(:,n+1) = B(:)

        !Reducción de la matriz mediante el método de Gauss
        do i = 1, n-1
            if ( A_amp(i,i) == 0 ) then
                call permutacion_filas(A_amp,i)
            end if
            do j = i+1,n
                m = A_amp(j,i)/A_amp(i,i) !Múltiplo por el cual se va a restar la fila
                A_amp(j,:) = A_amp(j,:) - m*A_amp(i,:)
            end do
        end do

        !Sustitución hacia atrás
        x(n) = A_amp(n,n+1)/A_amp(n,n)
        do i = n-1, 1,-1
            x(i) = A_amp(i,n+1)
            do j = i+1, n
                x(i) = x(i) - A_amp(i,j)*x(j)
            end do
            x(i) = x(i)/A_amp(i,i)
        end do

        deallocate(A_amp)
    end subroutine eliminacion_gaussiana

    subroutine eliminacion_gaussiana_pivoteo_parcial(A,B,x)
        implicit none
        !Declaración parametros de entrada y salida
        real(rkind8),intent(in) :: A(:,:),B(:)
        real(rkind8),intent(out),dimension(:) :: x

        !Declaración variables de la subrutina
        real(rkind8),allocatable,dimension(:,:) :: A_amp
        real(rkind8) :: m
        integer :: n,i,j

        n = size(A(1,:))
        allocate(A_amp(n,n+1))

        !Creación de la matriz ampliada
        A_amp(:,:n) = A(:,:)
        A_amp(:,n+1) = B(:)

        !Reducción de la matriz mediante el método de Gauss
        do i = 1, n-1
            call filas_mas_grande(A_amp,i) !Pivoteo parcial            
            if ( A_amp(i,i) == 0 ) then
                call permutacion_filas(A_amp,i)
            end if
            do j = i+1,n
                m = A_amp(j,i)/A_amp(i,i) !Múltiplo por el cual se va a restar la fila
                A_amp(j,:) = A_amp(j,:) - m*A_amp(i,:)
            end do
        end do

        !Sustitución hacia atrás
        x(n) = A_amp(n,n+1)/A_amp(n,n)
        do i = n-1, 1,-1
            x(i) = A_amp(i,n+1)
            do j = i+1, n
                x(i) = x(i) - A_amp(i,j)*x(j)
            end do
            x(i) = x(i)/A_amp(i,i)
        end do
        
        deallocate(A_amp)
    end subroutine eliminacion_gaussiana_pivoteo_parcial

    subroutine eliminacion_gauss_jordan(A,B,x)
        implicit none
        !Declaración parametros de entrada y salida
        real(rkind8),intent(in) :: A(:,:),B(:)
        real(rkind8),intent(out),dimension(:) :: x

        !Declaración variables de la subrutina
        real(rkind8),allocatable,dimension(:,:) :: A_amp
        real(rkind8) :: m
        integer :: n,i,j

        n = size(A(1,:))
        allocate(A_amp(n,n+1))

        !Creación de la matriz ampliada
        A_amp(:,:n) = A(:,:)
        A_amp(:,n+1) = B(:)

        !Reducción de la matriz mediante el método de Gauss
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
                A_amp(j,n+1) = A_amp(j,n+1) - m*A_amp(i,n+1)
            end do
        end do

        x = A_amp(:,n+1)
        
        deallocate(A_amp)
    end subroutine eliminacion_gauss_jordan

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

    subroutine metodo_jacobi(A,B,x,tol)
        implicit none
        !Declaración parametros de entrada y salida
        real(rkind8),intent(in) :: A(:,:),B(:),tol
        real(rkind8),intent(inout) :: x(:)

        real(rkind8),allocatable :: x_anterior(:)
        integer :: i,j,z,suma,num_iteraciones=10000

        allocate(x_anterior(size(x)))

        do z = 1, num_iteraciones
            x_anterior = x
            do j = 1, size(A(:,1))
                suma = 0
                do i = 1, size(A(1,:))
                    if ( i /= j ) then
                        suma = suma + x_anterior(i)*A(j,i)    
                    end if                   
                end do
                x(j) = (B(j)-suma)/A(j,j)
            end do
            if (norma(x - x_anterior) < tol) then
                return
            end if
        end do
        print *,"El metodo no convergio"
    end subroutine
    
    subroutine metodo_gauss_seidel(A,B,x,tol)
        implicit none
        !Declaración parametros de entrada y salida
        real(rkind8),intent(in) :: A(:,:),B(:),tol
        real(rkind8),intent(inout) :: x(:)

        real(rkind8),allocatable :: x_anterior(:)
        integer :: i,j,z,suma,num_iteraciones=10000

        allocate(x_anterior(size(x)))

        do z = 1, num_iteraciones
            x_anterior = x
            do j = 1, size(A(:,1))
                suma = 0
                do i = 1, size(A(1,:))
                    if ( i /= j ) then
                        suma = suma + x(i)*A(j,i)    
                    end if                   
                end do
                x(j) = (B(j)-suma)/A(j,j)
            end do
            if (norma(x - x_anterior) < tol) then
                return
            end if
        end do
        print *,"El metodo no convergio"
    end subroutine

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

end module sistemas_ecuaciones_lineales