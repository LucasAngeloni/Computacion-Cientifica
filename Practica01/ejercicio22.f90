module diag_dom_utility
contains
    subroutine swap_arr(A,j,k)
        implicit none
        
        integer, intent(in) ::j,k
        integer, dimension(:,:),intent(inout) :: A
        integer :: i,aux

        do i = 1, size(A,1)
            aux = A(j,i)
            A(j,i) = A(k,i)
            A(k,i) = aux
        end do

    end subroutine swap_arr

    subroutine transformacion(A)
        implicit none
        
        integer,intent(inout) :: A(:,:)

        
        
    
    end subroutine transformacion

end module

program ejercicio22
    use diag_dom_utility
    implicit none

    real,dimension(4,4) :: A
    integer :: i,j
    real :: suma = 0

    open(unit=10,file="valores.txt")
    
    read(10,*) A        
    
    close(10)

    print *,A,"-",maxloc(A,dim=2)

    call swap_arr(4,A,1,2)

    do i = 1, 4
        suma = suma + A(1,i)
    end do

    print *,A
    print *,"Suma = ",suma
    
end program ejercicio22