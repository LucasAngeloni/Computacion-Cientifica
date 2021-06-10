program ejercicio12
    implicit none

    integer :: i,j

    do i = 1,50
        print *,i,selected_real_kind(i)
    end do
    
end program ejercicio12