program cycle
    implicit none

    integer :: i
    do i = 1, 20

        if(i*2 > 20) then
            cycle
            print *,"cycle"
        end if
        print *,i
    end do
    
end program cycle