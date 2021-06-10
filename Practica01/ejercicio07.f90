program ejercicio07
    implicit none
    integer :: n1,n2,result

    write(*,*)"Ingresar 2 valores"
    read(*,*)n1,n2

    call divisibles(n1,n2,result)

    if(result == 1) then
        print *,"Los numeros son divisibles"
    else
        print *,"Los numeros no son divisibles"
    endif
    
end program ejercicio07

subroutine divisibles(n1,n2,resultado)
    implicit none
    integer :: n1,n2
    integer,intent(out) :: resultado

    resultado = 0
    if(n1>=n2) then
        if(n1/n2*n2 == n1) then
            resultado = 1
        endif
    else
        if(n2/n1*n1 == n2) then
            resultado = 1
        endif
    endif

end subroutine divisibles