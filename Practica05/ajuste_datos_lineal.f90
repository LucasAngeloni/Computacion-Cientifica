module aj_dat_lin
    use kinds
    use my_funcs
    use sistemas_ecuaciones_lineales
    implicit none

    contains

    subroutine minimos_cuadrados(datos,param_ajuste,err)
        implicit none
        real(rkind8),intent(in) :: datos(:,:)
        real(rkind8),intent(out) ::  param_ajuste(n_param),err

        real(rkind8) :: Z(size(datos,1),n_param),Y(size(datos,1)),ZT(n_param,size(datos,1))
        real(rkind8) :: ZTZ(n_param,n_param),ZTZ_Inv(n_param,n_param)
        integer :: i

        Y = datos(:,2)

        do i = 1,size(datos,1)
            Z(i,:) = f(datos(i,1))
        end do

        do i = 1,n_param
            ZT(i,:) = Z(:,i)
        end do

        ZTZ = matmul(ZT,Z) !Matmul: multiplicación matricial
        
        call matriz_inversa(ZTZ,ZTZ_Inv)

        param_ajuste = matmul(matmul(ZTZ_Inv,ZT),Y)
        err = error(datos(:,1),datos(:,2),param_ajuste)
    
    end subroutine minimos_cuadrados

    subroutine minimos_cuadrados_nolineal(x,Y,param_ajuste,err,iter_max,tol)
        implicit none
        real(rkind8),intent(in) :: x(:),Y(:),tol
        integer, intent(in) :: iter_max
        real(rkind8),intent(out) ::  param_ajuste(n_param),err

        real(rkind8) :: Z(size(x),n_param),ZT(n_param,size(x)),D(size(x)),D_nuevo(size(x)) !datos,1 representa los valores de x
        real(rkind8) :: ZTZ(n_param,n_param),ZTZ_Inv(n_param,n_param),delta_a(n_param)
        integer :: i,j

        do j = 1,iter_max
            !Calculo de la matriz de derivadas parciales
            do i = 1, size(x)
                Z(i,:) = derivf(x(i),param_ajuste)
            end do
    
            !Calculo traspuesta de Z
            do i = 1, n_param
                ZT(i,:) = Z(:,i)
            end do
    
            ZTZ = matmul(ZT,Z)
            call matriz_inversa(ZTZ,ZTZ_Inv)
    
            !Calculo de errores en cada punto
            do i = 1, size(x)
                D(i) = Y(i) - Est(x(i),param_ajuste)
            end do
    
            delta_a = matmul(matmul(ZTZ_Inv,ZT),D)
    
            param_ajuste = param_ajuste + delta_a

            !Calculo de errores en cada punto con los nuevos parametros de ajuste
            do i = 1, size(x)
                D_nuevo(i) = Y(i) - Est(x(i),param_ajuste)
            end do
    
            err = error_met_NL(D_nuevo)
            if(abs(err-error_met_NL(D))/err <= tol) then
                print *,j
                return
            end if
        end do
        print *,"Se alcanzo el numero de iteraciones maximo"
    
    end subroutine minimos_cuadrados_nolineal

    function error_met_NL(diferencias) result(resultado)
        implicit none
        real(rkind8) :: diferencias(:),resultado
        integer :: i

        resultado = 0
        do i = 1, size(diferencias)
            resultado = resultado + diferencias(i)**2
        end do

    end function

    function error(x,y,c)
        implicit none
        real(rkind8),intent(in) :: x(:),y(:),c(:)
        real(rkind8) :: error

        integer :: i,j
        real(rkind8) :: valores_f(n_param),pred

        error = 0
        do i = 1, size(y)
            valores_f = f(x(i))
            pred = 0
            do j = 1,size(c)
                pred = pred + c(j)*valores_f(j)
            end do

            error = error + (y(i) - pred)**2
        end do
        
    end function

    function interpolacion_Lagrange(datos,x) result(P)
        implicit none
        real(rkind8) :: datos(:,:),x
        real(rkind8) :: P,L
        integer :: k,j

        P = 0
        do k = 1, size(datos,1)
            L = 1
            do j = 1,size(datos,1)
                if ( k /= j) then
                    L = L*(x-datos(j,1))/(datos(k,1)-datos(j,1))
                end if
            end do
            P = P + datos(k,2)*L           
        end do

    end function

    function spline_lineal(x,y,p) result(f)
        implicit none
        real(rkind8) :: x(:),y(:),p,f,m
        integer :: i

        if ( p < x(1) .or. p > x(size(x)) ) then
            print *,"El punto a buscar esta fuera del intervalo permitido"
            stop
        end if
        do i = 1, size(x)
            if (p >= x(i) .and. p <= x(i+1)) then
                m = (y(i+1) - y(i))/(x(i+1) - x(i))
                f = y(i) + m*(p - x(i))
                return 
            end if
        end do
    end function

    function spline_cuadratico(x,y,p,a) result(f)
        implicit none
        real(rkind8) :: x(:),y(:),p,f,deriv,m
        real(rkind8) :: a(:)
        integer :: i

        if ( p < x(1) .or. p > x(size(x)) ) then
            print *,"El punto a buscar esta fuera del intervalo permitido"
            stop
        end if

        do i = 1,size(x)-1
            if ( p >= x(i) .and. p <= x(i+1) ) then
                m = (y(i+1) - y(i))/(x(i+1) - x(i))
                f = y(i) + m*(p - x(i)) + a(i)*(p - x(i))*(p - x(i+1))
                return 
            end if
        end do
    end function
end module