program ej2
    use aj_dat_lin
    use my_funcs
    implicit none

    real(rkind8) :: datos(4,2),a(n_param),e

    open(unit=1,file="ej2.dat")
    read(1,*) datos(:,1)
    read(1,*) datos(:,2)
    close(1)

    call minimos_cuadrados(datos,a,e)
    
    print *,"Valores parametros:",a
    print *,"Error: ",e
end program ej2