program pruebas_plot
    implicit none

    call system("gnuplot -p grafica.plt")
    write(*,*) "Ejecucion finalizada"
    write(*,*) "Se grafico el histograma en histograma.pdf"
    
end program pruebas_plot