set terminal png
set output "grafica.png"

plot "spline_cuadratico.dat", 1.7632 - 0.97286*x + 0.19933*x**2