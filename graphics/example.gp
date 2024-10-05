reset 

set grid

set xlabel "x_1"
set ylabel "x_2"

f(x, y) = 0.5*x*x + y*y -x*y -2*x - 6*y 

h1(x, y) = - x - y + 2
h2(x, y) = - x + 2*y - 2
h3(x, y) = x + 3*y + 3

set object 1 polygon from 2.0/3.0, 4.0/3.0, 0.0 to -2.4, -0.2, 0 to 4.5, -2.5, 0.0 to 2.0/3.0, 4.0/3.0, 0.0 \
    fillstyle transparent solid 0.3 fc "yellow"

set xrange [-3.5:5.5]
set yrange [-3:3]

set cntrparam levels discrete 0
set contour base
set isosamples 1000, 1000
set key noautotitle
unset clabel

set view map
splot "../data/state.dat" using ($1-$2):($3-$4):(0) with linespoints title "QP example" lw 2, \
      h1(x, y) nosurface with lines title "x + y - 2 = 0" linecolor "red" lw 2, \
      h2(x, y) nosurface with lines title "x - 2y + 2 = 0" linecolor "blue" lw 2, \
      h3(x, y) nosurface with lines title "x + 3y + 3 = 0" linecolor "green" lw 2, \
      f(x, y) nosurface with lines notitle linecolor "gray", \
      f(x, y)+5 nosurface with lines notitle linecolor "gray", \
      f(x, y)+10 nosurface with lines notitle linecolor "gray", \
      f(x, y)+15 nosurface with lines notitle linecolor "gray", \
      f(x, y)+20 nosurface with lines notitle linecolor "gray", \
      f(x, y)-5 nosurface with lines notitle linecolor "gray", \
      f(x, y)-10 nosurface with lines notitle linecolor "gray", \
      f(x, y)-15 nosurface with lines notitle linecolor "gray", \
      f(x, y)-20 nosurface with lines notitle linecolor "gray", \
      f(x, y)-25 nosurface with lines notitle linecolor "gray", \
      f(x, y)-30 nosurface with lines notitle linecolor "gray", \
      f(x, y)-35 nosurface with lines notitle linecolor "gray", \
      f(x, y)-40 nosurface with lines notitle linecolor "gray"

