set terminal png

set output 'attempts-n1.png'
plot 'attempts.dat' using 1:2 with lines title '1n', \
     '' using 1:4 with lines title '2n', \
     '' using 1:6 with lines title '3n', \
     '' using 1:8 with lines title '5n', \
     '' using 1:10 with lines title '6n', \
     '' using 1:12 with lines title '7n', \
     '' using 1:14 with lines title '10n'

set output 'attempts-n3.png'
plot 'attempts.dat' using 1:3 with lines title '1n', \
     '' using 1:5 with lines title '2n', \
     '' using 1:7 with lines title '3n', \
     '' using 1:9 with lines title '5n', \
     '' using 1:11 with lines title '6n', \
     '' using 1:13 with lines title '7n', \
     '' using 1:15 with lines title '10n'

set output 'difference.png'
plot 'difference.dat' using 1:2 with lines
