#!/usr/bin/gnuplot -persist

set terminal png
set output 'prime.png'
set title "Primes"
set xlabel "bits"
set ylabel "Primes"
plot "prime-avg.dat" with lines title "avg", \
     "prime-sd.dat" with lines title "sd"

set terminal png
set output 'steps.png'
set title "Steps"
set xlabel "bits"
set ylabel "Steps"
plot "steps-avg.dat" with lines title "avg", \
     "steps-sd.dat" with lines title "sd"

set terminal png
set output 'prime-zoom1.png'
set title "Primes"
set xlabel "bits"
set ylabel "Primes"
set xrange [32:56]
plot "prime-avg.dat" with lines title "avg", \
     "prime-sd.dat" with lines title "sd"

set terminal png
set output 'steps-zoom1.png'
set title "Steps"
set xlabel "bits"
set ylabel "Steps"
set xrange [32:56]
plot "steps-avg.dat" with lines title "avg", \
     "steps-sd.dat" with lines title "sd"

set terminal png
set output 'prime-zoom2.png'
set title "Primes"
set xlabel "bits"
set ylabel "Primes"
set xrange [56:80]
plot "prime-avg.dat" with lines title "avg", \
     "prime-sd.dat" with lines title "sd"

set terminal png
set output 'steps-zoom2.png'
set title "Steps"
set xlabel "bits"
set ylabel "Steps"
set xrange [56:80]
plot "steps-avg.dat" with lines title "avg", \
     "steps-sd.dat" with lines title "sd"


     

