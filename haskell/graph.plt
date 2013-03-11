#!/usr/bin/gnuplot -persist

#set terminal png 
#set output 'hist-32.png'
#set title "Histogram of factors of orders"
#set xlabel "prime" 
#set ylabel "count" 
#set xrange [0:100]
#plot "hist-32.dat" with lines notitle

#set terminal png 
#set output 'hist-80.png'
#set title "Histogram of factors of orders"
#set xlabel "prime" 
#set ylabel "count" 
#set xrange [0:100]
#plot "hist-80.dat" with lines notitle

#set terminal png 
#set output 'expected-p1.png'
#set title "Expected number of prime forms before split (n = 1 mod 4)"
#set xlabel "nbits" 
#set ylabel "primes" 
#plot "exp1-1.dat" with lines title "1n", \
#     "exp1-2.dat" with lines title "2n", \
#     "exp1-3.dat" with lines title "3n", \
#     "exp1-5.dat" with lines title "5n", \
#     "exp1-6.dat" with lines title "6n", \
#     "exp1-7.dat" with lines title "7n", \
#     "exp1-10.dat" with lines title "10n"

#set terminal png 
#set output 'expected-p3.png'
#set title "Expected number of prime forms before split (n = 3 mod 4)"
#set xlabel "nbits" 
#set ylabel "primes" 
#plot "exp3-1.dat" with lines title "1n", \
#     "exp3-2.dat" with lines title "2n", \
#     "exp3-3.dat" with lines title "3n", \
#     "exp3-5.dat" with lines title "5n", \
#     "exp3-6.dat" with lines title "6n", \
#     "exp3-7.dat" with lines title "7n", \
#     "exp3-10.dat" with lines title "10n"

set terminal png
set output 'second-factor-n1.png'
set title "Second largest factor when n=1 (mod 4)"
set ylabel "prime"
plot "plot-1-1.dat" with lines title "1p", \
     "plot-1-2.dat" with lines title "2p", \
     "plot-1-3.dat" with lines title "3p", \
     "plot-1-5.dat" with lines title "5p", \
     "plot-1-6.dat" with lines title "6p", \
     "plot-1-7.dat" with lines title "7p", \
     "plot-1-10.dat" with lines title "10p"

set terminal png
set output 'second-factor-n3.png'
set title "Second largest factor when n=3 (mod 4)"
set ylabel "prime"
plot "plot-3-1.dat" with lines title "1p", \
     "plot-3-2.dat" with lines title "2p", \
     "plot-3-3.dat" with lines title "3p", \
     "plot-3-5.dat" with lines title "5p", \
     "plot-3-6.dat" with lines title "6p", \
     "plot-3-7.dat" with lines title "7p", \
     "plot-3-10.dat" with lines title "10p"
     

