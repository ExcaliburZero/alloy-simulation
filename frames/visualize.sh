#!/bin/bash

rm *.png

for ((i=0;i<=100;i++)); 
do 
    gnuplot -e "iteration='$i'" cube.gnu
    echo $i
done
