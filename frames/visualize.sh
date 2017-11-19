#!/bin/bash

for ((i=1;i<=100;i++)); 
do 
    gnuplot -e "iteration='$i'" cube.gnu
    echo $i
done
