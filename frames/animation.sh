#!/bin/bash

rename 's/\d+/sprintf("%05d", $&)/e' *.png
convert -delay 1x12 -loop 0 *.png cube.gif
