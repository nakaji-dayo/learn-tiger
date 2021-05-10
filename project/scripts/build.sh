#!/bin/bash

set -eu

input=$1
assem=${input%.*}.s
output=${input%.*}.o

stack build
stack install
tigerc-exe $input $assem > build.log
arm-none-eabi-gcc -w $assem runtime.c -o $output
qemu-arm $output
