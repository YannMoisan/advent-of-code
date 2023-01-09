#!/bin/sh
for i in $(seq 1 25)
do
	curl https://adventofcode.com/$1/day/$i/input -o ~/projects/perso/advent-of-code/$1/src/main/resources/input$i --cookie "session=$2"
done
