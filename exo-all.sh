#!/bin/sh
for i in $(seq 1 25)
do
	curl https://adventofcode.com/$1/day/$i -o ~/projects/perso/advent-of-code/${i}.html --cookie "session=$2"
done
seq 1 25 | xargs -I {} cat ~/projects/perso/advent-of-code/{}.html | ~/.cargo/bin/htmlq main | pandoc -f html -o ${1}.pdf
