#!/bin/sh
curl https://adventofcode.com/$1/day/$2 -o ~/$2.html --cookie "session=$3"
