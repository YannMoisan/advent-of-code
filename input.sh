#!/bin/sh
curl https://adventofcode.com/$1/day/$2/input -o ~/input$2 --cookie "session=$3"
