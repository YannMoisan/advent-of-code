#!/bin/sh

# Check if the required arguments are provided
if [ "$#" -ne 3 ]; then
    echo "Usage: $0 <session_cookie> <year> <day>"
    exit 1
fi

# Define the URL and output file
url="https://adventofcode.com/$2/day/$3/input"
output_file="$2/src/main/resources/input$3"

# Use curl to download the input file
curl "$url" -o "$output_file" --cookie "session=$1"
