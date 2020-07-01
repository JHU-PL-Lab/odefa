#!/bin/bash

filename="$1"
if [ ! -f "$filename" ]; then
    echo "Provide an .svg file."
    exit 1
fi

convert -density 144 "$filename" -trim "${filename%.*}.png"
