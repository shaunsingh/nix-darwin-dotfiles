#!/bin/bash

SCRIPT_DIR=$(dirname $(which $0))

SRC_DIR="${SCRIPT_DIR}"
DST_DIR="${SCRIPT_DIR}/../../png/$(basename "$SCRIPT_DIR")"

PNG_FILE=""

mkdir -p "$DST_DIR"
cd "$SRC_DIR"

for SVG_FILE in *.svg
do

   # Get output PNG file path
   PNG_FILE="$(basename "$SVG_FILE")"
   PNG_FILE="${PNG_FILE%.*}"
   PNG_FILE="${DST_DIR}/${PNG_FILE}.png"

   # Convert from SVG to PNG
   inkscape --export-area-page --export-dpi=96 --export-png="$PNG_FILE" "$SVG_FILE"
   # Set colour of transparent pixels to full white
   magick mogrify -background 'rgb(255,255,255)' -alpha Background "$PNG_FILE"
   # Optimise generated file
   optipng -o7 -strip all "$PNG_FILE"

done
