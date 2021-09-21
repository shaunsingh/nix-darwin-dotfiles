#!/bin/bash

# button_table_file format:
# - First line: '<layout_width> <layout_height>'
# - Following lines: '<button_id> <button_target> <hitbox_type> <button_width> <button_height> <button_x> <button_y>'
# Low quality script for internal use only - no error checking...

SCRIPT_DIR=$(dirname $(which $0))

SRC_FILE="$1"
DST_FILE=""

LAYOUT_WIDTH=0
LAYOUT_HEIGHT=0

BUTTON_ID=""
BUTTON_TARGET=""
HITBOX_TYPE=""
BUTTON_WIDTH=0
BUTTON_HEIGHT=0
BUTTON_X=0
BUTTON_Y=0

BUTTON_HALF_WIDTH=0
BUTTON_HALF_HEIGHT=0
BUTTON_X_CENTRE=0
BUTTON_Y_CENTRE=0

POSITION_X=0
POSITION_Y=0
RANGE_X=0
RANGE_Y=0

LAYOUT_DEFINITION=""
declare -a BUTTON_DEFINITIONS
BUTTON_DEFINITION=""

BC_SCALE=20

# Check input
if [[ -z "$SRC_FILE" || ! -f "$SRC_FILE" ]]
then
   echo "Usage: ./parse_layout <button_table_file>"
   exit 1
fi

# Get output file path
DST_FILE="$(basename "$SRC_FILE")"
DST_FILE="${DST_FILE%.*}"
DST_FILE="${SCRIPT_DIR}/${DST_FILE}.out"

rm -f "$DST_FILE"

# Read button table file
LINE_INDEX=0
while read LINE
do

	BUTTON_DEFINITIONS[LINE_INDEX]="$LINE"
	((LINE_INDEX++))
	
done < "$SRC_FILE"

# First line of file is layout dimensions
LAYOUT_DEFINITION="${BUTTON_DEFINITIONS[0]}"

LAYOUT_WIDTH="$(echo "$LAYOUT_DEFINITION" | awk '{print $1}')"
LAYOUT_HEIGHT="$(echo "$LAYOUT_DEFINITION" | awk '{print $2}')"

# Loop over remaining lines
for BUTTON_INDEX in $(seq 1 $((${#BUTTON_DEFINITIONS[@]} - 1)))
do
	# Get current definition string
   BUTTON_DEFINITION="${BUTTON_DEFINITIONS[BUTTON_INDEX]}"

   # If line is empty, output a newline
   if [[ -z "${BUTTON_DEFINITION// }" ]]
   then
      echo "" >> "$DST_FILE"
      continue
   fi

   # Parse definition
   BUTTON_ID="$(echo "$BUTTON_DEFINITION" | awk '{print $1}')"
   BUTTON_TARGET="$(echo "$BUTTON_DEFINITION" | awk '{print $2}')"
   HITBOX_TYPE="$(echo "$BUTTON_DEFINITION" | awk '{print $3}')"
   BUTTON_WIDTH="$(echo "$BUTTON_DEFINITION" | awk '{print $4}')"
   BUTTON_HEIGHT="$(echo "$BUTTON_DEFINITION" | awk '{print $5}')"
   BUTTON_X="$(echo "$BUTTON_DEFINITION" | awk '{print $6}')"
   BUTTON_Y="$(echo "$BUTTON_DEFINITION" | awk '{print $7}')"

   # Get button centre points
   BUTTON_HALF_WIDTH="$(echo "scale=$BC_SCALE; $BUTTON_WIDTH / 2" | bc)"
   BUTTON_HALF_HEIGHT="$(echo "scale=$BC_SCALE; $BUTTON_HEIGHT / 2" | bc)"

   BUTTON_X_CENTRE="$(echo "scale=$BC_SCALE; $BUTTON_X + $BUTTON_HALF_WIDTH" | bc)"
   BUTTON_Y_CENTRE="$(echo "scale=$BC_SCALE; $BUTTON_Y + $BUTTON_HALF_HEIGHT" | bc)"

   # Get normalised button positions
   POSITION_X="$(echo "scale=$BC_SCALE; $BUTTON_X_CENTRE / $LAYOUT_WIDTH" | bc | awk '{printf "%.20f", $0}')"
   POSITION_Y="$(echo "scale=$BC_SCALE; 1 - ($BUTTON_Y_CENTRE / $LAYOUT_HEIGHT)" | bc | awk '{printf "%.20f", $0}')"

   # Get normalised button ranges
   RANGE_X="$(echo "scale=$BC_SCALE; $BUTTON_HALF_WIDTH / $LAYOUT_WIDTH" | bc | awk '{printf "%.20f", $0}')"
   RANGE_Y="$(echo "scale=$BC_SCALE; $BUTTON_HALF_HEIGHT / $LAYOUT_HEIGHT" | bc | awk '{printf "%.20f", $0}')"

   # Generate output
   echo "$BUTTON_ID" >> "$DST_FILE"
   echo "\"$BUTTON_TARGET,$POSITION_X,$POSITION_Y,$HITBOX_TYPE,$RANGE_X,$RANGE_Y\"" >> "$DST_FILE"
   echo "" >> "$DST_FILE"

done
