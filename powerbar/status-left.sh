#!/bin/bash

command -v /opt/homebrew/Cellar/yabai/3.3.10/bin/yabai > /dev/null 2>&1 || { echo "{\"error\":\"yabai binary not found\"}"; exit 1; }

DESKTOPS_PRIMARY=$(/opt/homebrew/Cellar/yabai/3.3.10/bin/yabai -m query --spaces --display 1)

echo $(cat <<-EOF
{
	"desktops_primary": $DESKTOPS_PRIMARY
}
EOF
)
