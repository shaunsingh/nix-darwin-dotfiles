#!/bin/bash

# Check if date exists
if ! [ -x "$(command -v date)" ]; then
  echo "{\"error\":\"date binary not found\"}"
  exit 1
fi

# Check if pmset exists
if ! [ -x "$(command -v pmset)" ]; then
  echo "{\"error\":\"pmset binary not found\"}"
  exit 1
fi

# Check if egrep exists
if ! [ -x "$(command -v egrep)" ]; then
  echo "{\"error\":\"egrep binary not found\"}"
  exit 1
fi

# Check if cut exists
if ! [ -x "$(command -v cut)" ]; then
  echo "{\"error\":\"cut binary not found\"}"
  exit 1
fi

# Check if memory_pressure exists
if ! [ -x "$(command -v memory_pressure)" ]; then
  echo "{\"error\":\"memory_pressure binary not found\"}"
  exit 1
fi

# Check if sysctl exists
if ! [ -x "$(command -v sysctl)" ]; then
  echo "{\"error\":\"sysctl binary not found\"}"
  exit 1
fi

# Check if df exists
if ! [ -x "$(command -v df)" ]; then
  echo "{\"error\":\"df binary not found\"}"
  exit 1
fi

# Check if grep exists
if ! [ -x "$(command -v grep)" ]; then
  echo "{\"error\":\"grep binary not found\"}"
  exit 1
fi

# Check if awk exists
if ! [ -x "$(command -v awk)" ]; then
  echo "{\"error\":\"awk binary not found\"}"
  exit 1
fi

# Check if networksetup exists
if ! [ -x "$(command -v networksetup)" ]; then
  echo "{\"error\":\"networksetup binary not found\"}"
  exit 1
fi

export LC_TIME="en_US.UTF-8"
TIME=$(date +"%H:%M")
DATE=$(date +"%a %Y-%m-%d")

BATTERY_PERCENTAGE=$(pmset -g batt | egrep '([0-9]+\%).*' -o --colour=auto | cut -f1 -d'%')

BATTERY_STATUS=$(pmset -g batt | grep "'.*'" | sed "s/'//g" | cut -c 18-19)

BATTERY_CHARGING=""
if [ "$BATTERY_STATUS" == "Ba" ]; then
  BATTERY_CHARGING="false"
elif [ "$BATTERY_STATUS" == "AC" ]; then
  BATTERY_CHARGING="true"
fi

LOAD_AVERAGE=$(sysctl -n vm.loadavg | awk '{print $2}')

MEMORY_FREE=$(memory_pressure | grep "Pages free" | grep -o -E '[0-9]+')
MEMORY_TOTAL=$(memory_pressure | grep system | awk -F" " '{print $5}' | grep -o -E '[0-9]+')

echo $(cat <<-EOF
{
  "datetime": {
    "time": "$TIME",
    "date": "$DATE"
  },
  "battery": {
    "percentage": $BATTERY_PERCENTAGE,
    "charging": $BATTERY_CHARGING
  },
  "cpu": {
    "loadAverage": $LOAD_AVERAGE
  },
	"memory": {
		"total": $MEMORY_TOTAL,
		"free": $MEMORY_FREE
	}
}
EOF
)
