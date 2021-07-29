#!/bin/bash

set -eu

SYSTEM_TYPE=$(uname -s)

export STARSHIP_CONFIG=~/.config/starship/starship.toml

echo "starship✅"
