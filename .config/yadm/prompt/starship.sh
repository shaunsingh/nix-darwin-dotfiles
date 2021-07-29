#!/bin/bash

set -eu

brew install bat exa
export STARSHIP_CONFIG=~/.config/starship/starship.toml

echo "starship ✅"
