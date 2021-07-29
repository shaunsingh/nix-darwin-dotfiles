#!/bin/bash

set -eu

SYSTEM_TYPE=$(uname -s)

nvim --headless +PackerSync +qa

echo "neovim✅"
