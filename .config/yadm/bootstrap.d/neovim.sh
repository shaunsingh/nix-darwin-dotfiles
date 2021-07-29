#!/bin/bash

set -eu

nvim --headless +PackerSync +qa

echo "neovim ✅"
