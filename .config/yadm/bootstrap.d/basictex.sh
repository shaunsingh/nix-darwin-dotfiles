#!/bin/bash

set -eu

SYSTEM_TYPE=$(uname -s)

sudo tlmgr update --self
sudo tlmgr install dvipng dvisvgm l3packages xcolor soul adjustbox collectbox amsmath siunitx cancel mathalpha capt-of chemfig wrapfig mhchem


echo "LaTeX ✅"
