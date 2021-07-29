#!/bin/bash

set -eu

SYSTEM_TYPE=$(uname -s)

mkdir ~/.mbsync
mu init --maildir=~/.mbsync --my-address=shaunsingh0207@gmail.com
mu index
mbsync --all

echo "mu4e ✅"
