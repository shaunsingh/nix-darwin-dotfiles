#!/bin/bash

set -eu

rm -rf ~/.mbsync
mkdir ~/.mbsync
nvim .mbsyncrc
nvim .msmtprc
mu init --maildir=~/.mbsync --my-address=shaunsingh0207@gmail.com
mu index
mbsync --all

echo "mu4e ✅"
