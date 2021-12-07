#!/bin/bash
# Using Nix unstable OOTB
# If you want to use nix unstable out of of the box then you can use the following script

# [[file:../nix-config.org::*Using Nix unstable OOTB][Using Nix unstable OOTB:1]]
RELEASE="nix-2.5pre20211019_4a2b7cc"
URL="https://github.com/numtide/nix-unstable-installer/releases/download/$RELEASE/install"

# install using workaround for darwin systems
if [[ $(uname -s) = "Darwin" ]]; then
    FLAG="--darwin-use-unencrypted-nix-store-volume"
fi

[[ ! -z "$1" ]] && URL="$1"

if command -v nix > /dev/null; then
    echo "nix is already installed on this system."
else
    bash <(curl -L $URL) --daemon $FLAG
fi
# Using Nix unstable OOTB:1 ends here
