#!/usr/bin/env bash

set -eu -o pipefail

brew install git --force
brew install findutils

# Make the ~/Projects dir if it's not already there.
mkdir -p ~/Projects
cd ~/Projects

git clone https://github.com/cfclrk/dotfiles.git

export PATH=/usr/local/opt/findutils/libexec/gnubin:$PATH

# Run the setup.sh script!
pushd ~/Projects/dotfiles
./setup.sh
popd
