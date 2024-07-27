#!/usr/bin/env bash

set -eu -o pipefail

if ! command -v brew >/dev/null; then
    echo "Homebrew is not installed"
    exit 1
fi

brew install git --force --overwrite
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
