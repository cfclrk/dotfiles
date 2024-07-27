#!/usr/bin/env bash

set -eu -o pipefail

if ! command -v brew >/dev/null; then
    echo "Homebrew is not installed"
    exit 1
fi

# If brew in not installed, install it.
if ! command -v brew >/dev/null; then
    echo "Homebrew is not installed. Installing..."
    bash -c "$(curl -fsSL \
      https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
    brew generate-man-completions
else
    echo "Homebrew is already installed!"
fi

brew bundle --no-lock --file Brewfile

# Make the ~/Projects dir if it's not already there.
mkdir -p ~/Projects
cd ~/Projects

git clone https://github.com/cfclrk/dotfiles.git

# Run the setup.sh script!
pushd ~/Projects/dotfiles
./setup.sh
popd
