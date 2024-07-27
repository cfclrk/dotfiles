#!/bin/bash

set -eu -o pipefail

# If brew in not installed, install it.
if ! command -v brew >/dev/null; then
    echo "Homebrew is not installed. Installing..."
    bash -c "$(curl -fsSL \
      https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
    brew generate-man-completions
else
    echo "Homebrew is already installed!"
fi

# Make the ~/Projects dir if it's not already there.
mkdir -p ~/Projects
cd ~/Projects

git clone https://github.com/cfclrk/dotfiles.git

brew bundle --no-lock --file Brewfile

# Run the setup.sh script!
pushd ~/Projects/dotfiles
./setup.sh
popd
