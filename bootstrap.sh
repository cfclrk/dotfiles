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

brew update
brew upgrade

mkdir -p ~/Projects
cd ~/Projects

# This will clone using HTTPS since we don't have SSH set up yet
git clone https://github.com/cfclrk/dotfiles.git

cd ~/Projects/dotfiles
brew bundle install --verbose

./setup.sh
