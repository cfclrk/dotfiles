#!/usr/bin/env bash

# If brew in not installed, install it.
if ! command -v brew >/dev/null; then
    echo "Homebrew is not installed. Installing..."
    bash -c "$(curl -fsSL \
      https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
else
    echo "Homebrew is already installed!"
fi

# Install the Homebrew packages defined in Brewfile
brew bundle --file Brewfile
