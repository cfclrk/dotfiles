#!/usr/bin/env bash

# Computer setup stuff that is specific to MacOS.

# If brew in not installed, install it.
if ! command -v brew >/dev/null; then
    echo "Installing Homebrew"
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
else
    echo "Homebrew is already installed"
fi

# A small list of homebrew packages that I need for basic computer usage
brew bundle --file Brewfile
brew man
