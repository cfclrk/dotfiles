#!/usr/bin/env bash

set -eu -o pipefail

DOTFILES_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)"

# On MacOS, install Homebrew and all brew packages in the Brewfile
os=$(uname -s)
if [[ "$os" == "Darwin" ]]; then
    ./homebrew.sh
fi

# Dotfiles
dotFiles=$(ls -A "$DOTFILES_DIR/dotfiles")
for f in $dotFiles; do
    ln -svfn "$DOTFILES_DIR/dotfiles/$f" "$HOME/$f"
done

# ~/bin
mkdir -p ~/bin
scriptFiles=$(ls -A "$DOTFILES_DIR/bin")
for f in $scriptFiles; do
    ln -svfn "$DOTFILES_DIR/bin/$f" "$HOME/bin/$f"
done

# XDG config: ~/.config
mkdir -p ~/.config
xdgConfigs=$(ls -A "$DOTFILES_DIR/xdg_config")
for f in $xdgConfigs; do
    ln -svfn "$DOTFILES_DIR/xdg_config/$f" "$HOME/.config/$f"
done

# SSH
mkdir -p ~/.ssh
ln -svfn \
   "$DOTFILES_DIR/.ssh/config" \
   ~/.ssh/config

# Tmux
if [[ ! -d ~/.tmux/plugins/tpm ]]; then
    echo "Tmux package manager is not installed. Installing!"
    mkdir -p ~/.tmux/plugins
    git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
fi

# Pyenv
mkdir -p ~/.pyenv
ln -svf \
   "$DOTFILES_DIR/.pyenv/default_packages" \
   ~/.pyenv/default-packages

# Emacs
# -----------------------------------------------------------------------------

function linkEmacsMinimal {
    d=~/emacs/minimal
    mkdir -p $d
    files=$(find "$DOTFILES_DIR/emacs/minimal" -type f -printf "%P\n")
    for f in $files; do
        ln -svfn \
           "$DOTFILES_DIR/emacs/minimal/$f" \
           "$d/$f"
    done
}

function linkEmacsCfclrk {
    d=~/emacs/cfclrk
    mkdir -p $d
    files=$(find "$DOTFILES_DIR/emacs/cfclrk" -type f -printf "%P\n")
    for f in $files; do
        ln -svfn \
           "$DOTFILES_DIR/emacs/cfclrk/$f" \
           "$d/$f"
    done
}

function linkEmacsPrelude {
    d=~/emacs/prelude
    files=$(find "$DOTFILES_DIR/emacs/prelude" -type f -printf "%P\n")
    for f in $files; do
        ln -svfn \
           "$DOTFILES_DIR/emacs/prelude/$f" \
           "$d/personal/$f"
    done
}

# Set up chemacs2, installing it if necessary.
if [[ -d ~/.emacs.d ]]; then
    pushd ~/.emacs.d
    remote=$(basename $(git remote get-url origin))
    if [[ ! $remote == "chemacs2.git" ]]; then
        echo "$HOME/.emacs.d exists but it is not chemacs2"
        exit 1
    fi
    echo "Updating chemacs2"
    git pull
    popd
else
    echo "Cloning chemacs2 into $HOME/.emacs.d/"
    git clone https://github.com/plexus/chemacs2.git ~/.emacs.d
fi

# Create Emacs config directories
mkdir -p ~/emacs
linkEmacsCfclrk
linkEmacsPrelude
linkEmacsMinimal

# Install Prelude if necessary
if [[ ! -d $d ]]; then
    printf "\nEmacs Prelude not found. Installing Prelude...\n"
    export PRELUDE_INSTALL_DIR="$d"
    curl -L https://git.io/epre | sh
fi
