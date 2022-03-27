#!/usr/bin/env bash

set -eu -o pipefail

DOTFILES_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)"
OS=$(uname -s)

# On MacOS, install homebrew packages. Assumes homebrew is already installed.
if [[ "$OS" == "Darwin" ]]; then
    if ! command -v brew >/dev/null; then
        echo "Homebrew is not installed"
        exit 1
    fi
    brew bundle --no-lock --file Brewfile
fi

# Install the Hasklig font if necessary.
if [[ ! -f ~/Library/Fonts/Hasklig-Regular.ttf ]]; then
    echo "Installing Hasklig font"
    repo="https://github.com/i-tu/Hasklig"
    curl -L \
         $repo/releases/download/v1.2/Hasklig-1.2.zip \
         -o /tmp/hasklig.zip
    tar xvf /tmp/hasklig.zip -C /tmp
    mkdir -p ~/Library/Fonts/
    cp /tmp/TTF/*.ttf ~/Library/Fonts/
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
chmod 600 ~/.ssh/config

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
        # Create the directory for this file if the directory doesn't exist yet
        mkdir -p $d/$(dirname $f)
        # Create symlink to this file in my dotfiles project
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
        # Create the directory for this file if the directory doesn't exist yet
        mkdir -p $d/$(dirname $f)
        # Create symlink to this file in my dotfiles project
        ln -svfn \
           "$DOTFILES_DIR/emacs/cfclrk/$f" \
           "$d/$f"
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
linkEmacsMinimal

# Top-level emacs files, shared among emacsen.
ln -svfn \
   "$DOTFILES_DIR/emacs/projectile-discovery.el" \
   "$HOME/emacs/projectile-discovery.el"

# Clone my website
# git clone
# ln -svfn \
#    "$DOTFILES_DIR/emacs/projectile-discovery.el" \
#    "$HOME/emacs/projectile-discovery.el"
