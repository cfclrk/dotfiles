#!/usr/bin/env bash

set -eu -o pipefail

DOTFILES_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)"

function linkEmacsMinimal {
    d=~/emacs/minimal
    mkdir -p $d
    files=$(find "$DOTFILES_DIR/emacs_minimal" -type f -printf "%P\n")
    for f in $files; do
        ln -svfh \
           "$DOTFILES_DIR/emacs_minimal/$f" \
           "$d/$f"
    done
}

function linkEmacsCfclrk {
    d=~/emacs/cfclrk
    mkdir -p $d
    files=$(find "$DOTFILES_DIR/emacs_cfclrk" -type f -printf "%P\n")
    for f in $files; do
        ln -svfh \
           "$DOTFILES_DIR/emacs_cfclrk/$f" \
           "$d/$f"
    done
}

function linkEmacsPrelude {
    d=~/emacs/prelude

    # Install Prelude if necessary
    if [[ ! -d $d ]]; then
        printf "\nEmacs Prelude not found. Installing Prelude...\n"
        export PRELUDE_INSTALL_DIR="$d"
        curl -L https://git.io/epre | sh
    fi

    files=$(find "$DOTFILES_DIR/emacs_prelude" -type f -printf "%P\n")
    for f in $files; do
        ln -svfh \
           "$DOTFILES_DIR/emacs_prelude/$f" \
           "$d/personal/$f"
    done
}

# On MacOS, install Homebrew and all brew packages in the Brewfile
os=$(uname -s)
if [[ "$os" == "Darwin" ]]; then
    ./homebrew.sh
fi

# Dotfiles
dotFiles=$(ls -A "$DOTFILES_DIR/dotfiles")
for f in $dotFiles; do
    ln -svfh "$DOTFILES_DIR/dotfiles/$f" "$HOME/$f"
done

# ~/bin
mkdir -p ~/bin
scriptFiles=$(ls -A "$DOTFILES_DIR/bin")
for f in $scriptFiles; do
    ln -svf "$DOTFILES_DIR/bin/$f" "$HOME/bin/$f"
done

# XDG config: ~/.config
mkdir -p ~/.config
xdgConfigs=$(ls -A "$DOTFILES_DIR/xdg_config")
for f in $xdgConfigs; do
    ln -svfh "$DOTFILES_DIR/xdg_config/$f" "$HOME/.config/$f"
done

# SSH
mkdir -p ~/.ssh
ln -svfh \
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

# Set up chemacs2
if [[ -d ~/.emacs.d ]]; then
    pushd ~/.emacs.d
    remote=$(git remote get-url origin)
    if [[ ! $remote == "git@github-home:plexus/chemacs2.git" ]]; then
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

# Emacs
mkdir -p ~/emacs
linkEmacsCfclrk
linkEmacsPrelude
linkEmacsMinimal

