#!/usr/bin/env bash

set -e -o pipefail

# The absolute path to this directory
DOTFILES_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)"

# Dotfiles
dotFiles=$(ls -A "$DOTFILES_DIR/dotfiles")
for f in $dotFiles; do
    ln -svfn \
       "$DOTFILES_DIR/dotfiles/$f" \
       "$HOME/$f"
done

# Now we have a ~/.profile. Source it to update $PATH.
. ~/.profile

# ~/bin
mkdir -p ~/bin
scriptFiles=$(ls -A "$DOTFILES_DIR/bin")
for f in $scriptFiles; do
    ln -svfn \
       "$DOTFILES_DIR/bin/$f" \
       "$HOME/bin/$f"
done

# XDG ~/.config directory
mkdir -p ~/.config
xdgConfigs=$(ls -A "$DOTFILES_DIR/.config")
for f in $xdgConfigs; do
    ln -svfn \
       "$DOTFILES_DIR/.config/$f" \
       "$HOME/.config/$f"
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

# Link the contents of a directory in the dotfiles repo in ~/emacs.
#
# Parameters:
#
# - `dirName`: Name of a directory in the emacs/ directory of this project
#
# Example:
#
#     linkEmacs "min-package"
function linkEmacs {
    [[ -n "$1" ]] || {
        echo "Error: param 1 is required"
        return 1
    }
    dirName=$1
    dotfilesEmacsDir="$DOTFILES_DIR/emacs/$dirName"
    realEmacsDir=~/emacs/$dirName

    # Create ~/emacs/<dirName>
    mkdir -p "$realEmacsDir"

    # Create symlinks in $realEmacsDir to every file in $dotfilesEmacsDir
    files=$(find $dotfilesEmacsDir -type f -printf "%P\n")
    for f in $files; do
        # Create the subdirectory for this file if it doesn't exist
        mkdir -p $realEmacsDir/$(dirname $f)
        # Create symlink to this file in my dotfiles project
        ln -svfn \
           "$dotfilesEmacsDir/$f" \
           "$realEmacsDir/$f"
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
emacsDirs=$(find ./emacs -maxdepth 1 -type d -printf "%P\n")
for d in $emacsDirs; do
    linkEmacs "$d"
done

# Top-level emacs files, shared among emacsen.
emacsFiles=$(find ./emacs -maxdepth 1 -type f -printf "%P\n")
for f in $emacsFiles; do
    ln -svfn \
       "$DOTFILES_DIR/emacs/$f" \
       "$HOME/emacs/$f"
done
