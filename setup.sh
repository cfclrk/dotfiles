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

# SSH config
mkdir -p ~/.ssh
ln -svfn \
   "$DOTFILES_DIR/.ssh/config" \
   ~/.ssh/config
chmod 600 ~/.ssh/config

# SSH keys (stored in 1Password)
for key in codeberg github-home github-work gitlab; do
    if [[ ! -f ~/.ssh/$key ]]; then
        echo "SSH key $key not found. Fetching from 1Password..."
        op read "op://Personal/$key SSH key/private key?ssh-format=openssh" > ~/.ssh/$key
        chmod 600 ~/.ssh/$key
    else
        echo "SSH key $key already present."
    fi
    if [[ ! -f ~/.ssh/$key.pub ]]; then
        echo "SSH public key $key.pub not found. Fetching from 1Password..."
        op read "op://Personal/$key SSH key/public key" > ~/.ssh/$key.pub
        chmod 644 ~/.ssh/$key.pub
    else
        echo "SSH public key $key.pub already present."
    fi
done

# GPG config
mkdir -p ~/.gnupg
ln -svfn \
   "$DOTFILES_DIR/.gnupg/gpg-agent.conf" \
   ~/.gnupg/gpg-agent.conf
chmod 700 ~/.gnupg

# GPG signing key (stored in 1Password)
GPG_SIGNING_KEY="7D9725559B0BC823"
if ! gpg --list-secret-keys --keyid-format=long 2>/dev/null | grep -q "$GPG_SIGNING_KEY"; then
    echo "GPG signing key not found. Importing from 1Password..."
    op item get "GPG Private Key (cfclrk)" --fields notesPlain | gpg --import
fi

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

# Global NPM packages
npm_packages=(
    @agentclientprotocol/claude-agent-acp
    intelephense
)

printf "\nInstalling global npm packages"
for package in "${npm_packages[@]}"; do
    printf "\nInstalling %s\n" "$package"
    npm install -g "$package"
done

# Intelephense licence key (stored in 1Password)
if [[ ! -f ~/intelephense/licence.txt ]]; then
    echo "Intelephense licence not found. Fetching from 1Password..."
    mkdir -p ~/intelephense
    op item get "Intelephense License" --fields notesPlain > ~/intelephense/licence.txt
fi

# Fish shell
fish_path=$(which fish 2>/dev/null || true)
if [[ -n "$fish_path" ]]; then
    if ! grep -qF "$fish_path" /etc/shells; then
        echo "Adding $fish_path to /etc/shells"
        echo "$fish_path" | sudo tee -a /etc/shells > /dev/null
    fi
fi

# Projects
mkdir -p ~/Projects
if [[ ! -d ~/Projects/cfclrk.github.io ]]; then
    echo "Cloning cfclrk.github.io into ~/Projects/"
    git clone git@github.com:cfclrk/cfclrk.github.io.git ~/Projects/cfclrk.github.io
fi
ln -svfn ~/Projects/cfclrk.github.io/notes/org ~/notes

# Update dotfiles remote from HTTPS to SSH if needed
dotfiles_remote=$(git -C "$DOTFILES_DIR" remote get-url origin)
if [[ "$dotfiles_remote" == "https://github.com/cfclrk/dotfiles.git" ]]; then
    echo "Updating dotfiles remote from HTTPS to SSH..."
    git -C "$DOTFILES_DIR" remote set-url origin git@github.com:cfclrk/dotfiles.git
fi

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
