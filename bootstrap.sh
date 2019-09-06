#!/usr/bin/env bash

set -eu -o pipefail

DOTFILES_DIR=~/Projects/dotfiles

# Ensure dotfiles project exists
cd "$DOTFILES_DIR"

mkdir -p ~/.lein
mkdir -p ~/.ssh
mkdir -p ~/.config/fish
mkdir -p ~/.clojure
mkdir -p ~/.pyenv
dotFiles=(".bash_logout"
          ".bash_aliases"
          ".bashrc"
          ".functions.fish"
          ".functions.sh"
          ".gitconfig"
          ".gitconfig-personal"
          ".gitconfig-ironnet"
          ".gitignore_global"
          ".profile"
          ".tmux.conf"
          ".vimrc"
          ".clojure/deps.edn"
          ".lein/profiles.clj"
          ".ssh/config"
          ".config/fish/config.fish"
          ".config/tidyrc"
          ".pyenv/default-packages")
for f in ${dotFiles[*]}; do
    ln -svf "$DOTFILES_DIR/$f" "$HOME/$f"
done

# Emacs
if [[ ! -d ~/.emacs.d/personal ]]; then
    printf "\nNo ~/.emacs.d/personal directory. Install Prelude!\n"
    exit 1
fi
ln -svf "$DOTFILES_DIR/emacs/init.el" ~/.emacs.d/personal/init.el
ln -svf "$DOTFILES_DIR/emacs/org.el" ~/.emacs.d/personal/org.el

# ~/bin
mkdir -p ~/bin
scriptFiles=$(ls $DOTFILES_DIR/bin)
for f in $scriptFiles; do
    ln -svf "$DOTFILES_DIR/bin/$f" "$HOME/bin/$f"
done

# tmux
if [[ ! -d ~/.tmux/plugins/tpm ]]; then
    echo "Tmux package manager is not installed. Installing!"
    mkdir -p ~/.tmux/plugins
    git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
fi
