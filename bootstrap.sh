#!/usr/bin/env bash

set -eu -o pipefail

DOTFILES_DIR=~/Projects/dotfiles

# Ensure dotfiles project exists
cd "$DOTFILES_DIR"

mkdir -p ~/.lein
mkdir -p ~/.ssh
mkdir -p ~/.config/fish
mkdir -p ~/.config/git
mkdir -p ~/.clojure
mkdir -p ~/.pyenv
dotFiles=(".bash_logout"
          ".bash_aliases"
          ".bashrc"
          ".functions.fish"
          ".functions.sh"
          ".gitignore_global"
          ".profile"
          ".tmux.conf"
          ".vimrc"
          ".clojure/deps.edn"
          ".lein/profiles.clj"
          ".ssh/config"
          ".config/fish/config.fish"
          ".config/tidyrc"
          ".config/git/config"
          ".config/git/config-home"
          ".config/git/config-work"
          ".pyenv/default-packages")
for f in ${dotFiles[*]}; do
    ln -svf "$DOTFILES_DIR/$f" "$HOME/$f"
done

# Emacs
mkdir -p ~/.config/emacs
mkdir -p ~/.config/emacs/babel
if [[ ! -d ~/.config/emacs/personal ]]; then
    printf "\nNo ~/.config/emacs/personal directory. Install Prelude!\n"
    exit 1
fi
ln -svf "$DOTFILES_DIR/emacs/init.el" ~/.config/emacs/personal/init.el
ln -svf "$DOTFILES_DIR/emacs/org.el" ~/.config/emacs/personal/org.el
ln -svf "$DOTFILES_DIR/emacs/library-of-babel.org" \
   ~/.config/emacs/babel/library-of-babel.org
ln -svf "$DOTFILES_DIR/emacs/prelude-pinned-packages.el" \
   ~/.config/emacs/prelude-pinned-packages.el

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

# MacOS setup
os=$(uname -s)
if [[ "$os" == "Darwin" ]]; then
   ./macos.sh
fi
