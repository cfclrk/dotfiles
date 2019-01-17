#!/usr/bin/env bash

set -eu -o pipefail

# Install or update emacs Prelude
# Check if .emacs.d exists and get remote points to prelude.
# If yes:
cd ~/.emacs.d/
git pull

DOTFILES_DIR=~/Projects/dotfiles

# Ensure dotfiles project exists
cd "$DOTFILES_DIR"

# Link to all top-level dot-files files from $HOME
dot_files=(".bash_logout"
           ".bash_aliases"
           ".bashrc"
           ".functions.fish"
           ".functions.sh"
           ".gitconfig"
           ".gitignore_global"
           ".profile"
           ".tmux.conf"
           ".vimrc")
for f in ${dot_files[*]}; do
    ln -svf "$DOTFILES_DIR/$f" "$HOME/$f"
done

# Fish config
ln -svf "$DOTFILES_DIR/.config/fish/config.fish" "$HOME/.config/fish/config.fish"

# Emacs links
ln -svf "$DOTFILES_DIR/emacs/init.el" "$HOME/.emacs.d/personal/init.el"
ln -svf "$DOTFILES_DIR/emacs/org.el" "$HOME/.emacs.d/personal/org.el"

# Ssh config
ln -svf "$DOTFILES_DIR/.ssh/config" "$HOME/.ssh/config"

# Link to all executables in bin/ from $HOME/bin/
script_files=("tmuxinit.sh"
              "ssm")
for f in ${script_files[*]}; do
    ln -svf "$DOTFILES_DIR/bin/$f" "$HOME/bin/$f"
done
