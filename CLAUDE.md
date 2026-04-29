# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is a personal dotfiles repository that manages shell configurations, Emacs setups, and development tools for macOS. The repository uses a symlink-based approach to deploy configurations from this central location to the home directory.

Most of the repository is dedicated to Emacs configuration.

## Repository Architecture

1. **dotfiles/** - Shell and tool configurations that symlink to `~/`

   - `.zshrc`, `.bashrc`, `.profile`, `.zprofile` - Shell startup files
   - `.functions.sh`, `.functions.fish` - Custom shell functions
   - `.tmux.conf` - Tmux configuration
   - `.emacs-profiles.el` - Chemacs2 profile selector

2. **emacs/** - Multiple Emacs configurations using Chemacs2

   - `cfclrk/` - Primary Emacs configuration using Elpaca package manager
   - `min-*` directories - Minimal test configurations for specific features
   - Chemacs2 allows switching between profiles with `emacs --with-profile <name>`

3. **bin/** - Utility scripts that symlink to `~/bin`

   - `use` - Script for managing environment switching
   - `ssm` - AWS Systems Manager helper
   - `delete_buckets` - AWS S3 bucket cleanup utility

4. **.config/** - XDG configuration directory

   - `clojure/` - Clojure CLI tools configuration
   - `clj-kondo/` - Clojure linter configuration
   - `git/` - Git configuration files

### Working with the Primary Emacs Config (cfclrk)

The main Emacs configuration (`emacs/cfclrk/`) uses:

- **Elpaca** as the package manager (not package.el or straight.el)
- `init-elpaca.el` - Elpaca bootstrap code
- `init-functions.el` - Custom elisp functions
- `init.el` - Main configuration entry point

When making changes to Emacs configuration, edit files in `emacs/cfclrk/` (they are symlinked to `~/emacs/cfclrk/`).

## Shell Configuration

### Fish Shell (Primary Shell)

Fish configuration:

- `dotfiles/.functions.fish` - Fish-specific functions

### Zsh

The zsh configuration uses oh-my-zsh. Key files:

- `dotfiles/.zshrc` - Main zsh configuration
- `dotfiles/.zprofile` - Zsh profile (runs on login shells)
