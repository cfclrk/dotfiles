#!/usr/bin/env bash

# Sesison Home
# ------------

tmux new-session -d -s Home
tmux new-window -d -n clojure -t 1
tmux new-window -d -n rust -t 2
# TODO: set cwd

# Session Work
# ------------

tmux new-session -d -s Work
tmux new-window -d -n base -t 1
tmux new-window -d -n cloudcover -t 2

# Session AWS
# -----------

tmux new-session -d -s AWS
tmux new-window -d -n home -t 1
tmux new-window -d -n elenapon -t 2

# https://github.com/tmux-plugins/tmux-copycat/issues/116
tmux unbind -n n \; unbind -n N
