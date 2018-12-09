#!/bin/bash

# https://github.com/tmux-plugins/tmux-copycat/issues/116
tmux unbind -n n \; unbind -n N

###################
# Sesison: Projects
####################

tmux new-session -d -s Projects
tmux new-window -dk -n rust -t 1
# TODO: set cwd

####################
# Session: Work
####################

tmux new-session -d -s Work
tmux new-window -dk -n local -t 1

tmux new-window -dk -n fugue-ansible -t 2
# TODO: set cwd
tmux new-window -dk -n qa-harness -t 3
# TODO: set cwd

####################
# Session: QA-AWS
####################

tmux new-session -d -s QA-AWS
tmux new-window -dk -n chrisc -t 1
# TODO: set AWS creds

tmux new-window -dk -n chrisc-qa-0 -t 2
# TODO: set AWS creds

tmux new-window -dk -n chrisc-qa-1 -t 3
# TODO: set AWS creds

tmux new-window -dk -n chrisc-alt -t 4
# TODO: set AWS creds
