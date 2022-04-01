#!/usr/bin/env bash

# NOTE: I tried using brew, but it seems that "brew install
# haskell-language-server" does not install the haskell-language-server-wrapper.

# Install ghcup: https://www.haskell.org/ghcup/
curl --proto '=https' --tlsv1.2 -sSf \
     https://get-ghcup.haskell.org \
    | sh
