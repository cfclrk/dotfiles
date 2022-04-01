#!/usr/bin/env bash

# Install rustup. See: https://rustup.rs/
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

# Install rust-analyzer (LSP server)
brew install rust-analyzer

# Add wasm component

# cargo-edit can update the cargo.toml via CLI commands like "cargo add", etc.
cargo install cargo-edit
