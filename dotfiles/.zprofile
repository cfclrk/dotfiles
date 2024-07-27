eval "$(/opt/homebrew/bin/brew shellenv)"

# In zsh, path is an array version of $PATH
path=($HOME/bin
      $HOME/.local/bin
      $HOME/.cargo/bin
      $HOME/.cabal/bin
      $HOME/.ghcup/bin
      $HOME/Work/bin
      $HOMEBREW_PREFIX/sbin
      $HOMEBREW_PREFIX/opt/curl/bin
      $HOMEBREW_PREFIX/opt/coreutils/libexec/gnubin
      $HOMEBREW_PREFIX/opt/findutils/libexec/gnubin
      $HOMEBREW_PREFIX/opt/libpq/bin
      $HOMEBREW_PREFIX/opt/make/libexec/gnubin
      $HOMEBREW_PREFIX/opt/openssl@1.1/bin
      $HOMEBREW_PREFIX/opt/texinfo/bin
      $path)
