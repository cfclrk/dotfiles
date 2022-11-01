if not set -q TMUX

    # NOTE: Fish doesn't allow variables in commands, so to use EDITOR, run
    # "eval $EDITOR"
    set -gx EMACS "emacs"
    set -gx EDITOR "$EMACS --with-profile minimal"

    eval (/opt/homebrew/bin/brew shellenv)

    set PATH \
        $HOME/bin \
        $HOME/.local/bin \
        $HOME/.cargo/bin \
        $HOME/.cabal/bin \
        $HOME/.ghcup/bin \
        $HOME/Work/bin \
        $HOMEBREW_PREFIX/sbin \
        $HOMEBREW_PREFIX/opt/curl/bin \
        $HOMEBREW_PREFIX/opt/coreutils/libexec/gnubin \
        $HOMEBREW_PREFIX/opt/libtool/libexec/gnubin \
        $HOMEBREW_PREFIX/opt/findutils/libexec/gnubin \
        $HOMEBREW_PREFIX/opt/libpq/bin \
        $HOMEBREW_PREFIX/opt/make/libexec/gnubin \
        $HOMEBREW_PREFIX/opt/openssl@1.1/bin \
        $HOMEBREW_PREFIX/opt/texinfo/bin \
        $PATH

    for i in (string split : (go env GOPATH))
        set PATH $i/bin $PATH
    end

    # Setting XDG_CONFIG_HOME makes more programs use it
    set -gx XDG_CONFIG_HOME ~/.config

    # Use the new Docker run engine
	set -gx DOCKER_BUILDKIT 1

    # frum - this prepends to $PATH
    if command -v frum > /dev/null
        frum init | source
    end

    # pyenv - this prepends to $PATH
    if command -v pyenv > /dev/null
        pyenv init --path | source
        pyenv init - | source
        pyenv virtualenv-init - | source
    end

    # pipenv
    if command -v pipenv > /dev/null
        set -gx PIPENV_IGNORE_VIRTUALENVS 1
    end

    # ghcup
    set GHCUP_INSTALL_BASE_PREFIX $HOME

    set os (uname)
    switch $os
        case Darwin
            # pyinstaller
            set -gx PYTHON_CONFIGURE_OPTS "--enable-framework"
        case '*'
            echo "Set PYTHON_CONFIGURE_OPTS"
    end
end

alias md5sum "md5 -r"
source ~/.functions.fish
source ~/Work/bin/fish.fish
