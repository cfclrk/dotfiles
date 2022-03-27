if not set -q TMUX

    set -gx EMACS "$HOME/Projects/cloned/emacs/nextstep/Emacs.app/Contents/MacOS/Emacs"
    set -gx EDITOR "$EMACS --with-profile minimal"
    # Fish doesn't allow variables in commands, so to use EDITOR, run "eval $EDITOR"

    set PATH \
        $HOME/bin \
        $HOME/.local/bin \
        $HOME/.cargo/bin \
        $HOME/.cabal/bin \
        $HOME/.ghcup/bin \
        $HOME/Work/bin \
        /usr/local/sbin \
        # Programs installed with brew take precedence over pre-installed
        # programs
        /usr/local/opt/curl/bin \
        /usr/local/opt/coreutils/libexec/gnubin \
        /usr/local/opt/libtool/libexec/gnubin \
        /usr/local/opt/findutils/libexec/gnubin \
        /usr/local/opt/make/libexec/gnubin \
        /usr/local/opt/openssl@1.1/bin \
        /usr/local/opt/texinfo/bin \
        $PATH

    # Colors in less (makes man pages look nicer)
    set -x LESS_TERMCAP_us (set_color -o magenta)  # begin underline
    set -x LESS_TERMCAP_ue (set_color normal)      # reset underline

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
