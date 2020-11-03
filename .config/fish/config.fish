if not set -q TMUX
    set -gx EMACS "~/Projects/emacs/nextstep/Emacs.app/Contents/MacOS/Emacs"
    set -gx EDITOR "begin; set -lx XDG_CONFIG_HOME ~/.config/emacs_minimal; $EMACS; end"

    set PATH \
        $HOME/bin \
        $HOME/.local/bin \
        $HOME/.cargo/bin \
        $HOME/.cabal/bin \
        $HOME/IronNet/bin \
        /usr/local/sbin \
        $PATH

    for goPath in (string split : (go env GOPATH))
        set PATH $goPath/bin $PATH
    end

    # Programs installed with brew take precedence over pre-installed programs
    set PATH \
        /usr/local/opt/make/libexec/gnubin \
        /usr/local/opt/texinfo/bin \
        /usr/local/opt/openjdk/bin \
        $PATH

    # Colors in less (makes man pages look nicer)
    set -x LESS_TERMCAP_us (set_color -o magenta)  # begin underline
    set -x LESS_TERMCAP_ue (set_color normal)      # reset underline

    # pyenv
    if command -v pyenv > /dev/null
        source (pyenv init - | psub)
        source (pyenv virtualenv-init - | psub)
    end

    # pipenv
    if command -v pipenv > /dev/null
        set -gx PIPENV_IGNORE_VIRTUALENVS 1
    end

    # rbenv
    if command -v rbenv > /dev/null
        source (rbenv init - | psub)
    end

    set os (uname)
    switch $os
        case Darwin
            # pyinstaller
            set -gx PYTHON_CONFIGURE_OPTS "--enable-framework"
        case '*'
            echo "Yo you need to set PYTHON_CONFIGURE_OPTS"
    end
end

alias md5sum "md5 -r"
source ~/.functions.fish
