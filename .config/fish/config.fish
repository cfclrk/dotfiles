if not set -q TMUX
    set -gx EDITOR emacs

    # PATH
    set PATH $HOME/bin \
        $HOME/.local/bin \
        $HOME/.cargo/bin \
        $HOME/.cabal/bin \
        $HOME/IronNet/bin \
        $PATH
    for goPath in (string split : (go env GOPATH))
        set PATH $goPath/bin $PATH
    end

    # Conflicting programs installed with brew take precedence over
    # pre-installed programs
    set PATH /usr/local/opt/make/libexec/gnubin \
        /usr/local/opt/texinfo/bin \
        /usr/local/opt/openjdk/bin \
        $PATH

    # colors in less (makes man pages look nicer)
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

alias emacs "/Applications/Emacs.app/Contents/MacOS/Emacs -nw"
alias md5sum "md5 -r"
source ~/.functions.fish
