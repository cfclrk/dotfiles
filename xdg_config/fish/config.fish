if not set -q TMUX

    set -gx EMACS "$HOME/Projects/emacs/nextstep/Emacs.app/Contents/MacOS/Emacs"
    set -gx EDITOR $EMACS

    set PATH \
        $HOME/bin \
        $HOME/.local/bin \
        $HOME/.cargo/bin \
        $HOME/.cabal/bin \
        $HOME/IronNet/bin \
        /usr/local/sbin \
        # Programs installed with brew take precedence over pre-installed
        # programs
        /usr/local/opt/curl/bin \
        /usr/local/opt/findutils/libexec/gnubin \
        /usr/local/opt/make/libexec/gnubin \
        /usr/local/opt/openjdk/bin \
        /usr/local/opt/openssl@1.1/bin \
        /usr/local/opt/texinfo/bin \
        $PATH

    # Golang
    set -gx GO111MODULE on
    for goPath in (string split : (go env GOPATH))
        set PATH $goPath/bin $PATH
    end

    # pyenv
    if command -v pyenv > /dev/null
        pyenv init --path | source
        pyenv init - | source
        pyenv virtualenv-init - | source
    end

    # rbenv
    if command -v rbenv > /dev/null
        source (rbenv init - | psub)
    end

	# Use the new Docker run engine
	set -gx DOCKER_BUILDKIT 1

    # Colors in less (makes man pages look nicer)
    set -x LESS_TERMCAP_us (set_color -o magenta)  # begin underline
    set -x LESS_TERMCAP_ue (set_color normal)      # reset underline

    # pipenv
    if command -v pipenv > /dev/null
        set -gx PIPENV_IGNORE_VIRTUALENVS 1
    end

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
source ~/IronNet/shell/fish.fish
