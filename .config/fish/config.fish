if status is-interactive
and not set -q TMUX

    set -gx EDITOR emacs

    # PATH
    set PATH $PATH $HOME/bin
    set PATH $PATH $HOME/.local/bin
    set PATH $PATH $HOME/.cargo/bin
    for i in (string split : (go env GOPATH))
        set PATH $PATH $i/bin
    end

    # jenv (java version management)
    #source (jenv init - | psub)

    # pyenv (python version management)
    source (pyenv init - | psub)
    source (pyenv virtualenv-init - | psub)

    # pyinstaller config
    set os (uname)
    switch $os
        case Darwin
            set -gx PYTHON_CONFIGURE_OPTS "--enable-framework"
        case '*'
            echo "Yo you need to set PYTHON_CONFIGURE_OPTS"
    end
end

alias emacs "/Applications/Emacs.app/Contents/MacOS/Emacs -nw"

source ~/.functions.fish
source ~/.extras.fish
