if not set -q TMUX
    set -gx EDITOR emacs

    # Removed the following entries from /etc/paths because of
    # https://github.com/fish-shell/fish-shell/pull/4852:
    # /usr/local/bin /usr/bin /bin /usr/sbin /sbin $PATH
    set PATH /usr/local/bin $PATH

    # PATH
    set PATH $PATH $HOME/bin
    set PATH $PATH $HOME/.local/bin
    set PATH $PATH $HOME/.cargo/bin
    for i in (string split : (go env GOPATH))
        set PATH $PATH $i/bin
    end

    # pyenv
    source (pyenv init - | psub)
    source (pyenv virtualenv-init - | psub)

    # pyinstaller
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
