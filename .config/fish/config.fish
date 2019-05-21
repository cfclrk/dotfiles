if not set -q TMUX
    set -gx EDITOR emacs

    # Removed the following entries from /etc/paths because of
    # https://github.com/fish-shell/fish-shell/pull/4852:
    # /usr/local/bin /usr/bin /bin /usr/sbin /sbin
    # N.B. Delete those entries again after every OS upgrade!
    set PATH /usr/local/bin $PATH

    # PATH
    set PATH $PATH $HOME/bin
    set PATH $PATH $HOME/.local/bin
    set PATH $PATH $HOME/.cargo/bin
    for i in (string split : (go env GOPATH))
        set PATH $PATH $i/bin
    end
    set PATH $PATH $HOME/.poetry/bin

    # GNU programs installed with brew take precedence over pre-existing programs
    set PATH "/usr/local/opt/make/libexec/gnubin" $PATH

    # pyenv
    if command -v pyenv > /dev/null
        source (pyenv init - | psub)
        source (pyenv virtualenv-init - | psub)
    end

    # rbenv
    source (rbenv init - | psub)

    set os (uname)
    switch $os
        case Darwin
            alias emacs "/Applications/Emacs.app/Contents/MacOS/Emacs -nw"
            alias md5sum "md5 -r"

            # pyinstaller
            set -gx PYTHON_CONFIGURE_OPTS "--enable-framework"
        case '*'
            echo "Yo you need to set PYTHON_CONFIGURE_OPTS"
    end

end

source ~/.functions.fish
source ~/.extras.fish
