if not set -q TMUX
    set -gx EDITOR emacs

    # Removed the following entries from /etc/paths because of
    # https://github.com/fish-shell/fish-shell/pull/4852:
    # /usr/local/bin /usr/bin /bin /usr/sbin /sbin
    # N.B. Delete those entries again after every OS upgrade!
    set PATH /usr/local/bin $PATH
    set PATH /usr/local/sbin $PATH

    # PATH
    set PATH $HOME/bin $PATH
    set PATH $HOME/.local/bin $PATH
    set PATH $HOME/.cargo/bin $PATH
    set PATH $HOME/.cabal/bin $PATH
    for i in (string split : (go env GOPATH))
        set PATH $PATH $i/bin
    end
    set PATH $PATH $HOME/.poetry/bin

    # IronNet
    set PATH $HOME/IronNet/bin $PATH
    set -gx IGO $HOME/go/src/github.com/ironnetcybersecurity

    # GNU programs installed with brew take precedence over pre-existing programs
    set PATH "/usr/local/opt/make/libexec/gnubin" $PATH

    # pyenv
    if command -v pyenv > /dev/null
        source (pyenv init - | psub)
        source (pyenv virtualenv-init - | psub)
    end

    # pipenv
    set -gx PIPENV_IGNORE_VIRTUALENVS 1

    # rbenv
    source (rbenv init - | psub)

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
source ~/.extras.fish

set -gx REQUESTS_CA_BUNDLE ~/IronNet/certificates/ironnet_ca_bundle.pem
