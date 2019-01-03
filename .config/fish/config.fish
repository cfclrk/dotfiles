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

    # pyinstaller config
    set os (uname)
    switch $os
        case Darwin
            set -gx PYTHON_CONFIGURE_OPTS "--enable-framework"
        case '*'
            echo "Yo you need to set PYTHON_CONFIGURE_OPTS"
    end
end

if status --is-interactive
    # pyenv (python version management). I used to have this in the "and not set
    # -q TMUX" block, but that was broken by some updates for path_helper
    # (https://github.com/fish-shell/fish-shell/pull/4852). Now that fish
    # prepends shit to $PAH in every new subshell, I need to force pyenv stuff
    # to come first in every new subshell. And UGGGH, it takes looonger to
    # initilze a bunch of shells now!
    source (pyenv init - | psub)
    source (pyenv virtualenv-init - | psub)
end

alias emacs "/Applications/Emacs.app/Contents/MacOS/Emacs -nw"

source ~/.functions.fish
source ~/.extras.fish
