# Don't put duplicate lines or lines starting with space in the history
HISTCONTROL=ignoreboth

# For setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

export GOPATH=$HOME/IronNet/go

PATH=$HOME/bin:$PATH
PATH=$HOME/.cargo/bin:$PATH
PATH=$(go env GOPATH)/bin:$PATH
PATH="$HOME/.cargo/bin:$PATH"
PATH="$HOME/.poetry/bin:$PATH"

# GNU programs installed with brew take precedence over pre-existing programs
PATH="/usr/local/opt/make/libexec/gnubin:$PATH"

# pyenv
if command -v pyenv > /dev/null; then
   eval "$(pyenv init -)"
   eval "$(pyenv virtualenv-init -)"
fi

# pipenv
export PIPENV_IGNORE_VIRTUALENVS=1

# Homebrew: do not run cleanup every 30 days. I like to `brew switch`.
export HOMEBREW_NO_INSTALL_CLEANUP=1

. ~/.bashrc