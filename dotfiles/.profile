HISTCONTROL=ignoreboth  # Ignore duplicate lines in the history
HISTSIZE=1000
HISTFILESIZE=2000

PATH=$HOME/bin:$PATH
PATH=$HOME/.cargo/bin:$PATH
PATH=$(go env GOPATH)/bin:$PATH
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

. ~/.bashrc
