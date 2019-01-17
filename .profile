# don't put duplicate lines or lines starting with space in the history
HISTCONTROL=ignoreboth

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

PATH=$PATH:$HOME/bin
PATH=$PATH:$HOME/.cask/bin
PATH=$PATH:$HOME/.cargo/bin

# golang
PATH=$(go env GOPATH)/bin:$PATH

# ludwig
export LUDWIG_PATH=lib:compositions

if which pyenv > /dev/null; then
   eval "$(pyenv init -)"
   eval "$(pyenv virtualenv-init -)"
fi

. ~/.bashrc
