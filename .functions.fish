function fish_prompt --description 'Defines the prompt'

    # Save the return status of the previous command
    set stat $status

    if not set -q __fish_prompt_normal
        set -g __fish_prompt_normal (set_color normal)
    end

    if not set -q __fish_color_blue
        set -g __fish_color_blue (set_color -o blue)
    end

    if not set -q __fish_color_red
        set -g __fish_color_red (set_color -o red)
    end

    if not set -q __fish_color_time
        set -g __fish_color_time (set_color -o brmagenta)
    end

    # Set the color for the status depending on the value
    set __fish_color_status (set_color -o green)
    if test $stat -gt 0
        set __fish_color_status (set_color -o red)
    end

    if not set -q __fish_prompt_cwd
        set -g __fish_prompt_cwd (set_color $fish_color_cwd)
    end

    printf '%s[%s]%s %s%s %s(%s) \f\r%s$%s ' \
    "$__fish_color_time" \
    (date "+%H:%M:%S") \
    "$__fish_prompt_normal" \
    "$__fish_prompt_cwd" \
    "$PWD" \
    "$__fish_color_status" \
    "$stat" \
    "$__fish_color_red" \
    "$__fish_prompt_normal"
end

function man --description 'Colorize man'
    set -x LESS_TERMCAP_md (set_color -o red)
    set -x LESS_TERMCAP_us (set_color -o magenta)
    set -x LESS_TERMCAP_ue (set_color normal)
    set -x LESS_TERMCAP_me (set_color normal)
    env man $argv
end

function tmuxinit --description 'Initialize tmux'
    # https://github.com/tmux-plugins/tmux-copycat/issues/116
    tmux unbind -n n \; unbind -n N

    # Sesison: Projects
    tmux new-session -d -s Projects
    tmux new-window -dk -n rust -t 1

    # Session: Work
    tmux new-session -d -s Work
    tmux new-window -dk -n local -t 1
    tmux new-window -dk -n cloudcover -t 2

    # Session: QA-AWS
    tmux new-session -d -s QA-AWS
    tmux new-window -dk -n chrisc -t 1
    tmux new-window -dk -n chrisc-qa-0 -t 2
    tmux new-window -dk -n chrisc-qa-1 -t 3
    tmux new-window -dk -n chrisc-alt -t 4
end

function kvAll --description 'All keyvault keys and secrets'
    set secretIds (az keyvault secret list --vault-name $argv[1] --query "[].id" -o tsv)
    for i in $secretIds
        set val (az keyvault secret show --id $i --query "value" -o tsv)
        echo ""
        echo $i
        printf "%b\n" "$val"
    end
end

function rand --description 'Some ways to create a decent random string'
    openssl rand -base64 32
    ruby -rsecurerandom -e 'puts SecureRandom.hex(20)'
end

function ssh_decrypt --description 'Decrypt password-protected ssh key'
    openssl rsa -in $argv[1] -out $argv[2]
end

function clear_aws
    set -e AWS_ACCESS_KEY_ID
    set -e AWS_SECRET_ACCESS_KEY
    set -e AWS_DEFAULT_REGION
    set -e AWS_DEFAULT_PROFILE
    set -e AWS_PROFILE
end

function myip --description "What is my ip address?"
    # Another good one with a little more info is ifconfig.co
    curl https://checkip.amazonaws.com/
end

function npmex --description "Run a command with node_modules/.bin in the PATH"
    set -lx PATH (npm bin) $PATH
    eval $argv
end

function godocwkspc --description 'Serve godoc http for the current Go workspace'
    for p in (string split : (go env GOPATH))
        if string match --regex $p* (pwd)
            echo "Current Go workspace is: $p"
            echo "Visit: http://localhost:8000/pkg/"
            godoc -http=localhost:8000 -goroot $p
        end
    end
end
