function fish_prompt --description 'Defines the prompt'
    # Save the return status of the last command
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

function grep --description 'Override grep'
    command egrep --color=auto $argv
end

function rand --description 'Some ways to create a decent random string'
    openssl rand -base64 32
    ruby -rsecurerandom -e 'puts SecureRandom.hex(20)'
end

function ssh_decrypt --description 'Decrypt password-protected ssh key'
    openssl rsa -in $argv[1] -out $argv[2]
end

function myip --description "What is my ip address?"
    # curl ifconfig.co
    # curl icanhazip.com
    # for more info: http -b ifconfig.co/json
    curl https://checkip.amazonaws.com/
end

# Do not let computer go to sleep while Slack is running.
# Notes:
#  - Slack needs to *already be running*
#  - This command starts a process that stops when Slack is closed
#
# So, if I must re-run this command every time I open Slack.
function caff --description "caffeinate while Slack is running"
    nohup caffeinate -d -i -m -s -u -w (pgrep -x Slack) &
end

# Ruby
# -----------------------------------------------------------------------------

function uninstall_gems
    for gem in (gem list --no-versions)
        gem uninstall $gem -aIx
    end
end

# Clojure
# -----------------------------------------------------------------------------

function cljrepl
    clj -Sdeps '{:deps {cider/cider-nrepl {:mvn/version "RELEASE"}}}' \
        -m nrepl.cmdline \
        --middleware "[cider.nrepl/cider-middleware]"
end

function cljnew
    set projectName $argv[1]
    clj -A:new lib cfclrk/$projectName
end

# Golang
# -----------------------------------------------------------------------------

function godocwkspc --description 'Serve godoc http for the current Go workspace'
    for p in (string split : (go env GOPATH))
        if string match --regex $p* (pwd)
            echo "Current Go workspace is: $p"
            echo "Visit: http://localhost:8000/pkg/"
            godoc -http=localhost:8000 -goroot $p
        end
    end
end

function go_get_tools
    go get -u github.com/zmb3/gogetdoc  # Used in emacs godoc-at-point-function
    go get -u github.com/rogpeppe/godef  # Find symbol information in Go source
    go get -u github.com/mdempsky/gocode  # Editor auto completion
    go get -u golang.org/x/tools/...  # godoc, gopls, goimports, gorename, etc
    go get -u honnef.co/go/tools/...  # staticcheck, structlayout, rdeps, keyify
    go get -u golang.org/x/lint/golint  # golint
end

# Python
# -----------------------------------------------------------------------------

function newenv --description "Destroy and re-create a venv"
    deactivate && \
        rm -rf .venv && \
        python -m venv .venv && \
        source .venv/bin/activate.fish && \
        pip install -U pip setuptools
end

# AWS
# -----------------------------------------------------------------------------

function clear_aws
    set -e AWS_ACCESS_KEY_ID
    set -e AWS_SECRET_ACCESS_KEY
    set -e AWS_DEFAULT_REGION
    set -e AWS_DEFAULT_PROFILE
    set -e AWS_PROFILE
end

function export_aws --description "Export credentials from a profile in ~/.aws/credentials"
    set profile "\[$argv[1]\]"
    set creds (grep "$profile" -A 3 ~/.aws/credentials \
        | tail -n 3 \
        | string match -r '.+' \
        | string trim \
        | string split = \
        | string trim)
    set -gx (echo $creds[1] | tr '[:lower:]' '[:upper:]') (echo $creds[2])
    set -gx (echo $creds[3] | tr '[:lower:]' '[:upper:]') (echo $creds[4])
    if test (count $creds) -eq 6
        set -gx (echo $creds[5] | tr '[:lower:]' '[:upper:]') (echo $creds[6])
    end
end

function clearBucket --description "Clear one S3 bucket"
    set bucket $argv[1]
    echo Checking bucket $bucket
    set objects (aws s3api list-objects-v2 --bucket $bucket --delimiter /)
    if count $objects > /dev/null
        set_color red
        echo "$bucket - has objects. Deleting..."
        set_color normal
        aws s3api delete-bucket-policy --bucket $bucket
        aws s3 rm --recursive s3://$bucket
    else
        set_color green
        echo "$bucket - good to go."
        set_color normal
    end
end

function clearBuckets --description "Clear all S3 buckets"
    set buckets (aws s3api list-buckets | jq -r .Buckets[].Name)
    echo $buckets | xargs -n 1 -P (count $buckets) -I {} fish -c 'clearBucket {}'
end

function ssoRefresh --description "Refresh credentials for an AWS account using AWS SSO"
    # This doesn't work, just throwing down some stuff.

    # TODO:
    # - Look up token file
    # - Check for token expiration; if expired, run "aws sso login"

    test -z "$argv[1]"; and echo "arg1 must be an AWS account num"; and return
    set accountId $argv[1]

    set r us-east-1
    set tokenFile ~/.aws/sso/cache/bc340e54782a2aa31c5a3116c25dfa13dabaa7d3.json
    set token (cat $tokenFile | jq -r '.accessToken')

    set roles (aws --region us-east-1 sso list-account-roles \
        --account-id $accountId \
        --access-token $token \
        | jq -r '.roleList[].roleName')

    # TODO: prompt to select role
    set role $roles[1]
    echo $role

   set  creds (aws --region $r sso get-role-credentials \
        --role-name $role \
        --account-id $accountId \
        --access-token $token)

    echo $creds
end

# Kubernetes
# -----------------------------------------------------------------------------

alias k "kubectl"
alias kn "kubectl config set-context (kubectl config current-context) --namespace"
alias rk "rancher kubectl"

# Example: install_kubectl 1.16.7
function install_kubectl --description "Download a kubectl binary"
    test -z "$argv[1]"; and echo "arg1 must be a kubectl version"; and return
    set progVersion $argv[1]

    set progName kubectl
    set libPath ~/.local/lib/$progName/$progName-$progVersion
    set binPath ~/.local/bin/$progName
    set url https://storage.googleapis.com/kubernetes-release/release/v$progVersion/bin/darwin/amd64/kubectl
    curl -s -o $path $url
    chmod +x $path
    rm -f $binPath
    ln -s $libPath $binPath
    la $binPath
end

# Example: install_helm 3.1.2
function install_helm --description "Download a helm binary"
    test -z "$argv[1]"; and echo "arg1 must be a helm version"; and return
    set progVersion $argv[1]

    set progName helm
    set libPath ~/.local/lib/$progName/$progName-$progVersion
    set binPath ~/.local/bin/$progName
    set url https://get.helm.sh/helm-v$progVersion-darwin-amd64.tar.gz
    curl -s $url | tar xvz - -C /tmp
    cp /tmp/darwin-amd64/helm $libPath
    rm -f $binPath
    ln -s $libPath $binPath
    la $binPath
end

# Example: use helm 2.9.1
function use --description "Create symlink on $PATH to this installed program"
    set progName $argv[1]
    set progVersion $argv[2]
    set libPath ~/.local/lib/$progName/$progName-$progVersion
    set binPath ~/.local/bin/$progName
    rm -f $binPath
    ln -s $libPath $binPath
end

# Azure
# -----------------------------------------------------------------------------

function kvAll --description 'All keyvault keys and secrets'
    set secretIds (az keyvault secret list --vault-name $argv[1] --query "[].id" -o tsv)
    for i in $secretIds
        set val (az keyvault secret show --id $i --query "value" -o tsv)
        echo ""
        echo $i
        printf "%b\n" "$val"
    end
end

function getVmLimit --description "Get a quota limit in the given region"
    set location $argv[1]
    set limit (az vm list-usage \
        --location $location \
        -o json \
        | jq -r '.[] | select(.localName == "Total Regional vCPUs") | .limit')
    echo "$location: $limit"
end

function getAllVmLimits --description "Get all Total Region vCPU limits"
    set locations (az account list-locations | jq -r '.[].name')
    echo $locations | xargs -n 1 -P (count $locations) -I {} fish -c 'getVmLimit {}'
end
