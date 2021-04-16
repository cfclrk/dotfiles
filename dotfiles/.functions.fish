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

function s --description "Export environment variables from a file"
    test -z "$argv[1]"; and echo "Error: arg1 must be a file path"; and return 1
    set envFile $argv[1]

    for line in (cat $envFile)
        if test -n $line
            set -l v (string replace "=" " " -- $line)
            eval set -gx $v
        end
    end
end

function grep --description 'Override grep'
    command egrep --color=auto $argv
end

function rand --description 'Some ways to create a decent random string'
    ruby -rsecurerandom -e 'puts SecureRandom.hex(20)'
    # openssl rand -base64 32
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

function rlogin
    rancher login $RANCHER_URL --token $RANCHER_TOKEN
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
    set -e AWS_SESSION_TOKEN
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
    test -z "$argv[1]"; and echo "Error: arg1 must be a string"; and return 1

    echo "Checking bucket $bucket"
    set objects (aws s3api list-objects-v2 --bucket $bucket --delimiter /)
    if count $objects > /dev/null
        set_color red; echo "$bucket has objects. Deleting..."; set_color normal
        aws s3api delete-bucket-policy --bucket $bucket
        aws s3 rm --recursive --only-show-errors s3://$bucket
    else
        set_color green; echo "$bucket is empty." set_color normal
    end
end

function clearBuckets --description "Clear all S3 buckets matching a prefix"
    test -z "$argv[1]"; and echo "Error: arg1 must be a string"; and return 1
    set prefix $argv[1]

    # To filter to those buckets that start with prefix
    set buckets (aws s3api list-buckets \
        | jq -r --arg prefix $prefix \
        '.Buckets[] | select(.Name | startswith($prefix)).Name')

    echo "Matched buckets:"
    for bucket in $buckets
        echo " - $bucket"
    end

    echo $buckets | xargs -n 1 -P (count $buckets) -I {} fish -c 'clearBucket {}'
end

function sso --description "Set credentials for an AWS account using AWS SSO"
    test -z "$argv[1]"; and echo "arg1 must be an AWS account num"; and return
    set accountId $argv[1]
    set tokenFile ~/.aws/sso/cache/b9d131856200283be9603e59dc49f1dc50aa7c62.json

    # TODO: Check for token expiration; if expired, run "aws sso login"
    set token (cat $tokenFile | jq -r '.accessToken')

    set roles (aws --region us-east-1 sso list-account-roles \
        --account-id $accountId \
        --access-token $token \
        | jq -r '.roleList[].roleName')

    # TODO: prompt to select role
    set role $roles[1]

    set creds (aws --region us-east-1 sso get-role-credentials \
        --role-name $role \
        --account-id $accountId \
        --access-token $token)

    set -l accessKey (echo $creds | jq -r '.roleCredentials.accessKeyId')
    set -l secretKey (echo $creds | jq -r '.roleCredentials.secretAccessKey')
    set -l sessionToken (echo $creds | jq -r '.roleCredentials.sessionToken')

    source (echo "\
set -gx AWS_ACCESS_KEY_ID \"$accessKey\"
set -gx AWS_SECRET_ACCESS_KEY \"$secretKey\"
set -gx AWS_SESSION_TOKEN \"$sessionToken\"\
" | psub)
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
    set url https://storage.googleapis.com/kubernetes-release/release/v$progVersion/bin/darwin/amd64/kubectl
    curl -s -o $libPath $url
    chmod +x $libPath
    use $progName $progVersion
end

# Example: install_helm 3.1.2
function install_helm --description "Download a helm binary"
    test -z "$argv[1]"; and echo "arg1 must be a helm version"; and return
    set progVersion $argv[1]
    set progName helm
    set libPath ~/.local/lib/$progName/$progName-$progVersion
    set url https://get.helm.sh/helm-v$progVersion-darwin-amd64.tar.gz
    curl -s $url | tar xvz - -C /tmp
    cp /tmp/darwin-amd64/helm $libPath
    use $progName $progVersion
end

# Example:
# - install_kops latest
# - install_kops v1.17.0
function install_kops
    test -z "$argv[1]"; and echo "arg1 must be a kops version or 'latest'"; and return
    set progVersion $argv[1]
    set progName kops
    if [ $progVersion = "latest" ]
        set progVersion (curl -s \
            https://api.github.com/repos/kubernetes/$progName/releases/latest \
            | grep tag_name \
            | cut -d '"' -f 4)
    end
    echo "progVersion $progVersion"
    set libPath ~/.local/lib/$progName/$progName-$progVersion
    set url https://github.com/kubernetes/$progName/releases/download/$progVersion/$progName-darwin-amd64
    curl -sL -o $libPath $url
    chmod +x $libPath
    use $progName $progVersion
end

# Examples:
#  - use
#  - use helm
#  - use helm 3.5.3
function use --description "Create symlink on $PATH to this installed program"
    set progName $argv[1]

    if test -z $progName
        echo "Local programs managed by use:"
        ls -1 ~/.local/lib/
        return 0
    end

    set progVersion $argv[2]

    if test -z $progVersion
        echo "Locally installed versions:"
        ls -1 ~/.local/lib/$progName
        return 0
    end

    set libPath ~/.local/lib/$progName/$progName-$progVersion
    set binPath ~/.local/bin/$progName
    rm -f $binPath
    ln -s $libPath $binPath
    la $binPath
end

function latest_python
    pyenv install --list \
        | grep "^  3" \
        | grep -v "src|dev" \
        | tail -1 \
        | tr -d '[:space:]'
end

# Azure
# -----------------------------------------------------------------------------

function kvAll --description "All Keyvault keys and secrets"
    set secretIds (az keyvault secret list \
        --vault-name $argv[1] \
        --query "[].id" -o tsv)
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
