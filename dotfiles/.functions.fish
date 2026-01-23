function fish_prompt --description 'Defines the prompt'
    # Save the return status of the last command
    set stat $status

    if not set -q __fish_prompt_normal
        set -g __fish_prompt_normal (set_color yellow)
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
        set -g __fish_prompt_cwd (set_color black)
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

function s-line
    test -z "$argv[1]"; and echo "Error [s-line]: arg1 must be a string"; and return 1
    set line $argv[1]
    set item (string split -m 1 '=' $line)
    eval set -gx $item[1] $item[2]
end

function s --description "Export environment variables from a file"
    test -z "$argv[1]"; and echo "Error: arg1 must be a file path"; and return 1
    set envFile $argv[1]
    for line in (cat $envFile)
        if test -n $line
            # Another option is to just do "export $line", but that doesn't
            # expand ~, and probably some other things.
            s-line "$line"
        end
    end
end

function grep --description 'Override grep'
    command egrep --color=auto $argv
end

function rand --description 'Some ways to create a decent random string'
    ruby -rsecurerandom -e 'puts SecureRandom.hex(20)'

    # A random binary byte array encoded in base64
    # openssl rand -base64 32
end

function myip --description "What is my ip address?"
    # curl ifconfig.co
    # curl icanhazip.com
    # for more info: http -b ifconfig.co/json
    curl https://checkip.amazonaws.com/
end

# Ruby
# -----------------------------------------------------------------------------

function uninstall_gems
    for gem in (gem list --no-versions)
        gem uninstall $gem -aIx
    end
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

function export-aws --description "Export credentials from a profile in ~/.aws/credentials"
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

function clear-bucket --description "Clear one S3 bucket"
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

function clear-buckets --description "Clear all S3 buckets matching a prefix"
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

function kEnvVars --description "Get all env vars for service"
    set serviceName $argv[1]
    kubectl get configmap \
        "$serviceName-config" \
        -n staging \
        -o go-template='{{range $k, $v := .data}}{{$k}}={{$v}}{{"\n"}}{{end}}'
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
