#!/usr/bin/env bash

s() {
    [[ -n "$1" ]] || {
        echo "Error: arg1 must be a file path."
        return 1
    }
    envFile="$1"

    while read line; do
        if test -n "$line"; then
            eval "export $line"
        fi
    done <"$envFile"
}

export_aws() {
    [[ -n "$1" ]] || {
        echo "Error: AWS profile name is required."
        return 1
    }
    profile="\[$1\]"
    creds=$(grep "$profile" -A 2 ~/.aws/credentials | tail -n 2 | tr "=" "\n")
    creds_array=($creds)
    export $(echo ${creds_array[0]} | tr '[:lower:]' '[:upper:]')=$(echo ${creds_array[1]})
    export $(echo ${creds_array[2]} | tr '[:lower:]' '[:upper:]')=$(echo ${creds_array[3]})
}

function concatPdf {
    gs -q -sPAPERSIZE=letter -dNOPAUSE -dBATCH -sDEVICE=pdfwrite \
       -sOutputFile=output.pdf file1.pdf file2.pdf
}

function sso {
    [[ -n "$1" ]] || {
        echo "Error: arg1 must be an AWS account number"
        return 1
    }
    accountId="$1"
    tokenFile=~/.aws/sso/cache/bc340e54782a2aa31c5a3116c25dfa13dabaa7d3.json

    # TODO: Check for token expiration; if expired, run "aws sso login"
    token=$(cat $tokenFile | jq -r '.accessToken')

    roles=($(aws --region us-east-1 sso list-account-roles \
        --account-id $accountId \
        --access-token $token \
        | jq -r '.roleList[].roleName'))

    # TODO: prompt to select role
    role=${roles[0]}

    creds=$(aws --region us-east-1 sso get-role-credentials \
        --role-name $role \
        --account-id $accountId \
        --access-token $token)

    accessKey=$(echo $creds | jq -r '.roleCredentials.accessKeyId')
    secretKey=$(echo $creds | jq -r '.roleCredentials.secretAccessKey')
    sessionToken=$(echo $creds | jq -r '.roleCredentials.sessionToken')

    eval $(echo "\
export AWS_ACCESS_KEY_ID=\"$accessKey\"
export AWS_SECRET_ACCESS_KEY=\"$secretKey\"
export AWS_SESSION_TOKEN=\"$sessionToken\"\
")
}
