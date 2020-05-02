#!/usr/bin/env bash

s() {
    [[ -n "$1" ]] || {
        echo "Error: arg1 must be a file path."
        return 1
    }
    envFile="$1"
    export $(cat "$envFile" | xargs -L 1)
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
