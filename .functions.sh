function export_aws {
    profile="\[$1\]"
    creds=$(grep $profile -A 3 ~/.aws/credentials | tail -n 3 | tr "=" "\n")
    creds_array=($creds)
    export $(echo ${creds_array[0]} | tr '[:lower:]' '[:upper:]')=$(echo ${creds_array[1]})
    export $(echo ${creds_array[2]} | tr '[:lower:]' '[:upper:]')=$(echo ${creds_array[3]})
    export $(echo ${creds_array[4]} | tr '[:lower:]' '[:upper:]')=$(echo ${creds_array[5]})
}
