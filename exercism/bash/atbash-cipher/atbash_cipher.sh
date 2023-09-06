#!/usr/bin/env bash

main() {
  local command="$1"
  local input="$2"
  local output=""

  for (( i=0; i<${#input}; i++ )); do
    local char="${input:$i:1}"
    local alphabet="abcdefghijklmnopqrstuvwxyz"
    if [[ $char =~ [a-zA-Z] ]]; then
      output+=${alphabet:26-$((${#${alphabet%%${char,,}*}})):1}
    elif [[ $char =~ [0-9] ]]; then
      output+="$char"
    fi
  done

  case "$command" in
    encode)
      echo $(echo $output | fold -w5)
      ;;
    decode)
      echo $output
      ;;
    *)
      echo "Invalid command: $command"
      exit 1
      ;;
  esac
}

main "$@"
