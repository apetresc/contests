#!/usr/bin/env bash

main() {
  local command="$1"
  local input="$2"
  local output=""

  for (( i=0; i<${#input}; i++ )); do
    local char="${input:$i:1}"
    if [[ $char =~ [a-zA-Z] ]]; then
      output+=$(printf \\$(printf '%03o' "$(( 219 - $(printf "%d" "'${char,,}") ))"))
    elif [[ $char =~ [0-9] ]]; then
      output+="$char"
    fi
  done

  case "$command" in
    encode)
      printf '%s\n' "$output" | fold -w5 | paste -sd' ' -
      ;;
    decode)
      printf '%s\n' "$output"
      ;;
    *)
      echo "Invalid command: $command"
      exit 1
      ;;
  esac
}

main "$@"