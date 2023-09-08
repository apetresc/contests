#!/usr/bin/env bash

main () {
  local number=$(for arg in "$@"; do echo -n "${arg// /}"; done)

  [[ ! $number =~ ^[0-9][0-9]+$ ]] && echo "false" && exit 0

  local -i sum=0
  for (( i=0; i<${#number}; i+=1 )); do
    local -i digit=${number:${#number}-i-1:1}
    (( i % 2 == 1 )) && (( digit*=2 )) && (( digit > 9 )) && (( digit -= 9 ))
    sum=$(( sum + digit ))
  done

  (( $sum % 10 == 0 )) && echo "true" || echo "false"
}

main "$@"
