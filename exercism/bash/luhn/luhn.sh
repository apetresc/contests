#!/usr/bin/env bash

main () {
  local number=$(for arg in "$@"; do echo -n "${arg// /}"; done)

  [[ ! $number =~ ^[0-9][0-9]+$ ]] && echo "false" && exit 0

  for (( i=${#number}-2; i>=0; i-=2 )); do
    local -i digit=${number:$i:1}
    (( digit*=2 )) && (( digit > 9 )) && (( digit -= 9 ))
    number=${number:0:$i}$digit${number:i+1}
  done

  local -i sum=0
  for (( i=0; i<${#number}; i++ )); do
    sum=$(( sum + ${number:$i:1} ))
  done

  (( $sum % 10 == 0 )) && echo "true" || echo "false"
}

main "$@"
