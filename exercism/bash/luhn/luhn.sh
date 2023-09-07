#!/usr/bin/env bash

main () {
  local number=$(for arg in "$@"; do echo -n "${arg// /}"; done)

  if [[ ! $number =~ ^[0-9][0-9]+$ ]]; then
    echo "false"
    exit 0
  fi

  for (( i=${#number}-2; i>=0; i-=2 )); do
    local -i digit=${number:$i:1}
    digit=$(( digit * 2 ))
    if (( digit > 9 )); then
      digit=$(( digit - 9 ))
    fi
    number=${number:0:$i}$digit${number:i+1}
  done
  local -i sum=0
  for (( i=0; i<${#number}; i++ )); do
    sum=$(( sum + ${number:$i:1} ))
  done

  if (( sum % 10 == 0 )); then
    echo "true"
  else
    echo "false"
  fi
}

main "$@"
