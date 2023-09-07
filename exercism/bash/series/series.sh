#!/usr/bin/env bash

main () {
  local -r number=$1
  local -r -i n=$2

  die() { echo "$1"; exit 1; }
  [[ -z $1 ]]             && die "series cannot be empty"
  [[ $n -gt ${#number} ]] && die "slice length cannot be greater than series length"
  [[ $n -eq 0 ]]          && die "slice length cannot be zero"
  [[ $n -lt 0 ]]          && die "slice length cannot be negative"

  echo $(for (( i=0; i<${#number}-n+1; i++ )); do
    echo ${number:$i:$n}
  done)
}

main "$@"
