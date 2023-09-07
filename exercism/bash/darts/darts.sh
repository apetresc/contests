#!/usr/bin/env bash

main() {
  local -r x=$1
  local -r y=$2

  if [[ ! $x =~ ^[-0-9.]+$ || ! $y =~ ^[-0-9.]+$ ]]; then
    echo "Invalid input."
    exit 1
  fi

  local -r distance=$(bc <<< "scale=2; sqrt($x^2 + $y^2)")
  local -r scores=(10 5 1 0)
  echo ${scores[$(bc <<< "($distance > 10.0) + ($distance > 5.0) + ($distance > 1.0)")]}
}

main "$@"
