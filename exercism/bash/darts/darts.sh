#!/usr/bin/env bash

main() {
  local -r x=$1
  local -r y=$2

  # Validate that x and y exist and are numeric
  if [[ -z $x || -z $y || ! $x =~ ^-?[0-9]*\.?[0-9]*$ || ! $y =~ ^-?[0-9]*\.?[0-9]*$ ]]; then
    echo "Invalid input."
    exit 1
  fi

  local -r distance=$(bc <<< "scale=2; sqrt($x^2 + $y^2)")

  local -r scores=(10 5 1 0)
  local -r score=$(bc <<< "$distance > 10.0 ; $distance > 5.0; $distance > 1.0" | paste -sd+ | bc)
  echo ${scores[$score]}
}

main "$@"
