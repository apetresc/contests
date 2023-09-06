#!/usr/bin/env bash

main() {
  local -r x=$1
  local -r y=$2

  # Validate that x and y exist and are numeric
  if [[ -z $x || -z $y || ! $x =~ ^-?[0-9]+\.?[0-9]*$ || ! $y =~ ^-?[0-9]+\.?[0-9]*$ ]]; then
    echo "Invalid input."
    exit 1
  fi

  local -r distance=$(bc <<< "scale=2; sqrt($x^2 + $y^2)")

  (( $(bc -l <<< "$distance > 10.0") )) && echo 0 && exit 0 ||
    (( $(bc -l <<< "$distance > 5.0") )) && echo 1 && exit 0 ||
    (( $(bc -l <<< "$distance > 1.0") )) && echo 5 && exit 0 ||
    (( $(bc -l <<< "$distance >= 0.0") )) && echo 10 && exit 0
}

main "$@"
