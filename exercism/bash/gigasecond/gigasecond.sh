main() {
  local -r GIGASECOND=1000000000
  local -r BIRTHDAY=$(date -u -d "$1" +%s)
  local -r GIGASECOND_ANNIVERSARY=$((BIRTHDAY + GIGASECOND))
  date -u -d "@$GIGASECOND_ANNIVERSARY" +%Y-%m-%dT%H:%M:%S
}

main "$@"
