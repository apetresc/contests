#include "secret_handshake.h"

#include <stdlib.h>

const char **commands(size_t number) {
  short rev = (number >= (1 << 4)) ? -1 : 1;
  number &= ~(1 << 4);
  short n = __builtin_popcount(number);
  const char *events[] = {"wink", "double blink", "close your eyes", "jump"};

  const char **commands = malloc(n * sizeof(*commands));
  for (short i = (rev == 1) ? 0 : n - 1; i < n && i >= 0; i += rev) {
    short offset = __builtin_ctz(number);
    number &= ~(1 << offset);
    commands[i] = events[offset];
  }

  return commands;
}
