#include "pangram.h"

#include <ctype.h>
#include <stdint.h>
#include <string.h>

bool is_pangram(const char *sentence) {
  if (sentence == NULL)
    return false;

  uint32_t letters = 0;

  for (size_t i = 0; i < strlen(sentence); i++) {
    char c = tolower(sentence[i]);
    if (isalpha(c)) {
      letters |= (1 << (c - 'a'));
    }
  }

  return letters == ((1 << 26) - 1);
}
