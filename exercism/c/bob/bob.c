#include "bob.h"
#include <ctype.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

static bool is_question(char *greeting, size_t length) {
  size_t end = length - 1;
  while (end > 0 && isspace(greeting[end])) {
    end--;
  }
  return greeting[end] == '?';
}

static bool is_yelling(char *greeting, size_t length) {
  bool yelling = false;
  for (size_t i = 0; i < length; i++) {
    if (islower(greeting[i])) {
      return false;
    } else if (isupper(greeting[i])) {
      yelling = true;
    }
  }
  return yelling;
}

static bool is_silence(char *greeting, size_t length) {
  for (size_t i = 0; i < length; i++) {
    if (!isspace(greeting[i])) {
      return false;
    }
  }
  return true;
}

char *hey_bob(char *greeting) {
  size_t length = strlen(greeting);
  bool question = is_question(greeting, length);
  bool yelling = is_yelling(greeting, length);

  if (question && !yelling) {
    return "Sure.";
  } else if (question && yelling) {
    return "Calm down, I know what I'm doing!";
  } else if (!question && yelling) {
    return "Whoa, chill out!";
  } else if (is_silence(greeting, length)) {
    return "Fine. Be that way!";
  }

  return "Whatever.";
}
