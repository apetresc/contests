#include "sieve.h"

#include <stdlib.h>

uint32_t sieve(uint32_t limit, uint32_t *primes, size_t max_primes) {
  uint32_t *sieve = calloc(limit, sizeof(uint32_t));
  uint32_t n_primes = 0;

  for (uint32_t i = 2; i <= limit; i++) {
    if (sieve[i] == 0) {
      if (n_primes < max_primes) {
        primes[n_primes++] = i;
      } else {
        break;
      }
      for (uint32_t j = i * i; j <= limit; j += i) {
        sieve[j] = 1;
      }
    }
  }

  return n_primes;
}
