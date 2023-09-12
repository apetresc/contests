#include "binary_search.h"

const int *binary_search(int value, const int *arr, size_t length) {
  if (!arr || length == 0) {
    return NULL;
  } else if (length == 1) {
    return (*arr == value) ? arr : NULL;
  }

  size_t mid = length / 2;

  if (arr[mid] == value) {
    return arr + mid;
  } else if (arr[mid] > value) {
    return binary_search(value, arr, mid);
  } else {
    return binary_search(value, arr + mid, length - mid);
  }
}
