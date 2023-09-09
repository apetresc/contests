#include "linked_list.h"

#include <stdlib.h>

struct list_node {
  struct list_node *prev, *next;
  ll_data_t data;
};

struct list {
  struct list_node *first, *last;
};

struct list *list_create(void) {
  // Allocate a list_node
  struct list *list = malloc(sizeof(struct list));

  return list;
}

void list_destroy(struct list *list) {
  // Free all the nodes
  struct list_node *node = list->first;
  while (node != NULL) {
    struct list_node *next = node->next;
    free(node);
    node = next;
  }

  // Free the list
  free(list);
}

size_t list_count(const struct list *list) {
  size_t size = 0;
  struct list_node *node = list->first;

  while (node != NULL) {
    size++;
    node = node->next;
  }

  return size;
}

void list_push(struct list *list, ll_data_t item_data) {
  // Allocate a new node
  struct list_node *node = malloc(sizeof(struct list_node));
  node->data = item_data;

  // Add it to the end of the list
  node->prev = list->last;
  node->next = NULL;
  if (list->last != NULL) {
    list->last->next = node;
  }
  list->last = node;

  // If the list was empty, set the first node
  if (list->first == NULL) {
    list->first = node;
  }
}

ll_data_t list_pop(struct list *list) {
  // Get the last node
  struct list_node *node = list->last;
  if (node == NULL) {
    return 0;
  }

  // Remove it from the list
  list->last = node->prev;
  if (list->last != NULL) {
    list->last->next = NULL;
  }
  if (node == list->first) {
    list->first = NULL;
  }

  ll_data_t data = node->data;
  free(node);

  return data;
}

void list_unshift(struct list *list, ll_data_t item_data) {
  struct list_node *node = malloc(sizeof(struct list_node));
  node->data = item_data;
  node->prev = NULL;
  node->next = list->first;

  list->first = node;
}

// removes item from front of a list
ll_data_t list_shift(struct list *list) {
  struct list_node *node = list->first;
  ll_data_t data = node->data;
  list->first = node->next;
  if (list->first != NULL) {
    list->first->prev = NULL;
  } else {
    list->last = NULL;
  }
  free(node);
  return data;
}

void list_delete(struct list *list, ll_data_t data) {
  struct list_node *node = list->first;

  while (node != NULL) {
    if (node->data == data) {
      if (node->prev == NULL) {
        list->first = node->next;
      } else {
        node->prev->next = node->next;
      }
      if (node->next == NULL) {
        list->last = node->prev;
      } else {
        node->next->prev = node->prev;
      }
      free(node);
      return;
    }
    node = node->next;
  }
}
