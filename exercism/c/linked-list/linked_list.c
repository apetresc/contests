#include "linked_list.h"

#include <assert.h>
#include <stdlib.h>

struct list_node {
  struct list_node *prev, *next;
  ll_data_t data;
};

struct list {
  struct list_node *first, *last;
  size_t size;
};

struct list *list_create(void) {
  struct list *list = malloc(sizeof(*list));
  list->first = list->last = NULL;
  list->size = 0;
  return list;
}

static struct list_node *node_create(ll_data_t data, struct list_node *prev,
                                     struct list_node *next) {
  struct list_node *node = malloc(sizeof(*node));
  node->data = data;
  node->prev = prev;
  node->next = next;
  return node;
}

static ll_data_t node_delete(struct list *list, struct list_node *node) {
  ll_data_t data = node->data;
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
  list->size--;
  return data;
}

void list_destroy(struct list *list) {
  for (struct list_node *node = list->first, *next; node != NULL; node = next) {
    next = node->next;
    free(node);
  }

  free(list);
}

size_t list_count(const struct list *list) { return list->size; }

void list_push(struct list *list, ll_data_t item_data) {
  struct list_node *node = node_create(item_data, list->last, NULL);
  list->size > 0 ? (list->last->next = node) : (list->first = node);
  list->last = node;
  list->size++;
}

ll_data_t list_pop(struct list *list) {
  assert(list->size > 0);
  return node_delete(list, list->last);
}

void list_unshift(struct list *list, ll_data_t item_data) {
  struct list_node *node = node_create(item_data, NULL, list->first);
  list->size > 0 ? (list->first->prev = node) : (list->last = node);
  list->first = node;
  list->size++;
}

ll_data_t list_shift(struct list *list) {
  assert(list->size > 0);
  return node_delete(list, list->first);
}

void list_delete(struct list *list, ll_data_t data) {
  for (struct list_node *node = list->first; node != NULL; node = node->next) {
    if (node->data == data) {
      node_delete(list, node);
      return;
    }
  }
}
