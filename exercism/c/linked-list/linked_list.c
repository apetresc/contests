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
  *list = (struct list){.first = NULL, .last = NULL, .size = 0};
  return list;
}

static struct list_node *node_insert(struct list *list, ll_data_t data,
                                     struct list_node *prev,
                                     struct list_node *next) {
  struct list_node *node = malloc(sizeof(*node));
  *node = (struct list_node){.data = data, .prev = prev, .next = next};
  (prev != NULL) ? (prev->next = node) : (list->first = node);
  (next != NULL) ? (next->prev = node) : (list->last = node);
  list->size++;
  return node;
}

static ll_data_t node_delete(struct list *list, struct list_node *node) {
  assert(list->size > 0);
  ll_data_t data = node->data;
  (node->prev == NULL) ? list->first = node->next
                       : (node->prev->next = node->next);
  (node->next == NULL) ? list->last = node->prev
                       : (node->next->prev = node->prev);
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
  node_insert(list, item_data, list->last, NULL);
}

ll_data_t list_pop(struct list *list) { return node_delete(list, list->last); }

void list_unshift(struct list *list, ll_data_t item_data) {
  node_insert(list, item_data, NULL, list->first);
}

ll_data_t list_shift(struct list *list) {
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
