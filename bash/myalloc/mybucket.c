#include "mybucket.h"
#include <sys/mman.h>
#include <pthread.h>


mybucket* bfind(mybucket* list, int size) {
	while (list != NULL && list->size < size)
		list = list->next;
	return list;
}

void badd(mybucket** list, mybucket* b) {
	if (*list != NULL) (*list)->prev = b;
	b->next = *list;
	*list = b; 
}

void bdelete(mybucket** list, mybucket* b) {
	if ((*list) == NULL) return;
	if (b == NULL) return;
	if (*list == b) {
		//head
		*list = b->next;
		if (*list != NULL) (*list)->prev = NULL;
		b->next = NULL;
		return;
	} else {
		if (b->prev != NULL) b->prev->next = b->next;
		if (b->next != NULL) b->next->prev = b->prev;
	}
	b->prev = NULL;
	b->next = NULL;
}


mybucket* bcreate(int size) {
	mybucket *b = (mybucket*)mmap(0, sizeof(mybucket)+size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
	b->mem = ((void*)b) + sizeof(mybucket);
	b->size = size;
	b->next = NULL;
	b->prev = NULL;
	b->id = pthread_self();
	return b;
}

void bdestroy(mybucket* b) {
	munmap((void*)b, sizeof(mybucket) + b->size);
}

void bdestroy_list(mybucket* list) {
	if (list == NULL) return;
	if (list->next != NULL) bdestroy_list(list->next);
	bdestroy(list);
}

