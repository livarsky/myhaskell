#include "mybucket.h"
#include <sys/mman.h>
#include <pthread.h>


mybucket* bfind(mybucket* list, int size) {
	while (list != 0 && list->size < size)
		list = list->next;
	return list;
}

void badd(mybucket** list, mybucket** list_end, mybucket* b) {
	if (*list_end == 0) *list_end = b;	
	if (*list != 0) (*list)->prev = b;
	b->next = *list;
	*list = b; 
}

void bdelete(mybucket** list, mybucket** list_end, mybucket* b) {
	if ((*list) == 0) return;
	if (b == 0) return;
	if (b == *list && b == *list_end) {
		*list = 0;
		*list_end = 0;
		return;	
	}	
	if (*list == b) {
		//head
		*list = b->next;
		if (*list != 0) (*list)->prev = 0;
		b->next = 0;
		return;
	} else if (b == *list_end) {
		*list_end = b->prev;
		(*list_end)->next = 0;
		b->prev = 0;
		return;	
	}
	if (b->prev != 0) b->prev->next = b->next;
	if (b->next != 0) b->next->prev = b->prev;
	
	b->prev = 0;
	b->next = 0;
}


mybucket* bcreate(int size) {
	mybucket *b = (mybucket*)mmap(0, sizeof(mybucket)+size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
	b->mem = ((void*)b) + sizeof(mybucket);
	b->size = size;
	b->next = 0;
	b->prev = 0;
	b->id = pthread_self();
	return b;
}

void bdestroy(mybucket* b) {
	munmap((void*)b, sizeof(mybucket) + b->size);
}

void bdestroy_list(mybucket* list) {
	if (list == 0) return;
	if (list->next != 0) bdestroy_list(list->next);
	bdestroy(list);
}

