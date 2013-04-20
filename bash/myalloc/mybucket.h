#ifndef _MY_BUCKET_H
#define _MY_BUCKET_H

#include <pthread.h>
#include <sys/mman.h>

struct bucket {
	void* mem;
	size_t size;	
	struct bucket *next, *prev;
	pthread_t id;
};

typedef struct bucket mybucket;


mybucket* bfind(mybucket*, int);
void badd(mybucket** list, mybucket** list_end, mybucket* b);
void bdelete(mybucket** list, mybucket** list_end, mybucket* b);
mybucket* blast(mybucket* list);
mybucket* bcreate(int size);
void bdestroy(mybucket* b);
void bdestroy_list(mybucket* list);

#endif
