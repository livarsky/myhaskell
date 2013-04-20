#include "mythread.h"
#include "mybucket.h"
#include <sys/mman.h>
#include <pthread.h>

#define N 5000
#define HASHSIZE 15000
#define MAX_BIG_SIZE 10
#define MAX_GLOB_SIZE 20
#define MAX_SMALL_SIZE 100

mybucket* glob_list, *glob_list_end;
size_t glob_size = 0;
pthread_mutex_t glob_mutex;

mythread* threads[HASHSIZE]={};

void* memcpy(void* destination, const void* source, size_t num);
void* memset(void* ptr, int value, size_t num);

void* malloc(size_t size) {
	mythread* thread = getById(pthread_self());
	if (size >= N) {
		mybucket* b = bfind(thread->big_list, size);
		if (b == 0) {
			b = bfind(glob_list, size);
			if (b == 0) {
				b = bcreate(size);
				return b->mem;
			}
			pthread_mutex_lock(&glob_mutex);
			bdelete(&glob_list, &glob_list_end, b);
			glob_size--;
			pthread_mutex_unlock(&glob_mutex);
			return b->mem;
		}
		pthread_mutex_lock(&thread->thread_mutex);
		bdelete(&thread->big_list, &thread->big_list_end, b);
		thread->bigsize--;
		pthread_mutex_unlock(&thread->thread_mutex);
		return b->mem;
	} else {
		mybucket* b = bfind(thread->small_list, size);
		if (b == 0) {
			b = bcreate(size);
			return b->mem;
		}
		pthread_mutex_lock(&thread->thread_mutex);
		bdelete(&thread->small_list, &thread->small_list_end, b);
		thread->smallsize--;
		pthread_mutex_unlock(&thread->thread_mutex);
		return b->mem;
	}
}

void free(void* ptr) {
	if (ptr == 0) return;
	mybucket* b = (mybucket*)(ptr - sizeof(mybucket));
	if (b->size >= N) {
		mythread* th = getById(pthread_self());
		pthread_mutex_lock(&th->thread_mutex);
		badd(&th->big_list, &th->big_list_end,b);
		th->bigsize++;
		pthread_mutex_unlock(&th->thread_mutex);
		if (th->bigsize > MAX_BIG_SIZE) {
			mybucket* moving = th->big_list;
			pthread_mutex_lock(&th->thread_mutex);
			bdelete(&th->big_list, &th->big_list_end, moving);
			th->bigsize--;
			pthread_mutex_unlock(&th->thread_mutex);
			pthread_mutex_lock(&glob_mutex);
			badd(&glob_list, &glob_list_end, moving);
			glob_size++;
			pthread_mutex_unlock(&glob_mutex);
			if (glob_size > MAX_GLOB_SIZE) {
				mybucket* d = glob_list;
				pthread_mutex_lock(&glob_mutex);
				bdelete(&glob_list, &glob_list_end, d);
				glob_size--;
				pthread_mutex_unlock(&glob_mutex);
				bdestroy(d);
			}
		}
	} else {
		mythread* th = getById(b->id);
		pthread_mutex_lock(&th->thread_mutex);
		badd(&th->small_list, &th->small_list_end, b);
		th->smallsize++;
		pthread_mutex_unlock(&th->thread_mutex);
		if (th->smallsize > MAX_SMALL_SIZE) {
			mybucket* d = th->small_list;
			pthread_mutex_lock(&th->thread_mutex);
			bdelete(&th->small_list, &th->small_list_end,d);
			th->smallsize--;
			pthread_mutex_unlock(&th->thread_mutex);
			bdestroy(d);
		}
	}
}

void* calloc(size_t num, size_t size) {
	void* res = malloc(num * size);
	res = memset(res, 0, num  * size);
	return res;
}


void* realloc(void* ptr, size_t size) {
	if (ptr == 0) return malloc(size);
	if (size == 0) {
		free(ptr);
		return 0;
	}
	size_t old = ((mybucket*)(ptr - sizeof(mybucket)))->size;
	if (old >= size && (size >= N || old < N)) return ptr;
	void* res = malloc(size);
	if (old < size) {
		memcpy(res, ptr, old);
	} else {
		memcpy(res, ptr, size);
	}
	free(ptr);
	return res;
}

void _init() {
    pthread_mutex_init(&glob_mutex, 0);
}

void _fini() {
    pthread_mutex_destroy(&glob_mutex);

    size_t i;
    for (i = 0; i < HASHSIZE; i++)
        destroy_thread(threads[i]);

    bdestroy_list(glob_list);
}




