#include "mythread.h"
#include <sys/mman.h>

#define HASHSIZE 10000

mythread* threads[HASHSIZE]={};

mythread* getById(pthread_t id) {
	if (threads[id % HASHSIZE] != NULL) return threads[id % HASHSIZE];
	
	//create
	mythread* new_thread = (mythread*)mmap(0, sizeof(mythread), PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
	new_thread->id = id;
	new_thread->big_list = NULL;
	new_thread->small_list = NULL;
	new_thread->bigsize = 0;
	new_thread->smallsize = 0;
	pthread_mutex_init(&new_thread->thread_mutex, 0);
	
	threads[id % HASHSIZE] = new_thread;
	return new_thread;
}

void destroy_thread(mythread* p) {
	if (p == NULL) return;
  pthread_mutex_destroy(&p->thread_mutex);
  destroy_list(p->big_list);
  destroy_list(p->small_list);
  munmap((void*)p, sizeof(mythread));
}

