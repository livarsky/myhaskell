#include "mythread.h"
#include <sys/mman.h>

#define HASHSIZE 10000

extern mythread* threads[HASHSIZE];

mythread* getById(pthread_t id) {
	if (threads[id % HASHSIZE] != 0) return threads[id % HASHSIZE];

	//create

	mythread* new_thread = (mythread*)mmap(0, sizeof(mythread), PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
	new_thread->id = id;
	new_thread->big_list = 0;
	new_thread->small_list = 0;
	new_thread->big_list_end = 0;
	new_thread->small_list_end = 0;
	new_thread->bigsize = 0;
	new_thread->smallsize = 0;
	pthread_mutex_init(&(new_thread->thread_mutex), 0);
	
	threads[id % HASHSIZE] = new_thread;
	return new_thread;
}


void destroy_thread(mythread* p) {
  if (p == 0) return;
  pthread_mutex_destroy(&(p->thread_mutex));
  bdestroy_list(p->big_list);
  bdestroy_list(p->small_list);
  munmap((void*)p, sizeof(mythread));
}

