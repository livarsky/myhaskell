#ifndef _MY_THREAD_
#define _MY_THREAD_

#include "mybucket.h"
#include <pthread.h> 

typedef struct {

	pthread_t id;
	mybucket* big_list, *small_list;
	size_t bigsize, smallsize;
	pthread_mutex_t thread_mutex;
} mythread;

mythread* getById(pthread_t id);
void destroy_thread(mythread* p);

#endif
