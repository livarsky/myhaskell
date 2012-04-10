CC=gcc

all: libmyalloc

libmyalloc: mybucket.h mybucket.c mythread.h mythread.c myalloc.c
	$(CC) -fPIC -shared -g mybucket.c mythread.c myalloc.c -nostdlib -o libmyalloc.so

clean:
	rm libmyalloc.so
