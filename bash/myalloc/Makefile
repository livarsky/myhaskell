CC=gcc

all: libmyalloc test

libmyalloc: mybucket.h mybucket.c mythread.h mythread.c myalloc.c
	$(CC) -fPIC -shared -g mybucket.c mythread.c myalloc.c -nostdlib -o libmyalloc.so

test: test.c
	$(CC) test.c -o test

clean:
	rm libmyalloc.so
