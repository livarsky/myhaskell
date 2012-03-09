CC=gcc
all: myreverse
myreverse: myreverse.o
	$(CC) myreverse.o -o myreverse
myreverse.o: myreverse.c
	$(CC) -c myreverse.c
clean:
	rm -rf *o myreverse
