CC=gcc
CFLAGS=-O3 -c
EXECUTABLE=myserver

all: $(EXECUTABLE)

$(EXECUTABLE): myserver.o
	$(CC) -pthread myserver.o -o $@

myserver.o: myserver.c
	$(CC) $(CFLAGS) myserver.c

clean:
	rm -rf *.o $(EXECUTABLE)
