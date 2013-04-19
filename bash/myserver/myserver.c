#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <poll.h>
#include <errno.h>
#include <sys/unistd.h>
#include <sys/fcntl.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

void* malloc(size_t size);
void* memset(void* ptr, int value, size_t num);
void* memcpy(void* destination, const void* source, size_t num);

#define CAPACITY 21
#define Q_CAPACITY 1000

typedef struct  {
	char data[CAPACITY];
	int stored;
} message;


typedef struct
{
	pthread_mutex_t mutex;
	int tail;
	message data[Q_CAPACITY];
} queue;

typedef struct
{
	pthread_mutex_t mutex, waitmutex;
	pthread_cond_t wait;
	int head;
} client_head;

struct head_list;

struct head_list {
	struct head_list *next, *prev;
	client_head head;
};
typedef struct head_list head_list;

typedef struct {
	int count;
	head_list* begin;
	head_list* end;
} heads_container;

heads_container mheads_container;
queue mqueue;

void init() {
	pthread_mutex_init(&mqueue.mutex, 0);
	mqueue.tail = 0;
	mheads_container.count = 0;
	mheads_container.begin = malloc(sizeof(head_list));
	mheads_container.begin->next = mheads_container.begin;
	mheads_container.begin->prev = mheads_container.begin;
	mheads_container.end = mheads_container.begin;
}


void destroy() {
	int i;
	head_list *curr = mheads_container.begin;		
	for(i = 0; i < mheads_container.count; i++) {		
		curr = curr->next;
		free(curr->prev);
		pthread_mutex_destroy(&(curr->head.mutex));
		pthread_mutex_destroy(&(curr->head.waitmutex));
		pthread_cond_destroy(&(curr->head.wait));
	}
	free(mheads_container.end);
	pthread_mutex_destroy(&mqueue.mutex);
}


void q_add(const message* msg) {
	pthread_mutex_lock(&mqueue.mutex);
	int i;
	head_list *curr = mheads_container.begin;		
	for(i = 0; i < mheads_container.count; i++) {
		curr = curr->next;
		if ((curr->head.head + 1) % Q_CAPACITY == mqueue.tail) {
			pthread_mutex_lock(&(curr->head.mutex));
			curr->head.head = (curr->head.head + 1) % Q_CAPACITY;
			pthread_mutex_unlock(&(curr->head.mutex));
		}	
	}
	memcpy(&(mqueue.data[mqueue.tail].data), msg, msg->stored);
	mqueue.data[mqueue.tail].stored = msg->stored;
	mqueue.tail = (mqueue.tail + 1) % Q_CAPACITY;
	pthread_mutex_unlock(&mqueue.mutex);
	curr = mheads_container.begin;		
	for(i = 0; i < mheads_container.count; i++) {
		curr = curr->next;
		pthread_mutex_lock(&(curr->head.waitmutex));
		pthread_cond_signal(&(curr->head.wait));		
		pthread_mutex_unlock(&(curr->head.waitmutex));	
	}
}


void h_add()
{
	pthread_mutex_lock(&mqueue.mutex);
	mheads_container.count++;
	head_list *end = mheads_container.end;
	end->next = malloc(sizeof(client_head));	
	end->next->prev = end;	
	end = end->next;
	mheads_container.end = end;
	end->head.head = mqueue.tail;
	pthread_mutex_init(&(end->head.mutex), 0);
	pthread_mutex_init(&(end->head.waitmutex), 0);	
	pthread_cond_init(&(end->head.wait), 0);	
	pthread_mutex_unlock(&mqueue.mutex);
}

void mfree(head_list* curr)
{
	pthread_mutex_destroy(&(curr->head.mutex));
	pthread_cond_destroy(&(curr->head.wait));
	free(curr);
}

void h_remove(head_list* curr) 
{
	pthread_mutex_lock(&mqueue.mutex);
	head_list *end = mheads_container.end, *begin = mheads_container.begin; 
	if (begin == end) {
		return;
	} else if (curr == end) {
		end->prev->next = NULL;
		mheads_container.end = end->prev;
		mfree(end);
	} else if (curr == begin) {
		return;	
	} else {
		curr->prev->next = curr->next;
		curr->next->prev = curr->prev;
		mfree(curr);
	}
	mheads_container.count--;
	pthread_mutex_unlock(&mqueue.mutex);
}

void error(const char* msg) {
    perror(msg);
    exit(1);
}


typedef struct {
	int port;
	pthread_t thr; 
} server_thread;


static int make_socket_non_blocking (int sfd)
{
  int flags, s;

  flags = fcntl (sfd, F_GETFL, 0);
  if (flags == -1)
    {
      perror ("fcntl");
      return -1;
    }

  flags |= O_NONBLOCK;
  s = fcntl (sfd, F_SETFL, flags);
  if (s == -1)
    {
      perror ("fcntl");
      return -1;
    }

  return 0;
}



typedef struct {
	int socket;
	pthread_t sender;
	pthread_t receiver;
} client_thread;


int mwrite(int socket, const message *mes) {
    int written = 0;
    int len = mes->stored;
    while(written < len) {
        int tmp_w = send(socket, mes->data + sizeof(mes->data[0]) * written, len - written, 0);
        if (tmp_w == -1) return -1;
	written += tmp_w;
    }
    return 0; 
}

static void* sender(void* client) {
	client_thread* cli = (client_thread*)client;
	struct pollfd pfd;
	pfd.fd = cli->socket;
	pfd.events = POLLOUT;
	h_add();
	head_list* curr = mheads_container.end;
	while (1) {
		pthread_mutex_lock(&(curr->head.waitmutex));
		pthread_cond_wait(&(curr->head.wait), &(curr->head.waitmutex));
		pthread_mutex_unlock(&(curr->head.waitmutex));
		
		while(curr->head.head != mqueue.tail) {
			if (poll(&pfd, 1, -1) < 0)
				error("ERROR: poll crashed [sender thread]");
			if ((pfd.revents & POLLERR) || (pfd.revents & POLLHUP) || (pfd.revents & POLLNVAL)) {
				h_remove(curr);
				close(pfd.fd);
				return;
			}
			if (pfd.revents & POLLOUT) {
				message *mes = &mqueue.data[curr->head.head];
				if (mwrite(cli->socket, mes) == -1) {
					if (errno != EAGAIN && errno != EWOULDBLOCK) {
						h_remove(curr);
						close(pfd.fd);
						return;
					}				
				}
			}
			pthread_mutex_lock(&(curr->head.mutex));
			curr->head.head++;
			curr->head.head %= Q_CAPACITY;
			pthread_mutex_unlock(&(curr->head.mutex));
		}
	}
}

static void* receiver(void* client) {
	client_thread* cli = (client_thread*)client;
	struct pollfd pfd;
	pfd.fd = cli->socket;
	pfd.events = POLLIN;
	while (1) {
		if (poll(&pfd, 1, -1) < 0)
			error("ERROR: poll crashed [sender thread]");
		if ((pfd.revents & POLLERR) || (pfd.revents & POLLHUP) || (pfd.revents & POLLNVAL)) {
			close(pfd.fd);
			return;
		}
		if (pfd.revents & POLLIN) {
			message mes;
			mes.stored = 0;
			while(mes.stored < CAPACITY) {			
				int readed = recv(cli->socket, mes.data + mes.stored, CAPACITY - mes.stored, 0);
    				mes.stored += readed;
				if (readed > 0) {
					int x;
					for(x = mes.stored - readed; x < mes.stored; x++) {
						if (mes.data[x] == '\n') {
							mes.stored = x + 1;							
							q_add(&mes);
							mes.stored = CAPACITY;
							break;		
						}
					}
				} else {
					if (errno != EAGAIN && errno != EWOULDBLOCK) {
						close(pfd.fd);
						return;				
					}
				}
			}
			while (recv(cli->socket, mes.data, CAPACITY, 0) > 0);
			if (errno != EAGAIN && errno != EWOULDBLOCK) {
				close(pfd.fd);
				return;				
			}			
		}
	}
}


static void* acceptor(void *thread) {

	server_thread* server = (server_thread*)thread;
	int sock = socket(AF_INET6, SOCK_STREAM, IPPROTO_TCP);
	if (sock < 0)
        	perror("Could not create socket");
    	struct sockaddr_in6 server_address6;
    	memset(&server_address6, 0, sizeof(server_address6));
    	server_address6.sin6_family = AF_INET6;
    	server_address6.sin6_port = htons(server->port);
    	server_address6.sin6_addr = in6addr_any;
    	int no = 0;
    	if (setsockopt(sock, IPPROTO_IPV6, IPV6_V6ONLY, (char*)&no, sizeof(no)) < 0)
        	perror("setsockopt failed");
    	if (bind(sock, (struct sockaddr*)&server_address6, sizeof(server_address6)) < 0)
        	perror("Could not bind socket");
    	if (listen(sock, 10) < 0)
       		perror("Could not start listening on socket");
    	while (1) {
        	struct sockaddr_in6 cli_address6;
        	socklen_t cli_address_len = sizeof(cli_address6);
       		int cli_socket = accept(sock, (struct sockaddr*)&cli_address6, &cli_address_len);
       		if (cli_socket < 0)
            		perror("Error on accepting");
        	if (make_socket_non_blocking(cli_socket) < 0)
			perror("make_socket_non_blocking failed");
       		char cliAddrAsStr[INET6_ADDRSTRLEN];
        	if (inet_ntop(AF_INET6, (void*)&cli_address6.sin6_addr, cliAddrAsStr, INET6_ADDRSTRLEN) == NULL)
            		perror("inet_ntop failed");
        	printf("Connection from <%s>\n", cliAddrAsStr);

        	client_thread* ct = malloc(sizeof(client_thread));
       		ct->socket = cli_socket;
        	if (pthread_create(&ct->sender, NULL, sender, (void*)ct) != 0)
            		perror("Could not create sender thread");
        	if (pthread_create(&ct->receiver, NULL, receiver, (void*)ct) != 0)
            		perror("Could not create receiver thread");
    }
}




int main(int argc, char* argv[]) {
    init();
    int n = argc - 1;
    server_thread* servs = malloc(n * sizeof(server_thread));
    int i;
    for (i = 0; i < n; ++i) {
        servs[i].port = atoi(argv[i + 1]);
        if (pthread_create(&servs[i].thr, NULL, acceptor, (void*)&servs[i]) != 0)
            perror("Could not create acceptor thread");
    }
    if (pthread_join(servs[0].thr, NULL) != 0)
            perror("Could not join to acceptor thread");
  
    return 0;
}

























