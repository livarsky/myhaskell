#include <stdio.h>

#define CAPACITY 10
#define SPLITTER ('\n')

struct buffer {
    char data[CAPACITY];
    int size;
    int stored;
};

typedef struct buffer buf;

int mread(buf *b) {
    int readed = read(0, b->data + b->stored, b->size - b->stored);
    return readed >= 0 ? b->stored += readed : -1;
}

int reverse(buf *b, int len) {
    int end = len - 1;
    int start = 0;
    while (start < end) {
        char tmp = b->data[start];
        b->data[start] = b->data[end];
        b->data[end] = tmp;
        start++;
        end--;
    }
    if (len == 0) return 0; 
    return write(1, b->data, len + 1);
}

int findSplitter(buf *b) {
    int i = 0;
    while(i < b->stored && b->data[i] != SPLITTER) {
       i++;
    }
    return i;
}

void remove_seg(buf *b, int end) {
    memmove(b->data, b->data + end, b->stored - end);
    b->stored -= end;
}

int kernel() {
    buf b;
    b.size = CAPACITY;
    b.stored = 0;
  
    int skip = 0;
    while (1) {
        if (mread(&b) == -1) return -1;
        if (b.stored == 0) break;
        int end = findSplitter(&b);
        if (end == b.stored) {     
            if (end == b.size) {
                skip = 1;
                b.stored = 0;
            }
        } else {
            if (skip) {
                skip = 0;
                remove_seg(&b, end + 1);
            } else {  
                int rev_res = reverse(&b, end);
                if (rev_res == -1) return -1;
                remove_seg(&b, end + 1);
            }
        }
     }
     return 0;
}

int main() {
    int res = kernel();
    if (res == -1) return -1;
    return 0;
}
