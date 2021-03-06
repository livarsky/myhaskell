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
    b->stored += readed;
    return readed >= 0 ? readed : -1;
}

int mwrite(buf *b, int len) {
    int written = 0;
    while(written < len) {
        int tmp_w = write(1, b->data + sizeof(b->data[0]) * written, len - written);
        if (tmp_w == -1) return -1;
	    written += tmp_w;
    }
    return 0; 
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
    //if (len == 0) return 0; 
    return mwrite(b, len+1);
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
        int mr = mread(&b);
        if (mr == -1) return -1;
        if (b.stored == 0) break;
        
        int end = findSplitter(&b);
        if (end == b.stored) {     
            if (end == b.size) {
                skip = 1;
                b.stored = 0;
            } else {
                if (mr == 0) break;
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
