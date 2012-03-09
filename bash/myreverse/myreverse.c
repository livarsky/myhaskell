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
    return readed > 0 ? b->stored += readed : -1;
}

int reverse(buf *b, int start, int len) {
  int end = start + len - 1;
  int l = start;
  while (start < end) {
     char tmp = b->data[start];
     b->data[start] = b->data[end];
     b->data[end] = tmp;
     start++;
     end--;
  }
  if (len == 0) return 0; 
  return write(1, b->data + l, len+1);
}

int findSplitter(buf *b, int from) {
   int i = from;
   while(i < b->stored && b->data[i] != SPLITTER) {
     i++;
   }
   return i;
}

void remove_seg(buf *b, int st, int end) {
   memmove(b->data + st, b->data + end, b->stored - end);
   b->stored -= (end - st);
}

int kernel() {
  buf b;
  b.size = CAPACITY;
  b.stored = 0;
  
  int seg_start = 0, seg_end = 0;
  int skip = 0;
  mread(&b);
  while (1) {
    seg_end = findSplitter(&b, seg_start);
    if (seg_end == b.stored) {     
      if (seg_start == 0 && seg_end == b.size) {
        skip = 1;
        b.stored = 0;
      } else {
        memmove(b.data, b.data + seg_start, seg_end + 1 - seg_start);
      }
    } else {
      if (skip) {
        skip = 0;
        remove_seg(&b, seg_start, seg_end + 1);
      } else {  
	    int res = reverse(&b, seg_start, seg_end - seg_start);
        remove_seg(&b, seg_start, seg_end + 1);
      }
    }
    int res = mread(&b);
    if (res == -1 && b.stored == 0) break;
  }
  return 0;
}

int main() {
  kernel();
  return 0;
}
