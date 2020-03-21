#include <stdio.h>
#include <sys/ioctl.h>
#include <linux/tiocl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdlib.h>
#include <unistd.h>
#include <strings.h>


struct getsel {
  char code;
  int size;
  char data[0];
} __attribute((__packed__));

struct getsel * get_selection(int size) {
  struct getsel *d = (struct getsel *) malloc(size + sizeof(struct getsel));
  if (d == NULL) {
    perror("malloc");
    exit(1);
  }

  bzero(d, size + sizeof(struct getsel));

  d->code = 18; // TIOCL_GETSEL
  d->size = size;

  int fd = open("/dev/tty",O_RDWR);
  if (ioctl(fd, TIOCLINUX, d) < 0) {
    perror("paste: TIOCLINUX");
    exit(1);
  }
  close(fd);
  return d;
}

int main(void) {
  int size = 200;
  struct getsel *d = get_selection(size);

  printf("size: %d\n",d->size);
  for (int i = 0; i < size; i++) {
    if (d->data[i]) {
      printf("data[%d] = %d\n",i, d->data[i]);
    }
  }
  d->data[d->size-1] = '\0';
  printf("string: %s\n", d->data);
  free(d);
}
