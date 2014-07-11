#include <stdio.h>
#include <stdlib.h>

struct tag {int i;struct tag *p;} const typedef s, *ps;

long typedef long const unsigned int *pcull, * const acpcull[];

extern int *func1(int i), func2(pcull p);

int main(int argc, char *argv[]) {
  size_t i, l;
  int rv;
  unsigned long al;
  char *endp;

  al = 0ul;
  i = sizeof(unsigned int);
  l = sizeof(unsigned long);

  if (argc == 2)
    al = strtoul(argv[1],&endp,16);

  printf("sizeof(int)=%u, sizeof(long)=%u\n",i,l);

  if (*endp == '\0' && *argv[1])
    printf("arg was %lx (%lu decimal)\n",al,al);

  return 0;
}
