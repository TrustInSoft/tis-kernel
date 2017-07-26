#include <stdio.h>
#include <stdlib.h>

int main(void) {
  FILE *in = fopen("EXAMPLES.md", "r");
  if (!in) {
    printf("Couldn't open input file.\n");
    exit(1);
  }
  while (1) {
    char b[500];
    size_t r = fread(b, 1, 500, in);
    if (r == 0) break;
    printf("%zu bytes read:\n", r);
    printf("%.500s\n", b);
  }
}
