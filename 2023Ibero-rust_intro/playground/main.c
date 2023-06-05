#include <stdlib.h>
#include <stdio.h>

int main() {
  int *x = malloc(sizeof(int));
  int y = 2;
  x = &y;
  printf("%d", *x);
}
