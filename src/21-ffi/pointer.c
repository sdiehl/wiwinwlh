#include <stdio.h>

void invoke(void (*fn)(int))
{
  int n = 42;
  printf("Inside of C, now we'll call Haskell.\n");
  fn(n);
  printf("Back inside of C again.\n");
}
