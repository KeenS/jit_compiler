/*
$ gcc -fPIC -shared cbits.c -o cbits.so
*/


#include <stdio.h>

double
putchard(double x)
{
  putchar((char) x);
  fflush(stdout);
  return 0;
}
