#include <stdio.h>
#include <stdlib.h>

/* int myfun(int a, int b) { */
/*   return a + b; */
/* } */

/* int ref(int *px) { */
/*   return *px++; */
/* } */

/* int* ref2() { */
/*   int *p = malloc(sizeof(int)); */
/*   *p = 8; */
/*   return p; */
/* } */

/* void mod(int *x) { */
/*   int i = 0; */
/*   scanf("%d", &i); */
/*   *x += i; */
/* } */

/* int* myal() { */
/*   int *p = malloc(sizeof(int)); */
/*   scanf("%d", p); */
/*   return p; */
/* } */

/* int main() { */
/*   //int i = 99; */
/*   //mod(&i); */
/*   int *r = myal(); */
/*   return *r; */
/* } */

/* void f(int b) { */
/*   printf("%d", b); */
/* } */

/* void h(int y) { */
/*   int x; */
/*   x = y + 1; */
/*   f(x); */
/*   f(2); */
/* } */

/* extern void h(int, int); */

/* void m(int x, int y) { */
/*   h(y,y); */
/*   h(x,x); */
/* } */

extern int g(int*, int*);

int f(int a, int b) {
  int c[3], d, e;
  d = a + 1;
  e = g(c, &b);
  return e + c[1] + b + d;

}
