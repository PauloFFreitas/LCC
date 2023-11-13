/***
 * Eliandro Melo
 * Paulo Freitas
 * Francisca Sousa
 * Mar 22
*/

#include <stdio.h>
#include <string.h>
#include "stack2.h"
#include "stack2.c"

// Imprime o tipo da stack
void imprime_tipo(STACK *s) {
  if(has_type(top(s), STRING)) putchar ('S');
  if(has_type(top(s), INTEGER)) putchar ('I');
  if(has_type(top(s), DOUBLE)) putchar ('D');
  if(has_type(top(s), NUMBER)) putchar ('N');
  printf("\n");
}

int main (void) {
  STACK *s = create_stack();
  
  push_STRING(s, strdup("Hello World")); imprime_tipo(s);
  push_LONG(s, 2); imprime_tipo(s);
  push_DOUBLE(s, 3.2); imprime_tipo(s);
  push_CHAR(s, 'A'); imprime_tipo(s);
  print_stack(s);
  printf("%c\n", pop_CHAR(s));
  printf("%g\n", pop_DOUBLE(s));
  printf("%ld\n", pop_LONG(s));

  return 0;
}
