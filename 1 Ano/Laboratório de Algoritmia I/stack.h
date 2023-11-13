/***
 * Eliandro Melo
 * Paulo Freitas
 * Francisca Sousa
 * Mai 22
*/

#ifndef __STACK_H__
#define __STACK_H__

#include <assert.h>

typedef enum {LONG = 1, DOUBLE = 2, CHAR = 4, STRING = 8, ARRAY = 16, BLOCO = 32} TYPE;

#define INTEGER (LONG | CHAR)
#define NUMBER (INTEGER | DOUBLE)

typedef struct stack STACK;
typedef struct data DATA;

typedef struct data {
 TYPE type;
 long LONG;
 double DOUBLE;
 char CHAR;
 char *STRING;
 STACK *ARRAY;
 char *BLOCO;
} DATA;

typedef struct stack {
 DATA letras[26];
 DATA *stack;
 int size;
 int n_elems;
} STACK;

int has_type(DATA elem, int mask);
STACK *create_stack();
void push(STACK *s, DATA elem);
DATA pop(STACK *s);
DATA top(STACK *s);
int is_empty(STACK *s);
void print_stack(STACK *s);
void print_array(STACK *s);


#endif 
