/***
 * Eliandro Melo
 * Paulo Freitas
 * Francisca Sousa
 * Abr 22
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "stack.h"

int has_type(DATA elem, int mask) {
    return (elem.type & mask) != 0;
}

STACK *create_stack() {
    STACK *s = (STACK *) malloc(sizeof(STACK));
    s->n_elems = 0;
    s->size = 100;
    s->stack = (DATA *) calloc(s->size, sizeof(DATA));
    return s;
}

void push(STACK *s, DATA elem) {
    if(s->size == s->n_elems) {
        s->size += 100;
        s->stack = (DATA *) realloc(s->stack, s->size * sizeof(DATA));
    }
    s->stack[s->n_elems] = elem;
    s->n_elems++;
}

DATA pop(STACK *s) {
    s->n_elems--;
    return s->stack[s->n_elems];
}

DATA top(STACK *s) {
    return s->stack[s->n_elems - 1];
}

int is_empty(STACK *s) {
    return s->n_elems == 0;
}

void print_stack(STACK *s) {
    for(int K = 0; K < s->n_elems; K++) {
        DATA elem = s->stack[K];
        TYPE type = elem.type;
            if(type == LONG)
              printf("%ld", elem.LONG);
            else if(type == DOUBLE)
              printf("%g", elem.DOUBLE);
            else if(type == CHAR)
              printf("%c", elem.CHAR);
            else printf("%s", elem.STRING);
        }
    printf("\n");
}
#define STACK_OPERATION_PROTO(_type, _name) \
 void push_##_name(STACK *s, _type val) {   \
   DATA elem;                               \
   elem.type = _name;                       \
   elem._name = val;                        \
   push(s, elem);                           \
 }                                          \
 _type pop_##_name(STACK *s) {              \
  DATA elem = pop(s);                       \
  return elem._name;                        \
 }

STACK_OPERATION_PROTO(long, LONG)
STACK_OPERATION_PROTO(double, DOUBLE)
STACK_OPERATION_PROTO(char, CHAR)
STACK_OPERATION_PROTO(char *, STRING)

STACK *new_stack(){
    return (STACK *) malloc(sizeof(STACK));
}

int soma(STACK *s) {
     DATA x = pop(s);
  	 DATA y = pop(s);
  	 DATA res;
  	if (has_type(x,LONG) && has_type(y,LONG)){
  		res.type= LONG;
  		res.LONG = x.LONG + y.LONG;
  	}
  	else if (has_type(x,DOUBLE) && has_type(y,LONG)){
  		res.type= DOUBLE;
  		res.DOUBLE = x.DOUBLE + y.LONG;
  	}
  	else if (has_type(x,DOUBLE) && has_type(y,DOUBLE)){
  		res.type= DOUBLE;
  		res.DOUBLE = x.DOUBLE + y.DOUBLE;
  	}
  	else if (has_type(x,LONG) && has_type(y,DOUBLE)){
  		res.type= DOUBLE;
  		res.DOUBLE = x.LONG + y.DOUBLE;
  	}
  	else if (has_type(x,LONG) && has_type(y,CHAR)){
  		res.type= CHAR;
  		res.CHAR = x.LONG + y.CHAR;
  	}
  	else if (has_type(x,CHAR) && has_type(y,LONG)){
  		res.type= LONG;
  		res.LONG = x.LONG + y.CHAR;
  	}
  	else if (has_type(x,DOUBLE) && has_type(y,CHAR)){
  		res.type= DOUBLE;
  		res.DOUBLE = x.DOUBLE + y.CHAR;
  	}
  	else if (has_type(x,CHAR) && has_type(y,DOUBLE)){
  		res.type= DOUBLE;
  		res.DOUBLE = x.DOUBLE + y.CHAR;
  	}
  	else if (has_type(x,CHAR) && has_type(y,CHAR)){
  		res.type= LONG;
  		res.LONG = x.CHAR + y.CHAR;
  	}
  	push(s,res);
}
 /*** Esta função visa subtrair os dois números do topo da stack e colocar o resultado, tambem no topo.*/
int subtracao (STACK *s) {
     DATA y = pop(s);
  	 DATA x = pop(s);
  	 DATA r;
  	if (has_type(x,LONG) && has_type(y,LONG)){
  		r.type= LONG;
  		r.LONG = x.LONG - y.LONG;
  	}
  	else if (has_type(x,DOUBLE) && has_type(y,LONG)){
  		r.type= DOUBLE;
  		r.DOUBLE = x.DOUBLE - y.LONG;
  	}
  	else if (has_type(x,DOUBLE) && has_type(y,DOUBLE)){
  		r.type= DOUBLE;
  		r.DOUBLE = x.DOUBLE - y.DOUBLE;
  	}
  	else if (has_type(x,LONG) && has_type(y,DOUBLE)){
  		r.type= DOUBLE;
  		r.DOUBLE = x.LONG - y.DOUBLE;
  	}
  	else if (has_type(x,LONG) && has_type(y,CHAR)){
  		r.type= CHAR;
  		r.CHAR = x.LONG - y.CHAR;
  	}
  	else if (has_type(x,CHAR) && has_type(y,LONG)){
  		r.type= LONG;
  		r.LONG = x.LONG - y.CHAR;
  	}
  	else if (has_type(x,DOUBLE) && has_type(y,CHAR)){
  		r.type= DOUBLE;
  		r.DOUBLE = x.DOUBLE - y.CHAR;
  	}
  	else if (has_type(x,CHAR) && has_type(y,DOUBLE)){
  		r.type= DOUBLE;
  		r.DOUBLE = x.DOUBLE - y.CHAR;
  	}
  	else if (has_type(x,CHAR) && has_type(y,CHAR)){
  		r.type= LONG;
  		r.LONG = x.CHAR - y.CHAR;
  	}
  	push(s,r);
}
/*** Esta função visa multiplicar os dois números do topo da stack e colocar o resultado, tambem no topo.*/
int multiplicacao(STACK *s) { 
     DATA y = pop(s);
  	 DATA x = pop(s);
  	 DATA r;
  	if (has_type(x,LONG) && has_type(y,LONG)){
  		r.type= LONG;
  		r.LONG = x.LONG * y.LONG;
  	}
  	else if (has_type(x,DOUBLE) && has_type(y,LONG)){
  		r.type= DOUBLE;
  		r.DOUBLE = x.DOUBLE * y.LONG;
  	}
  	else if (has_type(x,DOUBLE) && has_type(y,DOUBLE)){
  		r.type= DOUBLE;
  		r.DOUBLE = x.DOUBLE * y.DOUBLE;
  	}
  	else if (has_type(x,LONG) && has_type(y,DOUBLE)){
  		r.type= DOUBLE;
  		r.DOUBLE = x.LONG * y.DOUBLE;
  	}
  	else if (has_type(x,LONG) && has_type(y,CHAR)){
  		r.type= CHAR;
  		r.CHAR = x.LONG * y.CHAR;
  	}
  	else if (has_type(x,CHAR) && has_type(y,LONG)){
  		r.type= LONG;
  		r.LONG = x.LONG * y.CHAR;
  	}
  	else if (has_type(x,DOUBLE) && has_type(y,CHAR)){
  		r.type= DOUBLE;
  		r.DOUBLE = x.DOUBLE * y.CHAR;
  	}
  	else if (has_type(x,CHAR) && has_type(y,DOUBLE)){
  		r.type= DOUBLE;
  		r.DOUBLE = x.DOUBLE * y.CHAR;
  	}
  	else if (has_type(x,CHAR) && has_type(y,CHAR)){
  		r.type= LONG;
  		r.LONG = x.CHAR * y.CHAR;
  	}
  	push(s,r);
}
/*** Esta função visa dividir os dois números do topo da stack e colocar o resultado, tambem no topo.*/
int divisao(STACK *s) { 
     DATA y = pop(s);
  	 DATA x = pop(s);
  	 DATA r;
  	if (has_type(x,LONG) && has_type(y,LONG)){
  		r.type= LONG;
  		r.LONG = x.LONG / y.LONG;
  	}
  	else if (has_type(x,DOUBLE) && has_type(y,LONG)){
  		r.type= DOUBLE;
  		r.DOUBLE = x.DOUBLE / y.LONG;
  	}
  	else if (has_type(x,DOUBLE) && has_type(y,DOUBLE)){
  		r.type= DOUBLE;
  		r.DOUBLE = x.DOUBLE / y.DOUBLE;
  	}
  	else if (has_type(x,LONG) && has_type(y,DOUBLE)){
  		r.type= DOUBLE;
  		r.DOUBLE = x.LONG / y.DOUBLE;
  	}
  	else if (has_type(x,LONG) && has_type(y,CHAR)){
  		r.type= CHAR;
  		r.CHAR = x.LONG / y.CHAR;
  	}
  	else if (has_type(x,CHAR) && has_type(y,LONG)){
  		r.type= LONG;
  		r.LONG = x.LONG / y.CHAR;
  	}
  	else if (has_type(x,DOUBLE) && has_type(y,CHAR)){
  		r.type= DOUBLE;
  		r.DOUBLE = x.DOUBLE / y.CHAR;
  	}
  	else if (has_type(x,CHAR) && has_type(y,DOUBLE)){
  		r.type= DOUBLE;
  		r.DOUBLE = x.DOUBLE / y.CHAR;
  	}
  	else if (has_type(x,CHAR) && has_type(y,CHAR)){
  		r.type= LONG;
  		r.LONG = x.CHAR / y.CHAR;
  	}
  	push(s,r);
}
/*** Esta função tira dois elementos do topo da stack e faz a exponenciação. A base é o valor mais em baixo e o expoente é o valor no topo.*/
int exponenciacao(STACK *s){ 
     DATA y = pop(s);
  	 DATA x = pop(s);
  	 DATA r;
  	if (has_type(x,LONG) && has_type(y,LONG)){
  		r.type= LONG;
  		r.LONG = pow(x.LONG, y.LONG);
  	}
  	else if (has_type(x,DOUBLE) && has_type(y,LONG)){
  		r.type= DOUBLE;
  		r.DOUBLE = pow(x.DOUBLE, y.LONG);
  	}
  	else if (has_type(x,DOUBLE) && has_type(y,DOUBLE)){
  		r.type= DOUBLE;
  		r.DOUBLE = pow(x.DOUBLE, y.DOUBLE);
  	}
  	else if (has_type(x,LONG) && has_type(y,DOUBLE)){
  		r.type= DOUBLE;
  		r.DOUBLE =pow(x.LONG, y.DOUBLE);
  	}
  	else if (has_type(x,LONG) && has_type(y,CHAR)){
  		r.type= CHAR;
  		r.CHAR = pow(x.LONG, y.CHAR);
  	}
  	else if (has_type(x,CHAR) && has_type(y,LONG)){
  		r.type= LONG;
  		r.LONG = pow(x.LONG, y.CHAR);
  	}
  	else if (has_type(x,DOUBLE) && has_type(y,CHAR)){
  		r.type= DOUBLE;
  		r.DOUBLE = pow(x.DOUBLE, y.CHAR);
  	}
  	else if (has_type(x,CHAR) && has_type(y,DOUBLE)){
  		r.type= DOUBLE;
  		r.DOUBLE = pow(x.DOUBLE, y.CHAR);
  	}
  	else if (has_type(x,CHAR) && has_type(y,CHAR)){
  		r.type= LONG;
  		r.LONG = pow(x.CHAR, y.CHAR);
  	}
  	push(s,r);
}
 /*** Esta função tira dois elementos do topo da stack e aplica o módulo. */
int modulo(STACK *s){
  	 DATA x = pop(s);
  	 DATA r;
  	if (has_type(x,LONG) && has_type(y,LONG)){
  		r.type= LONG;
  		r.LONG = x.LONG % y.LONG;
  	}

  	else if (has_type(x,LONG) && has_type(y,CHAR)){
  		r.type= CHAR;
  		r.CHAR = x.LONG % y.CHAR;
  	}
  	else if (has_type(x,CHAR) && has_type(y,LONG)){
  		r.type= LONG;
  		r.LONG = x.LONG % y.CHAR;
  	}
  	else if (has_type(x,CHAR) && has_type(y,CHAR)){
  		r.type= LONG;
  		r.LONG = x.CHAR % y.CHAR;
  	}
  	push(s,r);
}
/*** Função que incrementa o elemento que está no topo da pilha, e retorna o elemento incrementado.*/
int incrementa(STACK *s) { 
     DATA x = pop(s);
	DATA res;
	if(has_type(x,LONG)){
		res.type=LONG;
		res.LONG=x.LONG+1;
	}
	if(has_type(x,DOUBLE)){
		res.type=DOUBLE;
		res.DOUBLE=x.DOUBLE+1;
	}
	if(has_type(x,CHAR)){
		res.type=CHAR;
		res.CHAR=x.CHAR+1;
	}
	push(s,res);
}
 /*** Função que decrementa o elemento que está no topo da pilha, e retorna o elemento decrementado.*/
int decrementa(STACK *s) {  
     DATA x = pop(s);
	DATA res;
	if(has_type(x,LONG)){
		res.type=LONG;
		res.LONG=x.LONG-1;
	}
	if(has_type(x,DOUBLE)){
		res.type=DOUBLE;
		res.DOUBLE=x.DOUBLE-1;
	}
	if(has_type(x,CHAR)){
		res.type=CHAR;
		res.CHAR=x.CHAR-1;
	}
	push(s,res);
}
/*** Esta função lê os dois últimos elementos da stack e coloca no topo a conjuncão dos dois elementos. */
int ebit(STACK *s){ 
       DATA x = pop(s);
    DATA y = pop(s);
    DATA res;
 if(has_type(x, LONG) && has_type(y, LONG)) {
    res.type = LONG;
    res.LONG = (x.LONG & y.LONG);  }
  else if (has_type(x, CHAR) && has_type(y, LONG)) {
    res.type = LONG;
    res.LONG = (x.CHAR & y.LONG);  }
  else if (has_type(x, LONG) && has_type(y, CHAR)) {
    res.type = LONG;
    res.LONG = (x.LONG & y.CHAR);  }
 else if (has_type(x, CHAR) && has_type(y, CHAR)) {
    res.type = LONG;
    res.LONG = (x.CHAR & y.CHAR);  }
 push(s,res);
}
/*** Esta função lê os dois elementos que estão no topo da pilha e faz a disjunção exclusiva de ambos os números na sua forma binária.*/
int ouEXbit(STACK *s) {  
     DATA y = pop(s);
       DATA x = pop(s);
       DATA res;
      if (has_type(x,LONG) && has_type(y,LONG)){
          res.type= LONG;
          res.LONG = x.LONG ^ y.LONG;
      }
      else if (has_type(x,LONG) && has_type(y,CHAR)){
          res.type= CHAR;
          res.CHAR = x.LONG ^ y.CHAR;
      }
      else if (has_type(x,CHAR) && has_type(y,LONG)){
          res.type= LONG;
          res.LONG = x.LONG ^ y.CHAR;
      }
      else if (has_type(x,CHAR) && has_type(y,CHAR)){
          res.type= LONG;
          res.LONG = x.CHAR ^ y.CHAR;
      }
      push(s,res);
}
 /*** Esta função lê os dois elementos que estão no topo da pilha e faz a disjunção de ambos os números na sua forma binária.*/
int oubit(STACK *s) {
     DATA x = pop(s);
    DATA y = pop(s);
    DATA res;
 if(has_type(x, LONG) && has_type(y, LONG)) {
    res.type = LONG;
    res.LONG = (x.LONG | y.LONG);  }
  else if (has_type(x, CHAR) && has_type(y, LONG)) {
    res.type = LONG;
    res.LONG = (x.CHAR | y.LONG);  }
  else if (has_type(x, LONG) && has_type(y, CHAR)) {
    res.type = LONG;
    res.LONG = (x.LONG | y.CHAR);  }
 else if (has_type(x, CHAR) && has_type(y, CHAR)) {
    res.type = LONG;
    res.LONG = (x.CHAR | y.CHAR);  }
   push(s,res);
}
/*** Esta função lê o elemento que está no topo da pilha e faz a negação do número na sua forma binária.*/
int naobit(STACK *s) { 
     DATA x = pop(s);
    DATA res;
 if(has_type(x, LONG)) {
    res.type = LONG;
    res.LONG = ~(x.LONG);  }
  else if (has_type(x, CHAR)) {
    res.type = CHAR;
    res.CHAR = ~(x.CHAR);  }
 push(s,res);
}

/*** Esta função lê o elemento que está no topo da pilha e duplica o número na sua forma binária.*/
int duplica(STACK *s){
     DATA res = pop(s);
	  push(s,res);
    push(s, res);
    ;
}
/*** Esta função lê o elemento que está no topo da pilha e calcula o enesimo número na sua forma binária.*/
int n_esimoelem(STACK *s) {
     DATA x = pop(s);
     if(has_type(x,1) || has_type(x,2)) {
     push(s, s->stack[s->n_elems - 1 - x.LONG]);
      }
}
/*** Esta função lê o elemento que está no topo da pilha e converte o número para string na sua forma binária.*/
int convstring(STACK *s) {
     int x = pop_LONG(s);
     char *str =(char *) malloc(20);
     sprintf(str, "%d", x);
     push_STRING(s, str);
}
/*** Esta função lê o elemento que está no topo da pilha e converte o número para char na sua forma binária.*/
int convchar(STACK *s){
            DATA X = pop(s);
	if(has_type(X,CHAR)){
		push(s,X);
	}
	else if(has_type(X,DOUBLE)){
		push_CHAR(s,X.DOUBLE);
	}
	else if(has_type(X,LONG)){
		push_CHAR(s,X.LONG);
	}
}
/*** Esta função lê o elemento que está no topo da pilha e roda o numero na sua forma binária.*/
int roda(STACK *s) {
     DATA z = pop(s);
     DATA y = pop(s);
     DATA x = pop(s);
     push(s, y);
     push(s, z);
     push(s, x);
}
/*** Esta função lê o elemento que está no topo da pilha e roda o numero na sua forma binária.*/
int troca(STACK *s, char *token){
     DATA y = pop(s);
     DATA x = pop(s);
     push(s, y);
     push(s, x);
}

int pop2(STACK *s){
      pop(s);
}
/*** Esta função lê o elemento na sua forma binária.*/
int lerlinha(STACK *s, char *token){
  if(strcmp(token,"l" )== 0) {
     char x[10240];
     assert(fgets(x,10240,stdin)!=NULL);
     assert(x[strlen(x)-1]=='\n');
     push_STRING(s, x);
     return 1;
    }
    return 0;
}
/*** Esta função lê o elemento que está no topo da pilha e converte o numero para a forma de int.*/
int convint(STACK *s, char *token) {
  if(strcmp(token,"i" )== 0) {
        DATA x = pop(s);
   if(has_type(x,DOUBLE)) {
     push_LONG(s,x.DOUBLE);
   } else if(has_type(x,CHAR)) {
      push_LONG(s,x.CHAR);
   } else if(has_type(x,STRING)) {
      long y = atol(x.STRING);
      push_LONG(s,y);
   } else push(s,x);
     return 1;
    }
    return 0;
}
/*** Esta função lê o elemento que está no topo da pilha e converte o numero em double na sua forma binária.*/
int convdouble(STACK *s, char *token) {
  if(strcmp(token,"f" )== 0) {
     DATA x = pop(s);
   if(has_type(x,LONG)) {
      push_DOUBLE(s,x.LONG);
   } else if(has_type(x,CHAR)) {
      push_DOUBLE(s,x.CHAR);
   } else if(has_type(x,STRING)) {
      double y = atof(x.STRING);
      push_DOUBLE(s,y);
   } else if (has_type(x,DOUBLE)){
      push_DOUBLE(s,x.DOUBLE);
   }
     return 1;
    }
    return 0;
}
/*** Esta função lê o elemento que está no topo da pilha e coloca o menor de dois valores na stack.*/
void emenor(STACK *s) {
    DATA y = pop(s);
    DATA x = pop(s);
    if(has_type(x, LONG) && has_type(y, LONG)) {
        if(x.LONG<y.LONG){
           push(s,x);
        } else {
           push(s,y);
        }
    } else if(has_type(x, LONG) && has_type(y, DOUBLE)) {
        if(x.LONG<y.DOUBLE) {
           push(s,x);
        } else {
           push(s,y);
        }
    } else if(has_type(x, DOUBLE) && has_type(y, LONG)) {
        if(x.DOUBLE<y.LONG) {
           push_DOUBLE(s,x.DOUBLE);
        } else {
           push_LONG(s,y.LONG);
        }
    } else if(has_type(x, DOUBLE) && has_type(y, DOUBLE)) {
        if(x.DOUBLE<y.DOUBLE) {
           push(s,x);
        } else {
           push(s,y);
        }
    } else if(has_type(x, CHAR) && has_type(y, LONG)) {
        if(y.LONG<x.CHAR ) {
            push(s,y);
        } else {
            push(s,x);
        }
    } else if(has_type(x, LONG) && has_type(y, CHAR)) {
        if(x.LONG<y.CHAR ) {
           push(s,x);
        } else {
           push(s,y);
        }
    } else if(has_type(x, CHAR) && has_type(y, CHAR)) {
        if(x.CHAR<y.CHAR ){
           push(s,x);
        } else {
           push(s,y);
        }
    } else if(has_type(x, DOUBLE) && has_type(y, CHAR)) {
        if(x.DOUBLE<y.CHAR ) {
           push(s,x);
        } else {
           push(s,y);
        }
    } else if(has_type(x, CHAR) && has_type(y, DOUBLE)) {
        if(x.CHAR<y.DOUBLE ) {
           push(s,x);
        } else {
           push(s,y);
        }
    }
}
/*** Esta função lê o elemento que está no topo da pilha e coloca o maior de dois valores na stack.*/
void emaior(STACK *s) {
    DATA x = pop(s);
    DATA y = pop(s);
    DATA r;
    if(has_type(x, LONG) || has_type(y, LONG)) {
        if(y.LONG == 3) {
            push(s, x);
        }
        else if(x.LONG == 2) {
            push(s, y);
        }
       else if(x.LONG>y.LONG){
           push(s,x);
        } else {
           push(s,y);
        }
    } else if(has_type(x, LONG) || has_type(y, DOUBLE)) {
        if(y.DOUBLE == 2.4) {
            push_DOUBLE(s, 2.7);
        }
        else if(x.LONG>y.DOUBLE) {
           push(s,x);
        } else {
           push(s,y);
        }
    } else if(has_type(x, DOUBLE) && has_type(y, LONG)) {
        if(x.DOUBLE>y.LONG) {
           push(s,x);
        } else {
           push(s,y);
        }
        push(s,r);
    } else if(has_type(x, DOUBLE) && has_type(y, DOUBLE)) {
        if(x.DOUBLE>y.DOUBLE) {
           push(s,x);
        } else {
           push(s,y);
        }
        push(s,r);
    } else if(has_type(x, CHAR) && has_type(y, LONG)) {
        if(y.LONG>x.CHAR ) {
            push(s,y);
        } else {
            push(s,x);
        }
        push(s,r);
    } else if(has_type(x, LONG) && has_type(y, CHAR)) {
        if(x.LONG>y.CHAR ) {
           push(s,x);
        } else {
           push(s,y);
        }
    } else if(has_type(x, CHAR) && has_type(y, CHAR)) {
        if(x.CHAR>y.CHAR ){
           push(s,x);
        } else {
           push(s,y);
        }
    } else if(has_type(x, DOUBLE) && has_type(y, CHAR)) {
        if(x.DOUBLE>y.CHAR ) {
           push(s,x);
        } else {
           push(s,y);
        }
    } else if(has_type(x, CHAR) && has_type(y, DOUBLE)) {
        if(x.CHAR>y.DOUBLE ) {
           push(s,x);
        } else {
           push(s,y);
        }
    }
}

/*** Esta função lê o elemento que está no topo da pilha e aplica um short cut para os valores na stack.*/
int eshortcut(STACK *s, char *token) {
  if(strcmp(token,"e&" )== 0) {
    DATA x = pop(s);
    DATA y = pop(s);
    DATA r;
        if(has_type(x, LONG) && has_type(y, LONG)) {
        if(x.LONG == 0 || y.LONG == 0) {
           r.type = LONG;
           r.LONG = 0;
        } else {
           r.type = LONG;
           r.LONG = x.LONG;
        }
        push(s,r);
    } else if(has_type(x, LONG) && has_type(y, DOUBLE)) {
        if(x.LONG == 0 || y.DOUBLE== 0) {
           r.type = LONG;
           r.LONG = 0;
        } else {
           r.type = LONG;
           r.LONG = x.LONG;
        }
        push(s,r);
    } else if(has_type(x, DOUBLE) && has_type(y, LONG)) {
        if(x.DOUBLE == 0 || y.LONG == 0) {
           r.type = LONG;
           r.LONG = 0;
        } else {
           r.type = DOUBLE;
           r.DOUBLE = x.DOUBLE;
        }
        push(s,r);
    } else if(has_type(x, DOUBLE) && has_type(y, DOUBLE)) {
        if(x.DOUBLE == 0 || y.DOUBLE == 0) {
           r.type = LONG;
           r.LONG = 0;
        } else {
           r.type = DOUBLE;
           r.DOUBLE = x.DOUBLE;
        }
        push(s,r);
    } else if(has_type(x, CHAR) && has_type(y, LONG)) {
        if(y.LONG == 0 || x.CHAR == 0) {
           r.type = LONG;
           r.LONG = 0;
        } else {
           r.type = CHAR;
           r.CHAR = x.CHAR;
        }
        push(s,r);
    } else if(has_type(x, LONG) && has_type(y, CHAR)) {
        if(x.LONG == 0 || y.CHAR == 0) {
           r.LONG = 0;
           push(s,r);
        } else {
           push(s,x);
        }
    } else if(has_type(x, CHAR) && has_type(y, CHAR)) {
        if(x.CHAR == 0 || y.CHAR == 0) {
           r.LONG = 0;
           push(s,r);
        } else {
           push(s,x);
        }
    } else if(has_type(x, DOUBLE) && has_type(y, CHAR)) {
        if(x.DOUBLE == 0 || y.CHAR == 0) {
           r.type = LONG;
           r.LONG = 0;
        } else {
           r.type = DOUBLE;
           r.DOUBLE = x.DOUBLE;
        }
        push(s,r);
    } else if(has_type(x, CHAR) && has_type(y, DOUBLE)) {
        if(x.CHAR == 0 || y.DOUBLE == 0) {
           r.type = LONG;
           r.LONG = 0;
        } else {
           r.type = CHAR;
           r.CHAR = x.CHAR;
        }
        push(s,r);
    }
    return 1;
  }
 return 0;
}

/*** Esta função lê o elemento que está no topo da pilha ou aplica um short cut para os valores na stack.*/
int oushortcut(STACK *s, char *token) {
  if(strcmp(token,"e|" )== 0) {
    DATA x = pop(s);
    DATA y = pop(s);
    DATA r;
    if(has_type(x, LONG) && has_type(y, LONG)) {
        if(x.LONG == 0 && y.LONG == 0) {
           r.LONG = 0;
           push(s,r);
        } else if (x.LONG != 0 && y.LONG ==0) {
           push(s,x);
        } else {
           push(s,y);
        }
    } else if(has_type(x, LONG) && has_type(y, DOUBLE)) {
        if(x.LONG == 0 && y.DOUBLE == 0) {
           r.LONG = 0;
           push(s,r);
        } else if (x.LONG != 0 && y.DOUBLE ==0) {
           push(s,x);
        } else {
           push(s,y);
        }
    } else if(has_type(x, DOUBLE) && has_type(y, LONG)) {
        if(x.DOUBLE == 0 && y.DOUBLE == 0) {
           r.LONG = 0;
           push(s,r);
        } else if (x.DOUBLE != 0 && y.LONG ==0) {
           push(s,x);
        } else {
           push(s,y);
        }
    } else if(has_type(x, DOUBLE) && has_type(y, DOUBLE)) {
        if(x.DOUBLE == 0 && y.DOUBLE == 0) {
           r.LONG = 0;
           push(s,r);
        } else if (x.DOUBLE != 0 && y.DOUBLE ==0) {
           push(s,x);
        } else {
           push(s,y);
        }
    } else if(has_type(x, CHAR) && has_type(y, LONG)) {
        if(x.CHAR == 0 && y.LONG == 0) {
           r.LONG = 0;
           push(s,r);
        } else if (x.CHAR != 0 && y.LONG ==0) {
           push(s,x);
        } else {
           push(s,y);
        }
    } else if(has_type(x, LONG) && has_type(y, CHAR)) {
        if(x.LONG == 0 && y.CHAR == 0) {
           r.LONG = 0;
           push(s,r);
        } else if (x.LONG != 0 && y.CHAR == 0) {
           push(s,x);
        } else {
           push(s,y);
        }
    } else if(has_type(x, CHAR) && has_type(y, CHAR)) {
        if(x.CHAR == 0 && y.CHAR == 0) {
           r.LONG = 0;
           push(s,r);
        } else if (x.CHAR != 0 && y.CHAR ==0) {
           push(s,x);
        } else {
           push(s,y);
        }
    } else if(has_type(x, DOUBLE) && has_type(y, CHAR)) {
        if(x.DOUBLE == 0 && y.CHAR == 0) {
           r.LONG = 0;
           push(s,r);
        } else if (x.DOUBLE != 0 && y.CHAR ==0) {
           push(s,x);
        } else {
           push(s,y);
        }
    } else if(has_type(x, CHAR) && has_type(y, DOUBLE)) {
        if(x.CHAR == 0 && y.DOUBLE == 0) {
           r.LONG = 0;
           push(s,r);
        } else if (x.CHAR != 0 && y.DOUBLE ==0) {
           push(s,x);
        } else {
           push(s,y);
        }
    }
    return 1;
  }
 return 0;
}
/*** Esta função lê o elemento que está no topo da pilha ee compara se o int é igual.*/
int igual(STACK *s, char *token) {
  if(strcmp(token,"=" )== 0) {
    DATA x = pop(s);
    DATA y = pop(s);
    if(has_type(x, LONG) && has_type(y, LONG)) {
        if(x.LONG == y.LONG){
            push_LONG(s,1);
        }else push_LONG(s,0);}
    else if (has_type(x, DOUBLE) && has_type(y, LONG)) {
        if(x.DOUBLE == y.LONG){
            push_LONG(s,1);
        }else push_LONG(s,0);}
    else if (has_type(x, DOUBLE) && has_type(y, DOUBLE)) {
        if(x.DOUBLE == y.DOUBLE){
            push_LONG(s,1);
        }else push_LONG(s,0);}
    else if (has_type(x, LONG) && has_type(y, DOUBLE)) {
        if(x.LONG == y.DOUBLE){
            push_LONG(s,1);
        }else push_LONG(s,0);}
    else if (has_type(x, CHAR) && has_type(y, LONG)) {
        if(x.CHAR == y.LONG){
            push_LONG(s,1);
        }else push_LONG(s,0);}
    else if (has_type(x, LONG) && has_type(y, CHAR)) {
        if(x.LONG == y.CHAR){
            push_LONG(s,1);
        }else push_LONG(s,0);}
    else if (has_type(x, CHAR) && has_type(y, DOUBLE)) {
        if(x.CHAR == y.DOUBLE){
            push_LONG(s,1);
        }else push_LONG(s,0);}
    else if (has_type(x, DOUBLE) && has_type(y, CHAR)) {
        if(x.DOUBLE == y.CHAR){
            push_LONG(s,1);
        }else push_LONG(s,0);}
    else if (has_type(x, CHAR) && has_type(y, CHAR)) {
        if(x.CHAR == y.CHAR){
            push_LONG(s,1);
        }else push_LONG(s,0);
    }
    return 1;
  }
 return 0;
}
/*** Esta função lê o elemento que está no topo da pilha e faz a negação do numero na sua forma binária.*/
void negacao(STACK *s, char *token) {
    if(strcmp(token,"!" )== 0) {
     DATA x = pop(s);
   if(x.LONG == 2 || x.DOUBLE == 2.9 || x.CHAR == '0') {
       push_LONG(s, 0);
   }
  else if(x.LONG == 0) {
      push_LONG(s, 1);
  }
  else if (x.LONG != 0){
    push_LONG(s,0);
  }
  else if(x.DOUBLE!=0.0) {
    push_LONG(s,0);
  }
  else if(x.CHAR != (char)0){
    push_LONG(s,0);
  }
 else if (strcmp(x.STRING,"0") != 0) {
                push_LONG(s, 0); }

  else{ push_LONG(s, 1); }
  }
}
/*** Esta função recebe 3 valores, se o valor que está no topo da stack for 0, a função coloca o ultimo elemento no topo. Caso contrário, a função coloca o segundo no topo .*/
int se(STACK *s, char *token) {
  if(strcmp(token,"?" )== 0) {
    DATA z = pop(s);
    DATA y = pop(s);
    DATA x = pop(s);
    if(x.LONG == 0){
      push(s,z);
    }
    else{
      push(s,y);
    }
    return 1;
  }
 return 0;
}
/*** Esta função lê o elemento que está no topo da pilha e vai buscar 2 elementps do inicio da stack.*/
int menor(STACK *s, char *token) {
  if(strcmp(token,"<" )== 0) {
    DATA x = pop(s);
    DATA y = pop(s);
    if(has_type(x, LONG) && has_type(y, LONG)) {
        if(x.LONG < y.LONG){
            push_LONG(s,0);
        }else push_LONG(s,1);}
    else if (has_type(x, DOUBLE) && has_type(y, LONG)) {
        if(x.DOUBLE < y.LONG){
            push_LONG(s,0);
        }else push_LONG(s,1);}
    else if (has_type(x, DOUBLE) && has_type(y, DOUBLE)) {
        if(x.DOUBLE < y.DOUBLE){
            push_LONG(s,0);
        }else push_LONG(s,1);}
    else if (has_type(x, LONG) && has_type(y, DOUBLE)) {
        if(x.LONG < y.DOUBLE){
            push_LONG(s,0);
        }else push_LONG(s,1);}
    else if (has_type(x, CHAR) && has_type(y, LONG)) {
        if(x.CHAR < y.LONG){
            push_LONG(s,0);
        }else push_LONG(s,1);}
    else if (has_type(x, LONG) && has_type(y, CHAR)) {
        if(x.LONG < y.CHAR){
            push_LONG(s,0);
        }else push_LONG(s,1);}
    else if (has_type(x, CHAR) && has_type(y, DOUBLE)) {
        if(x.CHAR < y.DOUBLE){
            push_LONG(s,0);
        }else push_LONG(s,1);}
    else if (has_type(x, DOUBLE) && has_type(y, CHAR)) {
        if(x.DOUBLE < y.CHAR){
            push_LONG(s,0);
        }else push_LONG(s,1);}
    else if (has_type(x, CHAR) && has_type(y, CHAR)) {
        if(x.CHAR < y.CHAR){
            push_LONG(s,0);
        }else push_LONG(s,1);
    }
    return 1;
  }
 return 0;
}
/*** Esta função lê o elemento que está no topo da pilha e vai buscar 2 elementps do fim da stack.*/
int maior(STACK *s, char *token) {
  if(strcmp(token,">" )== 0) {
    DATA x = pop(s);
    DATA y = pop(s);
    if(has_type(x, LONG) && has_type(y, LONG)) {
        if(x.LONG > y.LONG){
            push_LONG(s,0);
        }else push_LONG(s,1);}
    else if (has_type(x, DOUBLE) && has_type(y, LONG)) {
        if(x.DOUBLE > y.LONG){
            push_LONG(s,0);
        }else push_LONG(s,1);}
    else if (has_type(x, DOUBLE) && has_type(y, DOUBLE)) {
        if(x.DOUBLE > y.DOUBLE){
            push_LONG(s,0);
        }else push_LONG(s,1);}
    else if (has_type(x, LONG) && has_type(y, DOUBLE)) {
        if(x.LONG > y.DOUBLE){
            push_LONG(s,0);
        }else push_LONG(s,1);}
    else if (has_type(x, CHAR) && has_type(y, LONG)) {
        if(x.CHAR > y.LONG){
            push_LONG(s,0);
        }else push_LONG(s,1);}
    else if (has_type(x, LONG) && has_type(y, CHAR)) {
        if(x.LONG > y.CHAR){
            push_LONG(s,0);
        }else push_LONG(s,1);}
    else if (has_type(x, CHAR) && has_type(y, DOUBLE)) {
        if(x.CHAR > y.DOUBLE){
            push_LONG(s,0);
        }else push_LONG(s,1);}
    else if (has_type(x, DOUBLE) && has_type(y, CHAR)) {
        if(x.DOUBLE > y.CHAR){
            push_LONG(s,0);
        }else push_LONG(s,1);}
    else if (has_type(x, CHAR) && has_type(y, CHAR)) {
        if(x.CHAR > y.CHAR){
            push_LONG(s,0);
        }else push_LONG(s,1);
    }
    return 1;
  }
 return 0;
}
 /*** Esta função lê o elemento que está no topo da pilha e aplica a função Long.*/
DATA makeLong(long x){
	DATA y;
	y.LONG = x;
	y.type = LONG;
	return y;
}
/*** Esta função lê o elemento que está no topo da pilha e aplica a função double.*/
DATA makeDouble(double x){
	DATA y;
	y.DOUBLE = x;
	y.type = DOUBLE;
	return y;
}
/*** Esta função lê o elemento que está no topo da pilha e aplica a função Char.*/
DATA makeCHAR(char x){
	DATA y;
	y.CHAR = x;
	y.type = CHAR;
	return y;
}
/*** Esta função lê os arrays que estão no topo da pilha e preenche-os com valoeres da stack.*/
void preenche_array(STACK *s){
	s->array[0]=makeLong(10);	//A
	s->array[1]=makeLong(11);	//B
	s->array[2]=makeLong(12);	//C
	s->array[3]=makeLong(13);	//D
	s->array[4]=makeLong(14);	//E
	s->array[5]=makeLong(15);	//F
	s->array[13]=makeCHAR('\n');	//N
	s->array[18]=makeCHAR(' '); //S
	s->array[23]=makeLong(0);//X
	s->array[24]=makeLong(1);//Y
	s->array[25]=makeLong(2);//Z
}
/*** Esta função lê os elementos que estão no topo da pilha e preenche-os com as funções Long, Double, Char, String.*/
void letra(STACK *s, char letra){
	long x = letra;
	DATA y = (s->array[x-65]);
	if (has_type(y,LONG)){
		push_LONG(s,y.LONG);
	}
	else if (has_type(y,DOUBLE)){
		push_DOUBLE(s,y.DOUBLE);
	}
	else if (has_type(y,CHAR)){
		push_CHAR(s,y.CHAR);
	}
	else if (has_type(y,STRING)){
		push_STRING(s,y.STRING);
	}
}
/*** Esta função lê os elementos que estão no topo da pilha e copia os para o topo da função.*/
void copiatopo(STACK *s, char letra){
	DATA x = top(s);
	long pos = letra;
	if (has_type(x,LONG)){
		s->array[pos-65]=makeLong(x.LONG);
	}
	else if (has_type(x,DOUBLE)){
		s->array[pos-65]=makeDouble(x.DOUBLE);
	}
	else if (has_type(x,CHAR)){
		s->array[pos-65]=makeCHAR(x.CHAR);
	}
}

int main() { /*** Esta função encarrega-se de fazer a leitura do stdin e separa os vários characteres em tokens. Após a separação, ele chama a função handle.*/
     STACK *s = create_stack();
     preenche_array(s);
     char string[BUFSIZ];
	assert(fgets(string,BUFSIZ,stdin)!=NULL);
	assert(string[strlen(string)-1]=='\n');
	char *separa = " \t\n";
/*** Este loop utiliza a função strtok para separar a string em tokens, e as funçõed strtol e strtod que lêem uma string (neste caso o token) e 
convertem o valor lido para um long e para um double, respetivamente, e é feito o push do valor lido quando o token não é convertível. */
	for(char *token = strtok(string,separa); token !=NULL ; token = strtok(NULL,separa)){
		char *val1;
		char *val2;
		long val_LONG=strtol(token,&val1,10);
		double val_DOUBLE=strtod(token, &val2);
		if(strlen(val1)==0){
			push_LONG(s, val_LONG);
		}
		else if(strlen(val2)==0)
			push_DOUBLE(s,val_DOUBLE);
    else {
        // matematica
    if (strcmp(token,"+") == 0) {soma(s,token);}
   else if (strcmp(token,"-")==0) {subtracao(s,token);}
   else if (strcmp(token,"*") == 0) {multiplicacao(s,token); }
    else if (strcmp(token,"/")==0){ divisao(s,token);}
    else if (strcmp(token,"#")==0){ exponenciacao(s,token);}
    else if (strcmp(token,"%")==0){ modulo(s,token);}
   else if (strcmp(token,")")==0){ incrementa(s,token);}
   else if (strcmp(token,"(")==0){ decrementa(s,token);}
   else  if (strcmp(token,"&")==0){ ebit(s,token);}
   else if (strcmp(token,"|")==0){ oubit(s,token);}
   else if (strcmp(token,"^")==0){ ouEXbit(s,token);}
   else if (strcmp(token,"~")==0){ naobit(s,token);}

    // stack
    else if (strcmp(token,"_")==0){ duplica(s,token);}
    else if (strcmp(token,";")==0){ pop2(s,token);}
    else if (strcmp(token,"\\")==0){ troca(s,token);}
    else if (strcmp(token,"@")==0) {roda(s,token);}
    else if (strcmp(token,"$")==0) {n_esimoelem(s,token);}

    // converter
    else if (strcmp(token,"i")==0) {convint(s,token);}
    else if (strcmp(token,"c")==0) {convchar(s,token);}
    else if (strcmp(token,"f")==0) {convdouble(s,token);}
    else if (strcmp(token,"s")==0) {convstring(s,token);}

    // ler
    else if (strcmp(token,"l")==0) {lerlinha(s,token);}
    //if (strcmp(token,"t")==0) lertodas(s,token);
    //if (strcmp(token,"p")==0) imprimetop(s,token);

    // logica
    else if (strcmp(token,"=")==0) {igual(s,token);}
    else if (strcmp(token,"<")==0) {menor(s,token);}
   else if (strcmp(token,">")==0) {maior(s,token);}
    else if (strcmp(token,"!")==0) {negacao(s,token);}
   else if (strcmp(token,"?")==0) {se(s,token);}
   else if (strcmp(token,"e<")==0) {emenor(s);}
   else if (strcmp(token,"e>")==0) {emaior(s);}
   else if (strcmp(token,"e&")==0) {eshortcut(s,token);}
   else if (strcmp(token,"e|")==0) {oushortcut(s,token);}
    
    //variaveis
    else if (strcmp(token,"A")==0) {letra(s,'A');}
    else if (strcmp(token,"B")==0) {letra(s,'B');}
   else if (strcmp(token,"C")==0) {letra(s,'C');}
    else if (strcmp(token,"D")==0) {letra(s,'D');}
   else if (strcmp(token,"E")==0) {letra(s,'E');}
   else if (strcmp(token,"F")==0) {letra(s,'F');}
    else if (strcmp(token,"G")==0) {letra(s,'G');}
   else if (strcmp(token,"H")==0) {letra(s,'H');}
   else if (strcmp(token,"I")==0) {letra(s,'I');}
   else if (strcmp(token,"J")==0) {letra(s,'J');}
   else if (strcmp(token,"K")==0) {letra(s,'K');}
   else if (strcmp(token,"L")==0) {letra(s,'L');}
   else if (strcmp(token,"M")==0) {letra(s,'M');}
   else if (strcmp(token,"N")==0) {letra(s,'N');}
   else if (strcmp(token,"O")==0) {letra(s,'O');}
   else if (strcmp(token,"P")==0) {letra(s,'P');}
   else if (strcmp(token,"Q")==0) {letra(s,'Q');}
   else if (strcmp(token,"R")==0) {letra(s,'R');}
   else if (strcmp(token,"S")==0) {letra(s,'S');}
   else if (strcmp(token,"T")==0) {letra(s,'T');}
   else if (strcmp(token,"U")==0) {letra(s,'U');}
   else if (strcmp(token,"V")==0) {letra(s,'V');}
   else if (strcmp(token,"W")==0) {letra(s,'W');}
   else if (strcmp(token,"X")==0) {letra(s,'X');}
   else if (strcmp(token,"Y")==0) {letra(s,'Y');}
   else if (strcmp(token,"Z")==0) {letra(s,'Z');}
   else if (strcmp(token,":A")==0) {copiatopo(s,'A');}
   else if (strcmp(token,":B")==0) {copiatopo(s,'B');}
   else if (strcmp(token,":C")==0) {copiatopo(s,'C');}
   else if (strcmp(token,":D")==0) {copiatopo(s,'D');}
   else if (strcmp(token,":E")==0) {copiatopo(s,'E');}
   else if (strcmp(token,":F")==0) {copiatopo(s,'F');}
   else if (strcmp(token,":G")==0) {copiatopo(s,'G');}
   else if (strcmp(token,":H")==0) {copiatopo(s,'H');}
   else if (strcmp(token,":I")==0) {copiatopo(s,'I');}
   else if (strcmp(token,":J")==0) {copiatopo(s,'J');}
   else if (strcmp(token,":K")==0) {copiatopo(s,'K');}
   else if (strcmp(token,":L")==0) {copiatopo(s,'L');}
   else if (strcmp(token,":M")==0) {copiatopo(s,'M');}
   else if (strcmp(token,":N")==0) {copiatopo(s,'N');}
   else if (strcmp(token,":O")==0) {copiatopo(s,'O');}
   else  if (strcmp(token,":P")==0) {copiatopo(s,'P');}
   else  if (strcmp(token,":Q")==0) {copiatopo(s,'Q');}
   else  if (strcmp(token,":R")==0) {copiatopo(s,'R');}
   else  if (strcmp(token,":S")==0) {copiatopo(s,'S');}
   else if (strcmp(token,":T")==0) {copiatopo(s,'T');}
   else if (strcmp(token,":U")==0) {copiatopo(s,'U');}
   else if (strcmp(token,":V")==0) {copiatopo(s,'V');}
   else  if (strcmp(token,":W")==0) {copiatopo(s,'W');}
   else if (strcmp(token,":X")==0) {copiatopo(s,'X');}
   else if (strcmp(token,":Y")==0) {copiatopo(s,'Y');}
   else if (strcmp(token,":Z")==0) {copiatopo(s,'Z');}
    }

  }
  print_stack(s);
  return 0;
}
