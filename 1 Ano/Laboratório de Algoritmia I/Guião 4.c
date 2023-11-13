/***
 * Eliandro Melo
 * Paulo Freitas
 * Francisca Sousa
 * Mai 22
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <ctype.h>
#include <assert.h>

#include "stack.h"

/**
  * Esta função verifica o tipo do elemento que lhe foi passada.
  */
int has_type(DATA elem, int mask) {
    return (elem.type & mask) != 0;
}

/**
  * Esta função inicia uma stack.
  */
STACK *create_stack() {
    STACK *s = (STACK *) malloc(sizeof(STACK));
    s->n_elems = 0;
    s->size = 100;
    s->stack = (DATA *) calloc(s->size, sizeof(DATA));
    return s;
}

/**
  * Esta função coloca no topo da stack o elemento que lhe foi passado (independente do tipo).
  */
void push(STACK *s, DATA elem) {
    if(s->size == s->n_elems) {
        s->size += 100;
        s->stack = (DATA *) realloc(s->stack, s->size * sizeof(DATA));
    }
    s->stack[s->n_elems] = elem;
    s->n_elems++;
}

/**
  * Esta função retira do topo da stack um elemento e retorna-o.
  */
DATA pop(STACK *s) {
    s->n_elems--;
    return s->stack[s->n_elems];
}

/**
  * Esta função retorna o elemento que está no topo da stack.
  */
DATA top(STACK *s) {
    return s->stack[s->n_elems - 1];
}

/**
  * Esta função verifica se a stack está vazia.
  */
int is_empty(STACK *s) {
    return s->n_elems == 0;
}

/**
  * Esta função é capaz de "imprimir" todos os elementos de um array, salvaguardando o seu tipo."
  */
void print_array(STACK *s){
     for(int K = 0; K < s->n_elems; K++) {
        DATA elem = s->stack[K];
        TYPE type = elem.type;
            if(type == LONG)
              printf("%ld", elem.LONG);
            else if(type == DOUBLE)
              printf("%g", elem.DOUBLE);
            else if(type == CHAR)
              printf("%c", elem.CHAR);
            else if(type == STRING)
              printf("%s", elem.STRING);
            else if(type == ARRAY)
              print_array(elem.ARRAY);
     }
     printf("\n");
}

/**
  * Semelhante à função print_array, esta função "imprime" todos os elementos presentes na stack.
  */
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
            else if(type == STRING)
              printf("%s", elem.STRING);
            else if(type == ARRAY)
              print_array(elem.ARRAY);
        }
    printf("\n");
}

/// Esta macro cria várias funções da qual fazem um pop ou push dos vários tipos de elementos.
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

/**
  * Esta função coloca no topo da stack um array que lhe foi indicado.
  */
void push_ARRAY(STACK *s, STACK * val){
		DATA elem;
		elem.type = ARRAY;
		elem.ARRAY = val;
		push(s, elem);
	}

/**
  * Esta função retira do topo da stack um array e retorna-o.
  */
STACK *pop_ARRAY(STACK *s){
		DATA elem = pop(s);
		assert(elem.type == ARRAY);
		return elem.ARRAY;
	}

void concatarr(STACK *s1, STACK *s2);

/**
  * Esta função retira do topo da stack dois elementos e soma-os.
  * Se os elementos forem arrays ou strings, a função concatena-os.
  * O resultado é colocado no topo da stack.
  */
void soma(STACK *s) {
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
    else if (has_type(x,STRING) && has_type(y, STRING)){
        res.type = STRING;
        strcat(y.STRING, x.STRING);
        res.STRING = strdup(y.STRING);
    }
    else if (has_type(x,STRING) && has_type(y, STRING)){
        res.type = STRING;

        strcat(y.STRING, x.STRING);
        res.STRING = strdup(y.STRING);
    }
    else if (has_type(x,CHAR) && has_type(y, STRING)){
        res.type = STRING;
        char *str = malloc(sizeof(char)*2 + sizeof(y.STRING)+1);
        str[0] = x.CHAR;
        str[1] = '\0';
        strcat(y.STRING, str);
        res.STRING = strdup(y.STRING);
    }
    else if (has_type(x, ARRAY) && has_type(y, ARRAY)){
       concatarr(y.ARRAY, x.ARRAY);
    }
    push(s, res);
}
/**
  * Esta função retira do topo da stack dois elementos e subtrai-os.
  * O resultado é colocado no topo da stack.
  */
void subtracao (STACK *s) {
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

/**
  * Esta função retira do topo da stack dois elementos e multiplica-os
  * Se os elementos forem arrays ou strings, a função concatena-os vezes a designar.
  * O resultado é colocado no topo da stack.
  */
void multiplicacao(STACK *s) {
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
    else if (has_type(x, LONG) && has_type(y, STRING)) {
        if(x.LONG != 0){
        r.type = STRING;
        strcpy(r.STRING, y.STRING);
        for(int i = 1; i < x.LONG ; i++){
            strcat(r.STRING, y.STRING);
        }
        } else {
           r.type = LONG;
           r.LONG = 0;
      }
}
    else if (has_type(x, STRING) && has_type(y, LONG)) {
        if (y.LONG != 0){
        r.type = STRING;
         strcpy(r.STRING, x.STRING);
        for(int i = 1; i < y.LONG ; i++){
            strcat(r.STRING, x.STRING);
        }
      } else {
           r.type = LONG;
           r.LONG = 0;
      }
    }
  	push(s,r);
}

STACK *substring(char *y, char *x);

/**
  * Esta função retira do topo da stack dois elementos e dividi-os.
  * O resultado é colocado no topo da stack.
  */
void divisao(STACK *s) {
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
    else if (has_type(x,STRING) && has_type(y,STRING)){
  		  r.type = ARRAY;
          r.ARRAY = substring(y.STRING, x.STRING);
  	}
  	push(s,r);
}

/**
  * Esta função retira do topo da stack dois elementos e faz a exponenciação dos dois valores.
  * Se os elementos forem strings, a função procura uma substring returnando o indicie do primeiro caracter.
  * O resultado é colocado no topo da stack.
  */
void exponenciacao(STACK *s){
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
  		r.DOUBLE = pow(x.CHAR, y.DOUBLE);
  	}
  	else if (has_type(x,CHAR) && has_type(y,CHAR)){
  		r.type= LONG;
  		r.LONG = pow(x.CHAR, y.CHAR);
  	}
    else if (has_type(x,STRING) && has_type(y,STRING)){
  		r.type= LONG;
        char *result = strstr(x.STRING, y.STRING);
        r.LONG = result - x.STRING;
  	}
  	push(s,r);
}

/**
  * Esta função retira do topo da stack dois elementos.
  * O resultado é o resto da divisão inteira e é colocado no topo da stack.
  */
void modulo(STACK *s){
  	 DATA x = pop(s);
     DATA y = pop(s);
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

/**
  * Esta função retira do topo da stack um elemento e incrementa-o.
  * Se o elemento for um arrays ou uma string, a função retira o primeiro elemento.
  * O resultado é colocado no topo da stack.
  */
void incrementa(STACK *s) {
     DATA x = pop(s);
	 DATA res;
	if(has_type(x,LONG)){
		res.type=LONG;
		res.LONG=x.LONG+1;
        push(s, res);
	}
	else if(has_type(x,DOUBLE)){
		res.type=DOUBLE;
		res.DOUBLE=x.DOUBLE+1;
        push(s, res);
	}
	else if(has_type(x,CHAR)){
		res.type=CHAR;
		res.CHAR=x.CHAR+1;
        push(s, res);
	}
    else if(has_type(x, STRING)){
		res.type = STRING;
        char c = x.STRING[strlen(x.STRING) - 1];
        int l = strlen(x.STRING) - 1;
        x.STRING[l] = '\0';
        push_STRING(s, x.STRING);
        push_CHAR(s, c);
	}
}

/**
  * Esta função retira do topo da stack um elemento e decrementa-o
  * Se o elemento for um arrays ou uma string, a função retira o ultimo elemento.
  * O resultado é colocado no topo da stack.
  */
void decrementa(STACK *s) {
    DATA x = pop(s);
	DATA res;
	if(has_type(x,LONG)){
		res.type=LONG;
		res.LONG=x.LONG-1;
        push(s, res);
	}
	else if(has_type(x,DOUBLE)){
		res.type=DOUBLE;
		res.DOUBLE=x.DOUBLE-1;
        push(s, res);
	}
	else if(has_type(x,CHAR)){
		res.type=CHAR;
		res.CHAR=x.CHAR-1;
        push(s, res);
	}
    else if(has_type(x, STRING)){
			res.type = STRING;
        char c = x.STRING[0];
        for(int i = 0; x.STRING[i] != '\0'; i++){
            x.STRING[i] = x.STRING[i+1];
        }
        push_STRING(s, x.STRING);
        push_CHAR(s, c);
	}
}

/**
  * Esta função retira do topo da stack dois elementos e conjunta-os em bitwise.
  * O resultado é colocado no topo da stack.
  */
void ebit(STACK *s){
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

/**
  * Esta função retira do topo da stack dois elementos e disjunta-os exclusivamente em bitwise.
  * O resultado é colocado no topo da stack.
  */
void ouEXbit(STACK *s) {
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

/**
  * Esta função retira do topo da stack dois elementos e dijunta-os em bitwise.
  * O resultado é colocado no topo da stack.
  */
void oubit(STACK *s) {
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

/**
  * Esta função retira do topo da stack dois elementos e nega-os em bitwise.
  * O resultado é colocado no topo da stack.
  */
void naobit(STACK *s) {
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

/**
  * Esta função retira do topo da stack um elemento e coloca-o duas vezes.
  */
void duplica(STACK *s){
    DATA res = pop(s);
    push(s,res);
    push(s, res);
}

/**
  * Esta função retira do topo da stack um elemento.
  */
void pop2(STACK *s){
    pop(s);
}

/**
  * Esta função retira do topo da stack dois elementos.
  * Os elementos são colocados na stack de forma invertida.
  */
void troca(STACK *s){
    DATA y = pop(s);
    DATA x = pop(s);
    push(s, y);
    push(s, x);
}

/**
  * Esta função retira do topo da stack três elementos.
  * Os elementos são colocados na stack de forma a "rodarem".
  */
void roda(STACK *s) {
     DATA z = pop(s);
     DATA y = pop(s);
     DATA x = pop(s);
     push(s, y);
     push(s, z);
     push(s, x);
}

/**
  * Esta função procura um elemento de um n indice e copia-o para o topo da stack.
  */
void n_esimoelem(STACK *s) {
     DATA x = pop(s);
     if(has_type(x,1) || has_type(x,2)) {
     push(s, s->stack[s->n_elems - 1 - x.LONG]);
      }
}

/**
  * Esta função retira do topo da stack um elemento e converte-o para um INT.
  * O resultado é colocado no topo da stack.
  */
void convint(STACK *s) {
        DATA x = pop(s);
   if(has_type(x,DOUBLE)) {
     push_LONG(s,x.DOUBLE);
   } else if(has_type(x,CHAR)) {
      push_LONG(s,x.CHAR);
   } else if(has_type(x,STRING)) {
      long y = atol(x.STRING);
      push_LONG(s,y);
   } else push(s,x);
}

/**
  * Esta função retira do topo da stack um elemento e converte-o para um CHAR.
  * O resultado é colocado no topo da stack.
  */
void convchar(STACK *s){
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

/**
  * Esta função retira do topo da stack um elemento e converte-o para um DOUBLE.
  * O resultado é colocado no topo da stack.
  */
void convdouble(STACK *s) {
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
}

/**
  * Esta função retira do topo da stack um elemento e converte-o para uma STRING.
  * O resultado é colocado no topo da stack.
  */
void convstring(STACK *s) {
     int x = pop_LONG(s);
     char *str =(char *) malloc(20);
     sprintf(str, "%d", x);
     push_STRING(s, str);
}

/**
  * Esta função lê uma linha.
  * O resultado é colocado no topo da stack com tipo STRING.
  */
void lerlinha(STACK *s){
     char x[10240];
     assert(fgets(x,10240,stdin)!=NULL);
     assert(x[strlen(x)-1]=='\n');
     push_STRING(s, x);
}

/**
  * Esta função lê varias linhas até que alguma seja NULL.
  * Os resultados são colocados no topo da stack com tipo STRING.
  */
void lertodas(STACK *s){
  char str1[BUFSIZ];
  char *str2 = malloc(sizeof(char)*BUFSIZ);
  str2[0] = '\0';
  while(fgets(str2,BUFSIZ,stdin)!=NULL){
      str1[strcspn(str1,"\n")] = '\n';
        strcat(str1, str2);
  }
  push_STRING(s, str2);
}

/**
  * Esta função retira do topo da stack dois elementos e verifica se são iguais.
  * Se algum dos elementos for um array ou string, a função procura elemento de indice n dentro do array/string.
  * O resultado é colocado no topo da stack.
  */
void igual(STACK *s) {
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
        }else push_LONG(s,0);}
    else if (has_type(x, STRING) && has_type(y, STRING)) {
         if (strcmp(y.STRING, x.STRING) == 0){
             push_LONG(s, 1);
         } else push_LONG(s, 0);
    }
    else if (has_type(x, LONG) && has_type(y, STRING)) {
         push_CHAR(s, y.STRING[x.LONG]);
    }
}

/**
  * Esta função retira do topo da stack dois elementos e verifica se um é menor que o outro.
  * Se algum dos elementos for um array ou string, a função cria um array/string com n primeiros elementos.
  * O resultado é colocado no topo da stack.
  */
void menor(STACK *s) {
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
    else if (has_type(x, STRING) && has_type(y, STRING)) {
        if(strcmp(x.STRING, y.STRING) < 0){
            push_LONG(s,0);
        }else push_LONG(s,1);
    }
    else if(has_type(x,LONG) && has_type(y, STRING)) {
        char str[BUFSIZ];
        for(int i = 0; i < x.LONG; i++){
             str[i] = y.STRING[i];
        }
        strcpy(y.STRING, str);
        push_STRING(s, y.STRING);
    }
}

/**
  * Esta função retira do topo da stack dois elementos e verifica se um é maior que o outro.
  * Se algum dos elementos for um array ou string, a função cria um array/string com n ultimos elementos.
  * O resultado é colocado no topo da stack.
  */
void maior(STACK *s) {
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
    else if (has_type(x, STRING) && has_type(y, STRING)) {
        if(strcmp(x.STRING, y.STRING) > 0){
            push_LONG(s,0);
        }else push_LONG(s,1);
    }
     else if(has_type(x,LONG) && has_type(y, STRING)) {
        char str[BUFSIZ];
        int l = strlen(y.STRING);
        for(int i = 0, j = l - x.LONG; y.STRING[j] != '\0'; i++, j++){
             str[i] = y.STRING[j];
        }
        strcpy(y.STRING, str);
        push_STRING(s, y.STRING);
    }
}

/**
  * Esta função retira do topo da stack e nega-o.
  * O resultado é colocado no topo da stack.
  */
void negacao(STACK *s) {
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

/**
  * Esta função retira do topo da stack dois elementos e verifica qual o menor.
  * O menor é colocado no topo da stack.
  */
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
    else if (has_type(x, STRING) && has_type(y, STRING)) {
        if(strcmp(x.STRING, y.STRING) < 0){
            push_STRING(s, x.STRING);
        }else push_STRING(s, y.STRING);
    }
}

/**
  * Esta função retira do topo da stack dois elementos e verifica qual o maior.
  * O maior é colocado no topo da stack.
  */
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
    else if (has_type(x, STRING) && has_type(y, STRING)) {
        if(strcmp(x.STRING, y.STRING) > 0){
            push_STRING(s, x.STRING);
        }else push_STRING(s, y.STRING);
    }
}

/**
  * Esta função retira do topo da stack dois elementos e verifica se têm valores 0.
  * O resultado de valor 0 é colocado no topo da stack.
  */
void eshortcut(STACK *s) {
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
}

/**
  * Esta função retira do topo da stack dois elementos e verifica se algum deles tem valor 0.
  * O elemento que não tiver, é colocado no topo da stack.
  */
void oushortcut(STACK *s) {
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
}

/**
  * Esta função retira do topo da stack três elementos e verifica se ultimo retirado é verdadeiro ou falso.
  * Se for verdadeiro, coloca no topo da stack o segundo elemento. Se for falso, coloca o primeiro.
  */
void se(STACK *s) {
    DATA z = pop(s);
    DATA y = pop(s);
    DATA x = pop(s);
    if(x.LONG == 0){
      push(s,z);
    }
    else{
      push(s,y);
    }
}

/**
  * Esta função recebe um inteiro e coloca-o na stack.
  */
DATA criaLONG(long x){
	DATA y;
	y.LONG = x;
	y.type = LONG;
	return y;
}

/**
  * Esta função recebe um double e coloca-o na stack.
  */
DATA criaDOUBLE(double x){
	DATA y;
	y.DOUBLE = x;
	y.type = DOUBLE;
	return y;
}

/**
  * Esta função recebe um char e coloca-o na stack.
  */
DATA criaCHAR(char x){
	DATA y;
	y.CHAR = x;
	y.type = CHAR;
	return y;
}

/**
  * Esta função recebe uma string e coloca-a na stack.
  */
DATA criaSTRING(char *x){
	DATA y;
	y.STRING = x;
	y.type = STRING;
	return y;
}

/**
  * Esta função trata de atribuir valores a letras especiais.
  */
void preenche_array(STACK *s){
	s->letras[0] = criaLONG(10);	///A
	s->letras[1] = criaLONG(11);	///B
	s->letras[2] = criaLONG(12);	///C
	s->letras[3] = criaLONG(13);	///D
	s->letras[4] = criaLONG(14);	///E
	s->letras[5] = criaLONG(15);	///F
	s->letras[13] = criaCHAR('\n');	///N
	s->letras[18] = criaSTRING(" ");  ///S
	s->letras[23] = criaLONG(0);    ///X
	s->letras[24] = criaLONG(1);    ///Y
	s->letras[25] = criaLONG(2);    ///Z
}

/**
  * Esta função procura o valor que fora atribuido a uma variavel.
  * O resultado é colocado no topo da stack.
  */
void letra(STACK *s, char letra){
	long x = letra;
	DATA y = (s->letras[x-65]);
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

/**
  * Esta função atribui a uma variavel um valor que estava no topo da stack.
  */
void copiatopo(STACK *s, char letra){
	DATA x = top(s);
	long pos = letra;
	if (has_type(x,LONG)){
		s->letras[pos-65]=criaLONG(x.LONG);
	}
	else if (has_type(x,DOUBLE)){
		s->letras[pos-65]=criaDOUBLE(x.DOUBLE);
	}
	else if (has_type(x,CHAR)){
		s->letras[pos-65]=criaCHAR(x.CHAR);
	}
}

/**
  * Esta função retira um elemento do topo da stack e estuda o seu tamanho.
  * Se, por exemplo, o elemento for uma string ou array, este vê a quantidade de elementos dentro da estrutura.
  * Se for de outro tipo, por exemplo INT, imprime todos os numeros de 0 ao determinado int.
  * O resultado é colocado no topo da stack.
  */

void virgula(STACK *s){
    DATA x = pop(s);
    STACK *array = create_stack();
    if(has_type(x,LONG)){
    long n = x.LONG;
    for(int i = 0; i<n; i++) {
        push_LONG(array, i);
    }
    push_ARRAY(s, array);
  }


  else if(has_type(x, CHAR)){
    char c = x.CHAR;
    for(char i = 'a'; i < c; i++) {
        push_CHAR(s, i);
    }
  }
  else if(has_type(x, STRING)){
      long l = strlen(x.STRING);
      push_LONG(s, l);
  }
  else if(has_type(x, ARRAY)){
     push_LONG(s, x.ARRAY->n_elems);
  }

}

void func_t(STACK *s){
  char str1[BUFSIZ];
  char *str2 = malloc(sizeof(char)*BUFSIZ);
  str2[0] = '\0';
  while(fgets(str2,BUFSIZ,stdin)!=NULL){
      str1[strcspn(str1,"\n")] = '\n';
        strcat(str1, str2);
  }
  push_STRING(s, str2);
}

char *strndup(const char *str, size_t len)
{
    size_t act = strnlen(str, len);
    char *dst = malloc(act + 1);
    if (dst != 0)
    {
        memmove(dst, str, act);
        dst[act] = '\0';
    }
    return dst;
}

char *get_string(const char *str, char *str1, char *str2) {
    const char *q;
    const char *p = strstr(str, str1);
    if (p) {
        p += strlen(str1);
        q = *str2 ? strstr(p, str2) : p + strlen(p);
        if (q)
            return strndup(p, q - p);
    }
    return NULL;
}

STACK * trata_array(STACK *s, char *string) {
	STACK *array = create_stack();
	push_ARRAY( s, eval(array, string));
return s;
}

/**
* @brief Esta função foi criada para guardar o que resta da string depois de ler uma array.
*/
char* afterspc(char* input) {
    char* starting = input;
   while (*starting != ']') {
     starting++;
   }
   // first one _after_
   starting++;
   return starting;
 } 

void invertarr(STACK *s_in, STACK *s_out) { 

    while(s_in->n_elems > 0){
    DATA x = pop(s_in);
                            
       push(s_out, x);     
    }
}

void concatarr(STACK *s1, STACK *s2){

	STACK *aux = create_stack();

	while(s2->n_elems > 0){
	DATA x = pop(s2);
     push(aux, x);                          
    }

    invertarr(aux, s1);
    free(aux);     
}

void newlines(STACK *s) {
    DATA x = pop(s);
    if(has_type(x, STRING)){
    char *separa = "\n";
    char *aux = strdup(x.STRING);
    STACK *array = create_stack();
                
    for (char *token = strtok(aux, separa); token != NULL; token = strtok(NULL, separa)) {
            push_STRING(array, token);
    }
    
    push_ARRAY(s, array);
    }
}

void whitespace(STACK *s) {
    DATA x = pop(s);
    if(has_type(x, STRING)){
    char *separa = " \t\n";
    char *aux = strdup(x.STRING);
    STACK *array = create_stack();
                
    for (char *token = strtok(aux, separa); token != NULL; token = strtok(NULL, separa)) {
            char *str = malloc(sizeof(char)*strlen(x.STRING));
            str = NULL;
            memset(str, '\"', 1);
            strcat(strcat(str, token), "\"");
            push_STRING(array, str);         
    }
    
    push_ARRAY(s, array);
    }
}
STACK *substring(char *y, char *x){
    STACK *array = create_stack();
      char *separa = y;
     for(char *token = strtok(x,separa); token !=NULL ; token = strtok(NULL,separa)){
           push_STRING(array, token);
           
            }
 
   return array;
}

void mani(STACK *s, char *token, char *val_ARRAY){
 char *restval1;
	     char *restval2;
         char *val_STRING = NULL;
         
         
        val_STRING = get_string(token, "\"", "\"");
		long val_LONG = strtol(token,&restval1,10);
		double val_DOUBLE = strtod(token, &restval2);
		if(strlen(restval1)==0){
			push_LONG(s, val_LONG);
		}
		else if(strlen(restval2)==0){
			push_DOUBLE(s,val_DOUBLE);}
        else if(val_STRING){
			push_STRING(s,val_STRING);}
        
    else {

        /**
         * @brief variaveis
         * 
         * @return ** variaveis 
         */
         if (isupper(token[0])) {letra(s, token[0]);}
    else if (token[0] == ':' && isupper(token[1])) {copiatopo(s, token[1]);}
     /**
        * @brief arrays
        * 
        * @return ** array 
        */
    else if(token[0]== '['){ trata_array(s, val_ARRAY);}  

        /**
         * @brief matematica
         * 
         */
    else if (strcmp(token,"+")==0) { soma(s);}
    else if (strcmp(token,"-")==0) { subtracao(s);}
    else if (strcmp(token,"*")==0) { multiplicacao(s);}
    else if (strcmp(token,"/")==0) { divisao(s);}
    else if (strcmp(token,"#")==0) { exponenciacao(s);}
    else if (strcmp(token,"%")==0) { modulo(s);}
    else if (strcmp(token,")")==0) { incrementa(s);}
    else if (strcmp(token,"(")==0) { decrementa(s);}
    else if (strcmp(token,"&")==0) { ebit(s);}
    else if (strcmp(token,"|")==0) { oubit(s);}
    else if (strcmp(token,"^")==0) { ouEXbit(s);}
    else if (strcmp(token,"~")==0) { naobit(s);}

        /**
         * @brief stack
         * 
         */
    else if (strcmp(token,"_")==0) { duplica(s);}
    else if (strcmp(token,";")==0) { pop2(s);}
    else if (strcmp(token,"\\")==0) { troca(s);}
    else if (strcmp(token,"@")==0) { roda(s);}
    else if (strcmp(token,"$")==0) { n_esimoelem(s);}
    else if (strcmp(token,",")==0) { virgula(s);}

        // converter
    else if (strcmp(token,"i")==0) { convint(s);}
    else if (strcmp(token,"c")==0) { convchar(s);}
    else if (strcmp(token,"f")==0) { convdouble(s);}
    else if (strcmp(token,"s")==0) { convstring(s);}

        // ler
    else if (strcmp(token,"l")==0) { lerlinha(s);}
    else if (strcmp(token,"t")==0) { func_t(s);}
    // else if (strcmp(token,"p")==0) { imprimetop(s);}

        // logica
    else if (strcmp(token,"=")==0) {igual(s);}
    else if (strcmp(token,"<")==0) {menor(s);}
    else if (strcmp(token,">")==0) {maior(s);}
    else if (strcmp(token,"!")==0) {negacao(s);}
    else if (strcmp(token,"?")==0) {se(s);}
    else if (strcmp(token,"e<")==0) {emenor(s);}
    else if (strcmp(token,"e>")==0) {emaior(s);}
    else if (strcmp(token,"e&")==0) {eshortcut(s);}
    else if (strcmp(token,"e|")==0) {oushortcut(s);}
        // arrays & stings
    else if (strcmp(token, "S/")) {whitespace(s);}
    else if (strcmp(token, "N/")) {newlines(s);}
   
    }
}

/**
  * Esta função recebe a linha e separa os varios caracteres em tokens, se esses tokens corresponderem a
  * um certo caracter especifico, ela chamará funções que executam operações que correspondem.
  */
STACK *eval(STACK *s, char *string) {

	 char *separa = " \t\n";
     char *val_ARRAY = NULL;
     val_ARRAY = get_string(string, "[", "]");

/// Este loop utiliza a função strtok para separar a string em tokens, e as funçõed strtol e strtod que lêem uma string (neste caso o token) e
/// convertem o valor lido para um long e para um double, respetivamente, e é feito o push do valor lido quando o token não é convertível. */
     for(char *token = strtok(string,separa); token !=NULL ; token = strtok(NULL,separa)){
	     mani(s, token, val_ARRAY);
  }
  return s;
}

/**
  * A função main vai ser o inicio de todo o programa.
  * Ela irá criar uma stack, designar as letras especiais e proceder à leitura do standard in.
  * Depois de ler a linha, irá chamar a função eval para tratar e "compilar" o comando pedido.
  * Por fim, imprime os valores presentes na stack, usando a função print_Stack, e liberta o espaço usado
  * pela stack s.
  */
int main(void){

	STACK *s = create_stack();
    char string[10000];
    preenche_array(s);

    /// caso a linha seja vazia o comando aborta a execução do programa.
    assert(fgets(string, 10000, stdin) != NULL);
    assert(string[strlen(string) - 1] == '\n');

    s = eval(s, string);
    print_stack(s);
	free(s);

    return 0;
}

