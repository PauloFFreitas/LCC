/***
 * Eliandro Melo
 * Paulo Freitas
 * Francisca Sousa
 * Mar 22
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "stack.h"

int adi (STACK *s, char *token) { /*** Esta função visa somar os dois números do topo da stack e colocar o resultado, tambem no topo.*/
  if(strcmp(token, "+")== 0) {
     int x = pop(s);
     int y = pop(s);
     push(s, x+y);
     return 1;
   }
   return 0;
}
int subt (STACK *s, char *token) { /*** Esta função visa subtrair os dois números do topo da stack e colocar o resultado, tambem no topo.*/
  if(strcmp(token, "-")== 0) {
     int y = pop(s);
     int x = pop(s);
     push(s, x-y);
     return 1;
    }
    return 0;
}

int mult(STACK *s, char *token) { /*** Esta função visa multiplicar os dois números do topo da stack e colocar o resultado, tambem no topo.*/
   if(strcmp(token, "*")== 0) {
     int x = pop(s);
     int y = pop(s);
     push(s, x*y);
     return 1;
    }
    return 0;
}

int divi(STACK *s, char *token) { /*** Esta função visa dividir os dois números do topo da stack e colocar o resultado, tambem no topo.*/
   if(strcmp(token, "/")== 0) {
     int y = pop(s);
     int x = pop(s);
     push(s, x/y);
     return 1;
    }
    return 0;
}

int expo(STACK *s, char *token){ /*** Esta função tira dois elementos do topo da stack e faz a exponenciação. A base é o valor mais em baixo e o expoente é o valor no topo.*/
    if(strcmp(token, "#")==0) { /*** A função coloca o resultado no topo da pilha. */
            int e = pop(s);
            int b = pop (s);
            push(s, pow(b,e));
            return 1;
    }
    return 0;
}

int modl(STACK *s, char *token){ /*** Esta função tira dois elementos do topo da stack e aplica o módulo. */
    if(strcmp(token, "%")==0) { /*** A função coloca o resultado no topo da pilha. */
            int b = pop(s);
            int a = pop (s);
            push(s, a % b);
             return 1;
    }
    return 0;
}

int inc(STACK *s, char *token) { /*** Função que incrementa o elemento que está no topo da pilha, e retorna o elemento incrementado.*/
  if(strcmp(token, ")")== 0) {
     int x = pop(s);
     push(s, x+1);
     return 1;
    }
    return 0;
}

int decre(STACK *s, char *token) {   /*** Função que decrementa o elemento que está no topo da pilha, e retorna o elemento decrementado.*/
  if(strcmp(token, "(")== 0) {
     int x = pop(s);
     push(s, x-1);
     return 1;
   }
   return 0;
}

int andbit(STACK *s, char *token){ /*** Esta função lê os dois últimos elementos da stack e coloca no topo a conjuncão dos dois elementos. */
    if(strcmp(token,"&")==0){
        int seg = pop(s);
        int pri = pop(s);
        push(s,pri & seg);
        return 1;
    }
    return 0;
}

int xorbit(STACK *s, char *token) {  /*** Esta função lê os dois elementos que estão no topo da pilha e faz a disjunção exclusiva de ambos os números na sua forma binária.*/
  if(strcmp(token, "^")== 0) {
     int y = pop(s);
     int x = pop(s);
     push(s, x^y);
     return 1;
   }
   return 0;
}

int orbit(STACK *s, char *token) { /*** Esta função lê os dois elementos que estão no topo da pilha e faz a disjunção de ambos os números na sua forma binária.*/
  if(strcmp(token, "|")== 0) {
     int y = pop(s);
     int x = pop(s);
     push(s, x | y);
     return 1;
   }
   return 0;
}

int notbit(STACK *s, char *token) { /*** Esta função lê o elemento que está no topo da pilha e faz a negação do número na sua forma binária.*/
  if(strcmp(token, "~")== 0) {
     int x = pop(s);
     push(s, ~x);
     return 1;
   }
   return 0;
}

int val(STACK *s, char *token) { /*** Se algum dos tokens apresentados não corresponderem a operações, esta função considera como um valor.*/
    int val;
     sscanf(token, "%d", &val);
     push(s, val);
}

void handle(STACK *s, char *token) { /*** Esta função "gerencia" os teste para determinados simbolos correspondem a operações.*/
   adi(s, token) || subt(s, token) || mult(s, token) || divi(s, token) || expo(s,token) || andbit(s,token) || xorbit(s, token) || notbit(s, token) || inc(s, token) || modl(s, token) || orbit(s, token) || decre(s, token) || val(s, token) ;
}

int main() { /*** Esta função encarrega-se de fazer a leitura do stdin e separa os vários characteres em tokens. Após a separação, ele chama a função handle.*/
     STACK *s = new_stack();
     char line[BUFSIZ];
     char token[BUFSIZ];


     if(fgets(line, BUFSIZ, stdin) != NULL) {
         while(sscanf(line, "%s%[^\n]", token, line)== 2) {
            handle(s, token);
         }
         handle(s, token);

         for(int i = 1; i<= s->sp; i++) /*** Após a resolução das operações, a função main "imprime" os valores presentes na pilha.*/
            printf("%d", s->stack[i]);
         printf("\n");
     }
     return 0;
}
