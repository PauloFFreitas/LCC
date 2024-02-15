import ply.lex as lex

tokens = (
   # TIPOS - feito em yacc
   'INT','NUM','STR','ID',

   # OPERACOES FUNCOES
   'WRITE','READ','IF','ELSE',
   'WHILE',

   # OPERACOES LOGICAS - feito em yacc
   'LPAREN','RPAREN','AND','OR',
   'NOT','EQUALS','GT','LT',
   'GE','LE',

   # OPERAÇOES ARITMETICAS - feito em yacc
   'PLUS','MINUS','TIMES','DIVIDE',
   'RESTO'
   
)

################

# literals - simbolos que têm um significado 
literals = ['+','-','*','/','%','=',
            '(',')','.',',','<','>',
            '!','{','}','[',']']


#################

# DEFINICAO TOKENS

# TIPOS
def t_INT(t):
   r'INT'
   return t

def t_NUM(t):
   r'-?\d+'
   t.value = int(t.value)
   return t
   return t

# OPERACOES FUNCOES
def t_WRITE(t):
   r'WRITE'
   return t


def t_READ(t):
   r'READ'
   return t

def t_IF(t):
   r'IF'
   return t

def t_ELSE(t):
   r'ELSE'
   return t

def t_WHILE(t):
   r'WHILE'
   return t

# OPERACOES LOGICAS

t_LPAREN = r'\('
t_RPAREN = r'\)'
t_EQUALS = r'=='

def t_AND(t):
   r'AND'
   return t

def t_OR(t):
   r'OR'
   return t

def t_NOT(t):
   r'NOT'
   return t

t_GT = r'>'
t_LT = r'<'
t_GE = r'>='
t_LE = r'<='

# OPERACOES ARITMETICAS
t_PLUS = r'\+'
t_MINUS = r'-'
t_TIMES = r'\*'
def t_DIVIDE(t):
   r'/'
   return t
t_RESTO = r'%'

# CASOS ESPECIAIS
def t_ID(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    return t

t_ignore = " \t"

def t_newline(t):
   r'\n+'
   t.lexer.lineno += len(t.value)

def t_error(t):
   print('Illegal character: ', t.value[0])
   t.lexer.skip(1)

lexer = lex.lex()

'''try:
    with open('textrunwhilesum.txt','r') as file:
      inp = file.read()
      lexer.input(inp)
      tok = lexer.token()
      while tok:
         print(tok)
         tok = lexer.token()
except KeyboardInterrupt:
    print()'''