from lex import tokens
import ply.yacc as yacc

precedence = (('right', 'UMINUS'),)

def p_init(p):
    '''Init : Declaracoes Funcao
            | Funcao
    '''
    if len(p) == 2:
        parser.assembly = f'START\n{p[1]}STOP'
    else:
        parser.assembly = f'{p[1]}START\n{p[2]}STOP'

def p_funcao(p):
    'Funcao      : Cmds'
    p[0] = f'{p[1]}'

def p_funcao_rec(p):
    'Funcao      : Funcao Cmds'
    p[0] = f'{p[1]}{p[2]}'

def p_Arrays(p):
    '''Arrays : Array ',' Arrays
              | Array
    '''
    if len(p) > 2:
        p[0] = f'{p[1]}{p[3]}'
    else:
        p[0] = f'{p[1]}'

def p_Array(p):
    '''Array : NUM ',' Array
             | NUM
    '''
    if len(p) > 2:
        p[0] = f'{p[1]}'
    else:
        p[0] = f'{p[1]}'

def p_cmds(p):
    '''Cmds : Cmds Cmd
            | Cmd
    '''
    if len(p) > 2:
        p[0] = f'{p[1]}{p[2]}'
    else:
        p[0] = f'{p[1]}'

def p_cmd_writes(p):
    '''Cmd_Writes : WRITE LPAREN Cmd_Write Writes RPAREN
                  | WRITE LPAREN Cmd_Write RPAREN
    '''
    if len(p) > 5:
        p[0] = f'{p[3]}{p[4]}'
    else:
        p[0] = f'{p[3]}'

def p_writes(p):
    '''Writes : ',' Cmd_Write
              | ',' Cmd_Write Writes
    '''
    if len(p) > 3:
        p[0] = f'{p[2]}{p[3]}'
    else:
        p[0] = f'{p[2]}'


def p_cmd_write(p):
    '''Cmd_Write : Exp
    '''
    p[0] = f'{p[1]}WRITEF\nPUSHS "\\n"\nWRITES\n'


def p_cmd_writeArray(p):
    '''Cmd_Write : ID '[' Factor ']'
    '''
    if p[1] in p.parser.registers:
        if p[1] not in p.parser.ints:
            if len(p.parser.registers.get(p[1])) == 2:
                array = ""
                for i in range(p.parser.registers.get(p[1])[1]):
                    array += f'PUSHGP\nPUSHI {p.parser.registers.get(p[1])[0]} \nPADD\nPUSHI {i}\nLOADN\n'
                    array += f'WRITEI\n'
                    array += f'PUSHS " "\nWRITES\n'
                p[0] = array + f'PUSHS "\\n"\nWRITES\n'
        else:
            print(f"Erro: {p[1]} não é um array.")
            parser.success = False
    else:
        print("Erro: Variável não definida.")
        parser.success = False

def p_cmd_writeMatrix(p):
    '''Cmd_Write : ID '[' Factor ']' '[' Factor ']'
    '''
    if p[1] in p.parser.registers:
        if p[1] not in p.parser.ints and len(p.parser.registers.get(p[1])) == 3:
            rows = p.parser.registers.get(p[1])[1]
            cols = p.parser.registers.get(p[1])[2]

            matrix_output = ""
            for i in range(rows):
                for j in range(cols):
                    matrix_output += f'PUSHGP\nPUSHI {p.parser.registers.get(p[1])[0]}\nPADD\n'
                    matrix_output += f'PUSHI {i}\nPUSHI {cols}\nMUL\n'  # Corrected multiplication
                    matrix_output += f'PUSHI {j}\nADD\nLOADN\n'
                    matrix_output += 'WRITEI\nPUSHS " "\nWRITES\n'
                matrix_output += 'PUSHS "\\n"\nWRITES\n'
            p[0] = matrix_output
        else:
            print(f"Erro: {p[1]} não é uma matriz.")
            parser.success = False
    else:
        print("Erro: Variável não definida.")
        parser.success = False



def p_Declaracoes(p):
    '''Declaracoes : Declaracao
                   | Declaracoes Declaracao
    '''
    if len(p) == 2:
        p[0] = f'{p[1]}'
    else:
        p[0] = f'{p[1]}{p[2]}'

def p_Declaracao_Int(p):
    '''Declaracao : INT ID
                  | INT ID '=' NUM
    '''
    num = 0
    if len(p) > 3:
        num = p[4]
    if p[2] not in p.parser.registers:
        p.parser.registers.update({p[2] : p.parser.gp})
        p[0] = f'PUSHI {num}\n'
        p.parser.ints.append(p[2])
        p.parser.gp += 1
    else:
        print("Error: Variável já existe.")
        parser.success = False

def p_Declaracao_Array(p):
    '''Declaracao : INT ID '[' NUM ']'
                  | INT ID '[' NUM ']' '=' '[' Array ']'
    '''
    #

    if p[2] not in p.parser.registers:
        p.parser.registers.update({p[2]: (p.parser.gp, int(p[4]))})
        p[0] = f'PUSHN {p[4]}\n'
        p.parser.gp += int(p[4])
    else:
        print("Erro: Variável já existe.")
        parser.success = False

def p_Declaracao_Matriz(p):
    '''Declaracao : INT ID '[' NUM ']' '[' NUM ']'
                  | INT ID '[' NUM ']' '[' NUM ']' '=' '[' Arrays ']'
    '''
    if p[2] not in p.parser.registers:
        p.parser.registers.update({p[2]: (p.parser.gp, int(p[4]), int(p[7]))})
        size = int(p[4]) * int(p[7])
        p[0] = f'PUSHN {str(size)}\n'
        p.parser.gp += size
    else:
        print("Erro: Variável já existe.")
        parser.success = False

def p_cmd_atr(p):
    '''Cmd : ID '=' Exp'''
    if p[1] in p.parser.registers:
        if p[1] in p.parser.ints:
            p[0] = f'{p[3]}STOREG {p.parser.registers.get(p[1])}\n'
        else:
            print("Error: Variável não é int")
            parser.success = False
    else:
        print("Error: Variável não definida.")
        parser.success = False

#o padd do 1 adicionado antees é aqui
def p_cmd_array(p):
    '''
    Cmd : ID '[' Factor ']' '=' Exp
    '''
    if p[1] in p.parser.registers:
        if p[1] not in p.parser.ints and len(p.parser.registers.get(p[1])) == 2:
            p[0] = f'PUSHGP\nPUSHI {p.parser.registers.get(p[1])[0]}\nPADD\n{p[3]}{p[6]}STOREN\n'
        else:
            print(f"Erro: Variável {p[1]} não é um array.")
            parser.success = False
    else:
        print("Erro: Variável não definida.")
        parser.success = False

#falta ver isto
def p_cmd_Matriz(p):
    '''
    Cmd : ID '[' Factor ']' '[' Factor ']' '=' Exp
    '''
    if p[1] in p.parser.registers:
        if p[1] not in p.parser.ints and len(p.parser.registers.get(p[1])) == 3:
            c = p.parser.registers.get(p[1])[2]
            p[0] = f'PUSHGP\nPUSHI {p.parser.registers.get(p[1])[0]}\nPADD\n{p[3]}PUSHI {c} \nMUL{p[6]}\nADD\n{p[9]}STOREN\n'
        else:
            print(f"Erro: Variável {p[1]} não é uma matriz.")
            parser.success = False
    else:
        print("Erro: Variável não definida.")
        parser.success = False

def p_factor_par(p):
    'Factor : LPAREN Exp RPAREN'
    p[0] = p[2]

def p_exp_operations(p):
    '''Exp : Exp PLUS Term
           | Exp MINUS Term
           | Term
    '''
    if len(p) == 4:
        if p[2] == '+':
            p[0] = f'{p[1]}{p[3]}ADD\n'
        elif p[2] == '-':
            p[0] = f'{p[1]}{p[3]}SUB\n'
    else:
        p[0] = p[1]

def p_term_factor(p):
    '''Term : Term DIVIDE Factor
            | Term TIMES Factor
            | Term RESTO Factor
            | Factor
    '''
    if len(p) > 2:
        if p[2] == '/':
            p[0] = f'{p[1]}{p[3]}DIV\n'
        elif p[2] == '*':
            p[0] = f'{p[1]}{p[3]}MUL\n'
        elif p[2] == '%':
            p[0] = f'{p[1]}{p[3]}MOD\n'
    else:
        p[0] = p[1]

def p_factor_id(p):
    '''Factor : ID
    '''
    if p[1] in p.parser.registers:
        if p[1] in p.parser.ints:
            p[0] = f'PUSHG {p.parser.registers.get(p[1])}\n'
        else:
            print("Erro: Variável não é de tipo inteiro.")
            parser.success = False
    else:
        print("Erro: Variável não definida.")
        parser.success = False

def p_factor_Array(p):
    '''Factor : ID '[' Exp ']'
    '''
    if p[1] in p.parser.registers:
        if p[1] not in p.parser.ints and len(p.parser.registers.get(p[1])) == 2:
            p[0] = f'PUSHGP\nPUSHI {p.parser.registers.get(p[1])[0]}\nPADD\n{p[3]}LOADN\n'
        else:
            print(f"Erro: Variável {p[1]} não é um array.")
            parser.success = False
    else:
        print("Erro: Variável não definida.")
        parser.success = False

def p_factor_Matrix(p):
    '''Factor : ID '[' Exp ']' '[' Exp ']'
    '''
    if p[1] in p.parser.registers:
        if p[1] not in p.parser.ints and len(p.parser.registers.get(p[1])) == 3:
            c = p.parser.registers.get(p[1])[2]
            p[0] = f'PUSHGP\nPUSHI {p.parser.registers.get(p[1])[0]}\nPADD\n{p[3]}PUSHI {c}\nMUL\n{p[6]}ADD\nLOADN\n'
        else:
            print(f"Erro: Variável {p[1]} não é uma matriz.")
            parser.success = False
    else:
        print("Erro: Variável não definida.")
        parser.success = False

def p_factor_not(p):
    'Factor : MINUS Exp %prec UMINUS'
    p[0] = f'PUSHI 0\n{p[2]}SUB\n'

#aqui esta a ser adicionado o 1antes do PUSHI

def p_factor_num(p):
    '''Factor : NUM
    '''
    p[0] = f'PUSHI {p[1]}\n'

def p_cmd(p):
    '''
    Cmd : Cmd_Writes
        | Cmd_Read
        | Cmd_If 
        | Cmd_If_Else
        | Cmd_While
    '''
    p[0] = p[1]

def p_cmd_read(p):
    '''Cmd_Read : READ ID
    '''
    if p[2] in p.parser.registers:
        p[0] = f'READ\nATOI\nSTOREG {p.parser.registers.get(p[2])}\n'
    else:
        print("Erro: Variável não definida.")
        parser.success = False

def p_cmd_readArray(p):
    '''Cmd_Read : READ ID '[' NUM ']'
    '''
    if p[2] in p.parser.registers:
        if p[2] not in p.parser.ints and len(p.parser.registers.get(p[2])) == 2:
            p[0] = f'PUSHGP\nPUSHI {p.parser.registers.get(p[2])[0]}\nPADD\nPUSHI{p[4]} \nREAD\nATOI\nSTOREN\n'
        else:
            print(f"Erro: Variável {p[2]} não é um array.")
            parser.success = False
    else:
        print("Erro: Variável não definida.")
        parser.success = False

def p_cmd_readMatrix(p):
    '''Cmd_Read : READ ID '[' NUM ']' '[' NUM ']'
    '''
    if p[2] in p.parser.registers:
        if p[2] not in p.parser.ints and len(p.parser.registers.get(p[2])) == 3:
            c = p.parser.registers.get(p[2])[2]
            p[0] = f'PUSHGP\nPUSHI {p.parser.registers.get(p[2])[0]}\nPADD\nPUSHI {p[4]}\nPUSHI {c}\nMUL\nPUSHI {p[7]}\nADD\nREAD\nATOI\nSTOREN\n'
        else:
            print(f"Erro: Variável {p[2]} não é uma matriz.")
            parser.success = False
    else:
        print("Erro: Variável não definida.")
        parser.success = False

def p_cmd_if(p):
    "Cmd_If        : IF LPAREN Condicao RPAREN '{' Cmds '}'"
    p[0] = f'{p[3]}JZ l{p.parser.labels}\n{p[6]}l{p.parser.labels}: NOP\n'
    p.parser.labels += 1

def p_condicao(p):
    '''Condicao : Condicao AND Condicao
                | Condicao OR Condicao
                | NOT Condicao
                | LPAREN Condicao RPAREN
                | Cond
    '''
    if len(p) == 4:
        if p[2] == 'AND':
            p[0] = f'{p[1]}{p[3]}AND\n'
        elif p[2] == 'OR':
            p[0] = f'{p[1]}{p[3]}OR\n'
        else:
            p[0] = p[2]
    elif p[1] == 'NOT':
        p[0] = f'{p[2]}PUSHI 0\nEQUAL\n'
    else:
        p[0] = p[1]

def p_cond(p):
    '''Cond : Exp EQUALS Exp
            | Exp LT Exp
            | Exp LE Exp
            | Exp GT Exp
            | Exp GE Exp
            | Exp
    '''
    if len(p) > 2:
        if p[2] == '==':
            p[0] = f'{p[1]}{p[3]}EQUAL\n'
        elif p[2] == '<':
            p[0] = f'{p[1]}{p[3]}INF\n'
        elif p[2] == '<=':
            p[0] = f'{p[1]}{p[3]}INFEQ\n'
        elif p[2] == '>':
            p[0] = f'{p[1]}{p[3]}SUP\n'
        elif p[2] == '>=':
            p[0] = f'{p[1]}{p[3]}SUPEQ\n'
    else:
        p[0] = p[1]

def p_cmd_if_else(p):
    "Cmd_If_Else        : IF LPAREN Condicao RPAREN '{' Cmds '}' ELSE '{' Cmds '}'"
    p[0] = f'{p[3]}JZ l{p.parser.labels}\n{p[6]}JUMP l{p.parser.labels}f\nl{p.parser.labels}: NOP\n{p[10]}l{p.parser.labels}f: NOP\n'
    p.parser.labels += 1

def p_cmd_while(p):
    "Cmd_While   : WHILE LPAREN Condicao RPAREN '{' Cmds '}'"
    p[0] = f'l{p.parser.labels}c: NOP\n{p[3]}JZ l{p.parser.labels}f\n{p[6]}JUMP l{p.parser.labels}c\nl{p.parser.labels}f: NOP\n'
    p.parser.labels += 1
        
def p_error(p):
    print('Syntax error: ', p)
    parser.success = False

parser = yacc.yacc(debug=True)

parser.success = True
parser.registers = {}
parser.labels = 0
parser.gp = 0
parser.ints = []
parser.assembly = ""

try:
    with open('./3ANO/PLC/TP02/testes/E5.txt','r') as file:
        inp = file.read()
        parser.parse(inp)
        if parser.success:
            with open('./3ANO/PLC/TP02/testes/E5.vm', 'w') as output:
                output.write(parser.assembly)
                print(parser.assembly)
        else:
            print("<><><><><><><><><><><><><><><><><><><><><><><>")
            print("Erro")
            print("<><><><><><><><><><><><><><><><><><><><><><><>")
except KeyboardInterrupt:
    print()
