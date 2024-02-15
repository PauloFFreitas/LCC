'''
TP01
PAULO FREITAS A100053
PEDRO SANTOS A100110
MIGUEL REGO A94017
'''

'''
Enunciado:
Neste exercício pretende-se trabalhar com um dataset gerado no âmbito do registo de cartas do séc. XVI/XVII
relativos à chegada das naus portuguesas à Etiópia e India.
Construa, então, um programa Python para processar o dataset "cartasetopia.csv" e produzir o solicitado nas
alíneas seguintes:
a) Calcular a frequência de Cartas por ano e mês;
b) Calcular a distribuição de Cartas por Local;
c) Calcular a frequência com que cada Interveniente aparece como destinatário, remetente ou mencionado em todas
as cartas;
d) Criar um ficheiro de saída em formato JSON agrupando Título e Resumo de cada carta. Inclua depois uma
mecanismo que permita procurar sobre esse objeto JSON as cartas onde ocorra um dado termo;
e) Construir um Grafo de Conhecimentos que mostra os nomes (apelidos) dos Intervenientes que estão conectados
uns aos outros como destinatário, remetente ou mencionado em cada carta. Para visualizar o grafo, descarregue
os triplos para um ficheiro DOT que possa depois ser aberto por um visualizador como o que pode ser encontrado
em http://www.webgraphviz.com/.

Crie uma página HTML (ficheiro ’index.html’) para apresentar os resultados do seu processador.
'''

# Para a resolusão deste trabalho prático serão necessárias definições presentes nas bibliotecas "re" e "json".
# Com estas bibliotecas, teremos acesso a operações com expressões regulares, e operações sobre arquivos .json.
import re
import json

# a)    Calcular a frequência de Cartas por ano e mês
# Para calcular a frequência das cartas foi necessário organizar num dicionário as datas.
# É usado um tratamento com uma expressão regular para obter o ano e mês apartir da data.
# Caso um ano ou mês esteja já no dicionário, incrementa na frequência, caso contrario adiciona essa data no dicionário e
# inicializa a 1.
def calendar(data, freq_mes, freq_ano):
    data = re.search(r'([0-9]{4})\.([0-9]{2})\.[0-9]{2}', data)
    ano = data.group(1)
    mes = data.group(2)
    if ano not in freq_ano:
        freq_ano[ano] = 1
    else:
        freq_ano[ano] += 1
    if mes not in freq_mes:
        freq_mes[mes] = 1
    else:
        freq_mes[mes] += 1


# b)    Calcular a distribuição de Cartas por Local
# Para calcular a distribuição de cartas por local, foi necessário organizar a informação num diionário.
# é usado um tratamento com expressões regulares para obter a localidade de uma carta. Essa ER permite reconhecer tambem
# caracteres com assentos, presentes em algumas situações.
# Caso uma certa localidade já esteja presente no dicionário, incrementa, caso contrário adiciona a localidade no dicionario
# e inicializa a 1.
def distri(data, dist):
    data = re.match(r"[a-zA-Z\u00C0-\u00FF]+", data)
    if data:
        local = data.group(0)
        if local not in dist:
            dist[local] = 1
        else:
            dist[local] += 1


# c)    Calcular a frequência com que cada Interveniente aparece como destinatário, remetente ou mencionado em todas as cartas.
# Para calcular a frequência de nomes nas cartas foi necessário organizar num dicionário.
# Tendo a lista com os nomes, e sabendo o formato dos nomes na lista é "remetente [0], destinatário [1], mencionados[2:]", verifica as posições
# dos nomes na lista e trata a sua frequência.
# O dicionário "freqdest" inclui nomes dos destinatarios, o dicionário "freqremet" inclui nomes dos remetentes, e o dicionário "freqmenc" inclui
# os nomes das pessoas mencionadas.
# Caso o nome(seja remetente, destinatário ou mencionado) já estiver no dicionário correspondente, incrementa a sua frequência, caso contrario
# adiciona o nome e inicializa a 1.
def namefreq(data, freqdest, freqremet, freqmenc):
    # Adicionar os nomes do remetente ao dicionário correspondente
    if len(data) > 0:
        name = re.match(r"[a-zA-Z\u00C0-\u00FF]+", data[0])
        if name:
            name = name.group(0)
            if name in freqremet:
                freqremet[name] += 1
            else:
                freqremet[name] = 1

    # Adicionar os nomes dos destinatários ao dicionário correspondente
    if len(data) > 1:
        name = re.match(r"[a-zA-Z\u00C0-\u00FF]+", data[1])
        if name:
            name = name.group(0)
            if name in freqdest:
                freqdest[name] += 1
            else:
                freqdest[name] = 1

    # Adicionar os nomes mencionados ao dicionário correspondente
    if len(data) > 2:
        for name in data[2:]:
            name = re.match(r"[a-zA-Z\u00C0-\u00FF]+", name)
            if name:
                name = name.group(0)
                if name in freqmenc:
                    freqmenc[name] += 1
                else:
                    freqmenc[name] = 1

# d.1) Cria um arquivo JSON com as informações fornecidas.
# Recorre à biblioteca json para abrir o arquivo de forma a facilitar a tradução entre conteudo e python.
def create_json(file, info):
    jsonfile = open(file, "w", encoding="utf-8")
    json.dump(info, jsonfile, ensure_ascii=False, indent=4)
    jsonfile.flush()

# d.2) Procura um termo específico no objeto JSON e retorna o titulo da carta onde encontra o termo.
# O programa lê o arquivo json de forma a conseguir processar os dicionários. Assim serpara os titulos e resumos das devidas cartas.
# Procura o termo no titulo e no resumo, se encontra em um dos dois, adiciona o titulo da carta à lista de resultados.
# Caso não encontre pelo menos uma carta com o termo, indica que não encontrou.
def search_term_json(file, term):
    resultados = []
    with open(file, 'r', encoding="utf-8") as arquivo:
        cartas=json.load(arquivo)
        for carta in cartas:
            titulo=carta["Título"]
            resumo=carta["Resumo"]
            if re.search(term,titulo) or re.search(term,resumo):
                resultados.append(titulo)
    return resultados 


# e.1)    Construir um Grafo de Conhecimentos que mostra os nomes (apelidos) dos Intervenientes que estão conectados
# uns aos outros como destinatário, remetente ou mencionado em cada carta.
# Para a criação do grafo, será necessario armazenar os nomes (vertices) e as relaçoes de envio e menções (arestas).
# Após armazenar os nomes da carta, armazena o primeiro e segundo nome em um par na lista "envios".
# Separa esses dois nomes e estabelece para os restantes nomes (caso existam) um par entre o primeiro nome e o mencionado em um par,
# que é inserido na lista "mencoes".
def grafo(carta, nomes, envios, mencoes):
    carta.remove('')
    for nome in carta:
        if nome not in nomes:
            nomes.append(nome)
    if len(carta) >= 2:
        envios.append((carta[0], carta[1]))
        mencionados = carta[2:]
        for pessoa in mencionados:
            mencoes.append((carta[0], pessoa))


# e.2)   Após verificar todas as linhas, e obter as informações em listas de pares e os respetivos nomes,
# Escreve para um ficheiro dot instruções para gerar o grafo.
# Para tal, abre o ficheiro em modo escrita.
# São escitos os nomes, e após isso, ecreve os nomes presentes nos pares nas listas.
# Caso o par pertença à lista "envios", a cor da aresta será Vermelha.
# Caso o par pertença à lista "mencoes", a cor da aresta será Azul.
def dot_file(nomes, envios, mencoes):
    with open('grafo_cartas.dot', 'w', encoding='utf-8') as dotfile:
        DOT_file = "digraph G {\n"
        sub = ""
        
        for x in envios:
            sub += (f'    "{x[0]}" -> "{x[1]}" [color="0.002 0.999 0.999"];\n')
        for y in mencoes:
            sub += (f'    "{y[0]}" -> "{y[1]}" [color="0.649 0.701 0.701"];\n')
        for n in nomes:
            if not re.search(n, sub):
                sub = (f'    "{n}";\n') + sub
        
        DOT_file += sub

        # Subgrafo para legenda
        DOT_file += ('''    subgraph cluster_01 {
        label = \"Legenda\";
        node [shape=point]
            {
                rank=same
                d0 [style = invis];
                d1 [style = invis];
                p0 [style = invis];
                p1 [style = invis];
            }
        d0 -> d1 [label=Enviou color=\"0.002 0.999 0.999\"]
        p0 -> p1 [label=Mencionou color=\"0.649 0.701 0.701\"]
    }
}''')
        dotfile.write(DOT_file)
        return DOT_file

# Funçao que gera o html com as informações
# Esta função gera o arquivo html com os resultaodos obtidos em cada alinea.
def gen_html(DOT_file, termo, c_termo, freq_mes, freq_ano, dist, freqdest, freqremet, freqmenc):
    #inicio do html
    html_str = "<!DOCTYPE html><html><head><title>TP01 de PLC, feito por Pedro Santos, Paulo Freitas, Miguel Rego</title></head><body>"
    html_str += '<h1 style="text-align:center">TP01, PLC</h1><p style="text-align:center">Pedro Santos, A100110</p><p style="text-align:center">Paulo Freitas, A100053</p><p style="text-align:center">Miguel Rego, A94017</p>'

    #a
    html_str += '<h2 style="text-align:center">Frequências, alínea a) </h2>'
    html_str += '<table width="100%" style="text-align:center"><tr valign="top"><td>'
    html_str += '<h3 style="text-align:center">Meses</h3>'
    for d in sorted(freq_mes.keys()):
        html_str += f'<p style="text-align:center">Mês {d}: {freq_mes[d]}</p>'
    html_str += '</td><td>'
    html_str += '<h3 style="text-align:center">Anos</h3>'
    for d in sorted(freq_ano.keys()):
        html_str += f'<p style="text-align:center">Ano {d}: {freq_ano[d]}</p>'
    html_str += '</td></tr></table>'
    
    #b
    html_str += '<h2 style="text-align:center">Distribuição, alínea b) </h2>'
    for d in sorted(dist.keys()):
        html_str += f'<p style="text-align:center">Carta para {d}: {dist[d]}</p>'

    #c
    html_str += '<h2 style="text-align:center">Frequência de intervenientes, alínea c) </h2>'
    html_str += '<table width="100%" style="text-align:center"><tr valign="top"><td>'
    html_str += '<h3 style="text-align:center">Destinatários</h3>'
    for d in sorted(freqdest.keys()):
        html_str += f'<p style="text-align:center">De {d}: {freqdest[d]}</p>'
    html_str += '</td><td>'
    html_str += '<h3 style="text-align:center">Remetentes</h3>'
    for d in sorted(freqremet.keys()):
        html_str += f'<p style="text-align:center">De {d}: {freqremet[d]}</p>'
    html_str += '</td><td>'
    html_str += '<h3 style="text-align:center">Menções</h3>'
    for d in sorted(freqmenc.keys()):
        html_str += f'<p style="text-align:center">De {d}: {freqmenc[d]}</p>'
    html_str += '</td></tr></table>'

    #d
    html_str += '<h2 style="text-align:center">Termo de pesquisa no JSON, alínea d) </h2>'
    if len(c_termo) > 0:
        html_str += f'<h3 style="text-align:center">Termo "{termo}" foi encontrado nas cartas:</h3>'
        for c in c_termo:
            html_str += f'<p style="text-align:center">{c}</p>'
    else:
        html_str += f'<h3 style="text-align:center">Termo "{termo}" não foi encontrado nas cartas</h3>'

    #e
    html_str += '<h2 style="text-align:center">Grafo, alínea e) </h2>'
    html_str += '<table width="100%" style="text-align:center"><tr valign="top"><td>'
    html_str += '<h3 style="text-align:center">Nomes sem conexões</h3>'
    nd = True
    rd = True
    for d in list(re.split(r'\n',DOT_file)):
        if re.search(r'subgraph cluster_01 {', d):
            break
        elif re.search(r'digraph G {', d):
            continue
        elif re.search(r'color="0.002 0.999 0.999"', d):
            if nd:
                html_str += '</td><td>'
                html_str += '<h3 style="text-align:center">Envios</h3>'
                nd = False
            env = re.split(r'[^a-zA-Z\u00C0-\u00FF]+', d)
            html_str += f'<p style="text-align:center">{env[1]} enviou a {env[2]}</p>'
        elif re.search(r'color="0.649 0.701 0.701"', d):
            if rd:
                html_str += '</td><td>'
                html_str += '<h3 style="text-align:center">Menções</h3>'
                rd = False
            env = re.split(r'[^a-zA-Z\u00C0-\u00FF]+', d)
            html_str += f'<p style="text-align:center">{env[1]} mencionou {env[2]}</p>'
        elif re.search(r'[a-zA-Z\u00C0-\u00FF]+', d):
            nome = re.search(r'[a-zA-Z\u00C0-\u00FF]+', d).group(0)
            html_str += f'<p style="text-align:center">{nome}</p>'
    html_str += '</td></tr></table>'

    #finalizaçao e escrita do html
    html_str += "</body></html>"
    with open('index.html', 'w', encoding='utf-8') as html:
        html.write(html_str)

# Função central do programa.
# Aqui existe uma conversão do conteudo do csv para conteudo legivel em python.
def main():
    # Esta porção de código é responsável por abrir o arquivo "cartasetiopia.csv" em modo leitura,
    # o que permiterá obter as informações presentes no arquivos para serem tratadas.
    # Para tal, a leitura é feita por linha e a ignora a primeira linha (linha tipo).
    with open("cartasetiopia.csv", 'r', encoding="utf-8") as file:
        file = file.readlines()
        file.pop(0)

    # Após a abertura do arquivo csv, cada linha do mesmo é tratada de modo a colocar as informações
    # em um dicionário. Para isso, são usadas operações com expressões regulares. Erros ortográficos
    # e de espaçamento são corrigidos.
    # Depois do tratamento da informação de cada linha para o dicionário, são invocadas definições das diferentes
    # alineas para um tratamento mais especifico de acordo com o enunciado pedido.
    cartas, nomes, envios, mencoes = [], [], [], []
    freq_mes, freq_ano, dist, freqdest, freqremet, freqmenc = {}, {}, {}, {}, {}, {}
    for line in file:
        if re.search(r"[^\s\n]+", line):
            values = re.split(r";", line)
            current = {}
            for v in range(len(values)):
                current[v] = re.sub(r"^\s+", '', values[v])
            current[4] = re.split(r":", current[4])
            for v in range(len(current[4])):
                current[4][v] = re.sub(r"^\s+|\s+$", '', current[4][v])
            current[5] = re.sub(r'\s+', ' ', current[5])

            # Invocações dos processadores das alineas.
            calendar(current[1], freq_mes, freq_ano)
            distri(current[2], dist)
            namefreq(current[4], freqdest, freqremet, freqmenc)
            grafo(current[4], nomes, envios, mencoes)
            cartas.append({
                "Título": current[3].strip(),
                "Resumo": current[5].strip(),
            })

    # Invocação dos processos sobre ficheiros.
    create_json("cartas_etiopia.json", cartas)
    termo = input("d)\n Escreva o termo a procurar:\n")
    c_termo = search_term_json("cartas_etiopia.json", termo)

    print("e)\nA criar ficheiro .dot")
    DOT_file = dot_file(nomes, envios, mencoes)
    print("grafo_cartas.dot criado.")

    print("A gerar o html...")
    gen_html(DOT_file, termo, c_termo, freq_mes, freq_ano, dist, freqdest, freqremet, freqmenc)
    print("ficheiro html gerado")

# Por aqui que começa o programa. É chamada a função main, responsável por tratar o csv e invocar
# os diferentes processos das respetivas alineas, como tambem o processo de criação do arquivo html.
main()