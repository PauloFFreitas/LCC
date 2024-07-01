#####################################################################
#                                                                   #
#   Geração Automática de Perfis de Pessoas Candidatas a um Emprego #
#                           Maio de 2024                            #
#                                                                   #
#                Paulo Freitas & Francisco Paulino                  #
#                   Universidade do Minho - LCC                     #
#                                                                   #
#####################################################################

from rdflib import Graph, Namespace, URIRef, Literal            # biblioteca rdf sobre grafos
import os 
import re
import copy 

#Pontuacao da avalicao
pontos = []

# Define os namespaces
ex = Namespace("http://uminho.pt/projeto_profiling#")
rdf = Namespace("http://uminho.pt/projeto_profiling/Type#")
rdfs = Namespace("http://uminho.pt/projeto_profiling/Sub_Type#")

# Cria um novo grafo RDF
g = Graph()

candidatos = dict()
proposta = [None]*15
pontuacoes = dict()

# Adiciona classes
g.add((ex.Candidato, rdf.tipo, rdfs.Class))
g.add((ex.Idade, rdf.tipo, rdfs.Class))
g.add((ex.Sexo, rdf.tipo, rdfs.Class))
g.add((ex.Nacionalidade, rdf.tipo, rdfs.Class))
g.add((ex.Endereco, rdf.tipo, rdfs.Class))
g.add((ex.Habilidades, rdf.tipo, rdfs.Class))
g.add((ex.Estudos, rdf.tipo, rdfs.Class))
g.add((ex.Exp_Profissao, rdf.tipo, rdfs.Class))
g.add((ex.Preferencia_Trab, rdf.tipo, rdfs.Class))
g.add((ex.Qual_Interpessoais, rdf.tipo, rdfs.Class))
g.add((ex.Carta_Conducao, rdf.tipo, rdfs.Class))

# Adiciona propriedades
    #Habilidades
g.add((ex.hasHabilidades, rdf.tipo, rdf.Propriedade))
g.add((ex.hasHabilidades, rdfs.dominio, ex.Candidato))
g.add((ex.hasHabilidades, rdfs.contradominio, ex.Habilidades))
    #Exp_Profissao
g.add((ex.hasExp_Profissao, rdf.tipo, rdf.Propriedade))
g.add((ex.hasExp_Profissao, rdfs.dominio, ex.Candidato))
g.add((ex.hasExp_Profissao, rdfs.contradominio, ex.Exp_Profissao))
    #Estudos
g.add((ex.hasEstudos, rdf.tipo, rdf.Propriedade))
g.add((ex.hasEstudos, rdfs.dominio, ex.Candidato))
g.add((ex.hasEstudos, rdfs.contradominio, ex.Estudos))
    #Idade
g.add((ex.hasIdade, rdf.tipo, rdf.Propriedade))
g.add((ex.hasIdade, rdfs.dominio, ex.Candidato))
g.add((ex.hasIdade, rdfs.contradominio, ex.Idade))
    #Sexo
g.add((ex.hasSexo, rdf.tipo, rdf.Propriedade))
g.add((ex.hasSexo, rdfs.dominio, ex.Candidato))
g.add((ex.hasSexo, rdfs.contradominio, ex.Sexo))
    #Nacionalidade
g.add((ex.hasNacionalidade, rdf.tipo, rdf.Propriedade))
g.add((ex.hasNacionalidade, rdfs.dominio, ex.Candidato))
g.add((ex.hasNacionalidade, rdfs.contradominio, ex.Nacionalidade))
    #Endereço
g.add((ex.hasEndereco, rdf.tipo, rdf.Propriedade))
g.add((ex.hasEndereco, rdfs.dominio, ex.Candidato))
g.add((ex.hasEndereco, rdfs.contradominio, ex.Endereco))
    #Pref_Trabalho
g.add((ex.hasPref_Trabalho, rdf.tipo, rdf.Propriedade))
g.add((ex.hasPref_Trabalho, rdfs.dominio, ex.Candidato))
g.add((ex.hasPref_Trabalho, rdfs.contradominio, ex.Preferencia_Trab))
    #Qual_Interpessoais
g.add((ex.hasQual_Interpessoais, rdf.tipo, rdf.Propriedade))
g.add((ex.hasQual_Interpessoais, rdfs.dominio, ex.Candidato))
g.add((ex.hasQual_Interpessoais, rdfs.contradominio, ex.Qual_Interpessoais))
    #Carta_Conducao
g.add((ex.hasCarta_Conducao, rdf.tipo, rdf.Propriedade))
g.add((ex.hasCarta_Conducao, rdfs.dominio, ex.Candidato))
g.add((ex.hasCarta_Conducao, rdfs.contradominio, ex.Carta_Conducao))


# Parser para ler o txt + introduzir candidatos no grafo
def leitor_candidatos():

    # procura o caminho para pesquisar candidaturas
    dir_pasta = os.getcwd()
    path = "candidaturas"
    path = os.path.join(dir_pasta,path)
    print("(leitor_candidaturas)>> A ler candidaturas de:", path, ".\n", "Foram encontrados", len(os.listdir(path)), "arquivos neste diretorio.")

    #copia do grafo g 
    ca = copy.deepcopy(g) 

    for dir in os.listdir(path): # para cada diretorio dentro da pasta
        c=1 # contador a 1
        nome_arq = os.path.join(path,dir) # nome do diretorio
        with open(nome_arq): # abre
            if os.path.isfile(nome_arq): # verifica se é ficheiro
                var=[None]*13 # lista de variaveis
                with open(nome_arq,'r') as file: # abre em leitura
                    conteudo = file.readlines() # le por linha

                    for linha in conteudo: # para cada linha
                        exp=r'.+:\s*(.+)' # re ignora primeira palavra : e espaço
                        corr = re.match(exp,linha) # primeira ocorr da re na linha
                        if corr: # se encontrar:
                            if c>=4 and c<=10: # verifica se é lugar de array
                                array = corr.group(1) # é lugar de array e prepara leitura de varias variaveis
                                exp = r"(?:,\s*|\b)([^,]+)" # re pra ignorar espaços e virgulas
                                vari = re.findall(exp, array) # encontra todas as vezes que re acontece

                                var[c-1]=vari # guarda no array na posiçao certa
                            else: 
                                var[c-1]= corr.group(1) # nao é lista e guarda direto no array
                        else:
                            print("  (leitor de candidaturas)>> Ocorreu um erro durante a leitura do arquivo.")
                        c+=1 # incrementa contador
                
                #introduzir no grafo
                nome= var[0]
                idade= var[1]
                sexo= var[2]
                nacionalidade= var[3]
                exp= var[4]
                estudos = var[5]
                habilidades =var[6]
                pref_Trabalho = var[7]
                qualidades = var[8]
                carta_conducao = var[9]
                endereço = var[12]

                candidatos[nome] = var

                
                # Adiciona Vertices e Arestas
                nome= ex[var[0].replace(" ", "_")]
                ca.add((nome, rdf.tipo, ex.Candidato))

                #V
                idade= ex[var[1]]
                ca.add((Literal(idade), rdf.tipo, ex.Idade))
                #A
                ca.add((nome, ex.hasIdade, Literal(idade)))

                #V
                sexo= ex[var[2]]
                ca.add((sexo, rdf.tipo, ex.Sexo))
                #A
                ca.add((nome, ex.hasSexo, sexo))
    
                for i in nacionalidade:
                    #V
                    ca.add((ex[i], rdf.tipo, ex.Nacionalidade))
                    #A
                    ca.add((nome, ex.hasNacionalidade, ex[i]))
                
                for i in exp:
                     o =re.sub(r'\d+', '', i)
                     #print(o,j)
                     #V
                     ca.add((ex[o.replace(" ", "")], rdf.tipo, ex.Exp_Profissao))
                     #A
                     ca.add((nome, ex.hasExp_Profissao, ex[o.replace(" ", "")]))
                     #Teste de peso
                     num=re.search(r'\d+', i)
                     #num1=num.group()
                     p=Literal(num.group())
                     #print(p)
                     str= "hasExp_ProfissaoPeso"+ (o.replace(" ", ""))
                     ca.add((nome, ex[str], ex[p]))

                for i in estudos:
                    #V
                    ca.add((ex[i.replace(" ", "_")], rdf.tipo, ex.Estudos))
                    #A
                    ca.add((nome, ex.hasEstudos, ex[i.replace(" ", "_")]))

                for i in habilidades:
                     o =re.sub(r'\d+', '', i)
                     #print(o,j)
                     #V
                     ca.add((ex[o.replace(" ", "")], rdf.tipo, ex.Habilidades))
                     #A
                     ca.add((nome, ex.hasHabilidades, ex[o.replace(" ", "")]))
                     #Teste de peso
                     num=re.search(r'\d+', i)
                     #num1=num.group()
                     p=Literal(num.group())
                     #print(p)
                     str= "hasHabilidadesPeso"+ (o.replace(" ", ""))
                     ca.add((nome, ex[str], ex[p]))
                     
                for i in pref_Trabalho:
                    #V
                    if i.upper() == "TODOS":
                        pref_Trabalho.append("Remoto")
                        pref_Trabalho.append("Presencial")
                        pref_Trabalho.append("Hibrido")
                    else:
                        ca.add((ex[i], rdf.tipo, ex.Preferencia_Trab))
                        #A
                        ca.add((nome, ex.hasPreferencia_Trab, ex[i]))

                for i in qualidades:
                    #V
                    ca.add((ex[i.replace(" ", "_")], rdf.tipo, ex.Qual_Interpessoais))
                    #A
                    ca.add((nome, ex.hasQual_Interpessoais, ex[i.replace(" ", "_")]))
                
                for i in carta_conducao:
                    #V
                    ca.add((ex[i.replace(" ", "")], rdf.tipo, ex.Carta_Conducao))
                    #A
                    ca.add((nome, ex.hasCarta_Conducao, ex[i.replace(" ", "")]))
                
                #V
                ca.add((ex[(endereço.split(', ', 1))[-1].replace(" ", "_")], rdf.tipo, ex.Endereco))
                #A
                ca.add((nome, ex.hasEndereco, ex[(endereço.split(', ', 1))[-1].replace(" ", "_")]))
            else:
                print(" (leitor de candidaturas)>> Foi encontrado uma pasta ou arquivo inesperado. Procedendo leitura para outros ficheiros.")

    # Salva a ontologia (em grafo) em um arquivo xml
    ca.serialize("candidate_profile.json", format="json-ld")


#PROSPOSTA DE EMPREGO 
def leitor_proposta():

    # procura o caminho para pesquisar candidaturas
    dir_pasta = os.getcwd()
           
    with open("referencia.txt",'r') as file: # abre em leitura
        print("(leitor_proposta)>> Foi detetado ficheiro de referencia.")
        conteudo = file.readlines() # le por linha
        c=1
        for linha in conteudo: # para cada linha
            exp = r'.+:\s*(.+)' # re ignora primeira palavra : e espaço
            corr = re.match(exp,linha) # primeira ocorr da re na linha
            if c==1:
                if corr:
                    exp = r'(\d\d)-(\d\d)'

                    # Aplicando a expressão regular
                    match = re.search(exp, linha)

                    if match:
                        n1 = int(match.group(1))
                        n2 = int(match.group(2))
                        #print(n1,n2)
                        x=list(range(n1,n2+1))
                        #print(x)
                        proposta[c-1]=x
                        #print(var)
                else:
                    proposta[c-1]=None
                c+=1
            elif (c>=2 and c<=15):
                if corr:
                    array = corr.group(1) # é lugar de array e prepara leitura de varias variaveis
                    exp = r"(?:,\s*|\b)([^,]+)"

                    # Aplicando a expressão regular
                    match = re.search(exp, linha)
                    vari = re.findall(exp, array) # encontra todas as vezes que re acontece

                    proposta[c-1]=vari # guarda no array na posiçao certa
                
                else:
                    proposta[c-1]=None
                c+=1
    #introduzir no grafo

    nome = ex["Proposta"]
    idade = proposta[0]
    sexo = proposta[1]
    nacionalidade = proposta[2]
    Exper_Profissional = proposta[3]
    Estudos = proposta[4]
    Estudos_rel = proposta[5]
    Habilidades = proposta[6]
    Habilidades_rel = proposta[7]
    Pref_trabalho = proposta[8]
    Qualidades = proposta[9]
    Qualidades_rel = proposta[10]
    Carta_conducao = proposta[11]
    Carta_conducao_rel = proposta[12]
    Endereco = proposta[13]
    Endereco_rel = proposta[14]
    
    #copia do grafo g 
    pr = copy.deepcopy(g) 

    # Adiciona Vertices e Arestas
    pr.add((nome, rdf.tipo, ex.Candidato))

    for i in idade:
        #v
        pr.add((Literal(i), rdf.tipo, ex.Idade))
        #A
        pr.add((nome, ex.hasIdade, ex[Literal(i)]))

    for i in sexo:
        #V
        pr.add((ex[i], rdf.tipo, ex.Sexo))
        #A
        pr.add((nome, ex.hasSexo, ex[i]))


    for i in nacionalidade:
        #V
        pr.add((ex[i], rdf.tipo, ex.Nacionalidade))
        #A
        pr.add((nome, ex.hasNacionalidade, ex[i]))
    
    for i in Exper_Profissional:
            o =re.sub(r'\d+', '', i)
            #print(o,j)
            #V
            pr.add((ex[o.replace(" ", "")], rdf.tipo, ex.Exp_Profissao))
            #A
            pr.add((nome, ex.hasExp_Profissao, ex[o.replace(" ", "")]))
            #Teste de peso
            num=re.search(r'\d+', i)
            #num1=num.group()
            p=Literal(num.group())
            #print(p)
            str= "hasExp_ProfissaoPeso"+ (o.replace(" ", ""))
            pr.add((nome, ex[str], p))

    for i in Estudos:
        #V
        pr.add((ex[i.replace(" ", "_")], rdf.tipo, ex.Estudos_pref))
        #A
        pr.add((nome, ex.hasEstudos_pref, ex[i.replace(" ", "_")]))

    for i in Estudos_rel:
        #V
        pr.add((ex[i.replace(" ", "_")], rdf.tipo, ex.Estudos_rel))
        #A
        pr.add((nome, ex.hasEstudos_rel, ex[i.replace(" ", "_")]))

    for i in Habilidades:
            o =re.sub(r'\d+', '', i)
            #print(o,j)
            #V
            pr.add((ex[o.replace(" ", "")], rdf.tipo, ex.Habilidades_pref))
            #A
            pr.add((nome, ex.hasHabilidades_pref, ex[o.replace(" ", "")]))
            #Teste de peso
            num=re.search(r'\d+', i)
            #num1=num.group()
            p=Literal(num.group())
            #print(p)
            str= "hasHabilidadesPeso"+ (o.replace(" ", ""))
            pr.add((nome, ex[str], ex[p]))
    
    for i in Habilidades_rel:
            o =re.sub(r'\d+', '', i)
            #print(o,j)
            #V
            pr.add((ex[o.replace(" ", "")], rdf.tipo, ex.Habilidades_rel))
            #A
            pr.add((nome, ex.hasHabilidades_rel, ex[o.replace(" ", "")]))
            #Teste de peso
            num=re.search(r'\d+', i)
            #num1=num.group()
            p=Literal(num.group())
            #print(p)
            str= "hasHabilidadesPeso"+ (o.replace(" ", ""))
            pr.add((nome, ex[str], ex[p]))
    
    for i in Pref_trabalho:
        if i.upper() == "TODOS":
            Pref_trabalho.append("Remoto")
            Pref_trabalho.append("Presencial")
            Pref_trabalho.append("Hibrido")
        else:
            pr.add((ex[i], rdf.tipo, ex.Preferencia_Trab))
            #A
            pr.add((nome, ex.hasPreferencia_Trab, ex[i]))

    for i in Qualidades:
        #V
        pr.add((ex[i.replace(" ", "_")], rdf.tipo, ex.Qual_Interpessoais_pref))
        #A
        pr.add((nome, ex.hasQual_Interpessoais_pref, ex[i.replace(" ", "_")]))

    for i in Qualidades_rel:
        #V
        pr.add((ex[i.replace(" ", "_")], rdf.tipo, ex.Qual_Interpessoais_rel))
        #A
        pr.add((nome, ex.hasQual_Interpessoais_rel, ex[i.replace(" ", "_")]))
    
    for i in  Carta_conducao:
        #V
        pr.add((ex[i.replace(" ", "")], rdf.tipo, ex.Carta_Conducao_pref))
        #A
        pr.add((nome, ex.hasCarta_Conducao_pref, ex[i.replace(" ", "")]))

    for i in  Carta_conducao_rel:
        #V
        pr.add((ex[i.replace(" ", "")], rdf.tipo, ex.Carta_Conducao_rel))
        #A
        pr.add((nome, ex.hasCarta_Conducao_rel, ex[i.replace(" ", "")]))
    
    for i in Endereco:
        #V
        pr.add((ex[(i.split(', ', 1))[-1]], rdf.tipo, ex.Endereco_pref))
        #A
        pr.add((nome, ex.hasEndereco_pref, ex[(i.split(', ', 1))[-1]]))

    for i in Endereco_rel:
        #V
        pr.add((ex[(i.split(', ', 1))[-1]], rdf.tipo, ex.Endereco_rel))
        #A
        pr.add((nome, ex.hasEndereco_rel, ex[(i.split(', ', 1))[-1]]))
    
    print("(leitor_proposta)>> Os dados da vaga de emprego foram guardados.")

    # Guarda a ontologia (em grafo) em um arquivo json
    pr.serialize("proposta.json", format="json-ld")


def extract_candidate_subgraph(g, subject):
  subgraph = Graph()
  for s, p, o in g.triples((subject, None, None)):
    subgraph.add((s, p, o))
  return subgraph

def comparador():
    # Carrega o grafo A
    g_a = Graph()
    g_a.parse("candidate_profile.json", format="json-ld")

    # Find and process Candidato nodes
    candidates = []
    #print("aqui")
    for s in g_a.subjects(rdf.tipo, ex.Candidato):
        name = s
        # Extract the subgraph
        subgraph = extract_candidate_subgraph(g_a, s)
        #verificar se a esta a sair o subgrafo
        #subgraph.serialize("Teste.json", format="json-ld")
        candidates.append((name, subgraph))

    #Estrair grafo de proposta 
    g_b = Graph()
    g_b.parse("proposta.json", format="json-ld")

    # Find and process Candidato nodes
    
    nome_proposta = URIRef("http://uminho.pt/projeto_profiling#Proposta")
        
    # Extract the subgraph
    subgraphP = extract_candidate_subgraph(g_b, nome_proposta )

    resultados = list()
    # Compara os grafos
    for (i,j) in candidates:
        n,correspondencias = compare_graphs(subgraphP, j, nome_proposta, i)
        i=str(i)
        i=i.split('#')[-1]
        i= i.replace("_", " ")

        pontuacoes[i] = correspondencias
        resultados.append((i,n))
        resultados=sorted(resultados, key=lambda x: -x[1])
    
    c=1
    for i in resultados:
        print(c,"-->",i[0],"--",i[1],"pontos")
        c+=1

def compare_graphs(graphp, graphc, nome_proposta , i):

    pontos = [[12],[2],[2],[2],[10,5],[4,2],[3],[4,2],[5,3],[7,5]]
    catcand= ["hasIdade","hasSexo", "hasNacionalidade","hasExp_Profissao", "hasEstudos", "hasHabilidades", "hasPreferencia_Trab", "hasQual_Interpessoais", "hasCarta_Conducao",
              "hasEndereco"]
    catprop= [["hasIdade"],["hasSexo"], ["hasNacionalidade"], ["hasExp_Profissao"], ["hasEstudos_pref", "hasEstudos_rel"], ["hasHabilidades_pref", "hasHabilidades_rel" ],
              ["hasPreferencia_Trab"], ["hasQual_Interpessoais_pref", "hasQual_Interpessoais_rel"], ["hasCarta_Conducao_pref", "hasCarta_Conducao_rel"], ["hasEndereco_pref","hasEndereco_rel"]]
    
    sumatorio = 0
    correspondecias=list()
    
    for j in range(10):
        flag=0
        for c in graphc.objects(i,ex[catcand[j]]):
            for gp in range(len(catprop[j])):
                for p in graphp.objects(nome_proposta,ex[catprop[j][gp]]):
                    if str(c).upper()==str(p).upper():
                        if(j==2 or j==6): # nacionalidades e preferencias de trabalho
                            if(flag==0):
                                sumatorio+=pontos[j][gp]
                                flag=1
                                c=str(c).split('#')[-1]
                                correspondecias.append((catcand[j],c,pontos[j][gp]))

                        elif(j==7): # qualidades
                            if(flag!=2):
                                sumatorio+=pontos[j][gp]
                                flag+=1
                                c=str(c).split('#')[-1]
                                correspondecias.append((catcand[j],c,pontos[j][gp]))
                        
                        elif(j==3 or j==5): # experiencia e habilidades
                            url = str(c)
                            url = url.split('#')[-1]
                            urlc = catcand[j] +"Peso"+ url
                            a = b = 0
                            for a in graphc.objects(i,ex[urlc]):
                                a=int(str(a).split('#')[-1])
                                
                            for b in graphp.objects(nome_proposta,ex[urlc]):
                                b=int(str(b).split('#')[-1])
                                
                            if (a>=b):
                                sumatorio += pontos[j][gp]*a
                                correspondecias.append((catcand[j],url, pontos[j][gp]*a))
                            else:
                                sumatorio += int(3*(pontos[j][gp]*a)/4)
                                correspondecias.append((catcand[j],url,int(3*(pontos[j][gp]*a)/4)))

                        else:
                            sumatorio+=pontos[j][gp]
                            c=str(c).split('#')[-1]
                            correspondecias.append((catcand[j],c.replace("_"," "),pontos[j][gp]))

    return sumatorio,correspondecias

def show_candidatos():
    for i in candidatos.keys():
        var = candidatos[i]
        print("(Candidaturas)>> Inserido no grafo o candidato",i,"com as informações:")
        print(" Nome:", var[0],
                "\n Idade:",var[1],
                "\n Sexo:", var[2],
                "\n Nacionalidades:", ", ".join(var[3]),
                "\n Experiencia:",", ".join(var[4]),
                "\n Educacao:",", ".join(var[5]),
                "\n Habilidades:", ", ".join(var[6]),
                "\n Preferencia de regime de trabalho:", ", ".join(var[7]),
                "\n Qualidades Interpessoais:", ", ".join(var[8]),
                "\n Carta de Conducao:", ", ".join(var[9]),
                "\n Endereço:", var[12], "\n")

def show_proposta():
    idades=list()
    for i in proposta[0]:
        idades.append(str(i))
    print("(Proposta)>> Inserido no grafo a vaga com informações:")
    print(" Idade:",", ".join(idades),
          "\n Sexo:", ", ".join(proposta[1]),
          "\n Nacionalidades:", ", ".join(proposta[2]),
          "\n Experiencia:",", ".join(proposta[3]),
          "\n Educacao Preferencial:",", ".join(proposta[4]),
          "\n Educacao Relativa:",", ".join(proposta[5]),
          "\n Habilidades Preferenciais:", ", ".join(proposta[6]),
          "\n Habilidades Relativas:", ", ".join(proposta[7]),
          "\n Preferencia de regime de trabalho:", ", ".join(proposta[8]),
          "\n Qualidades Interpessoais Preferenciais:", ", ".join(proposta[9]),
          "\n Qualidades Interpessoais Relativas:", ", ".join(proposta[10]),
          "\n Carta de Conducao Preferencial:", ", ".join(proposta[11]),
          "\n Carta de Conducao Relativa:", ", ".join(proposta[12]),
          "\n Endereços Preferencial:", ", ".join(proposta[13]),
          "\n Endereços Relativo:", ", ".join(proposta[14]), "\n")

def show_pontuacoes():
    for i in pontuacoes.keys():
        pontos=0
        print("(Pontuação)>> O candidato", i, "teve pontuação das seguintes categorias:")
        for (cat,obj,ponto) in pontuacoes[i]:
            print(" ",cat,"validou", obj,"e obteve",ponto,"pontos,")
            pontos += ponto
        print(" No total obteve",pontos,"pontos.\n")

def main():
    flag_p = 0
    flag_c = 0
    flag_e = 0
    print("\nGeração Automática de Perfis de Pessoas Candidatas a um Emprego")
    print("Escolha a opção:\n")
    print(" 1 - Avaliação completo\n")
    print(" 2 - Leitor de Candidaturas\n")
    print(" 3 - Leitor de Proposta\n")
    print(" 4 - Comparador\n")
    print(" 5 - Pontuações dos Candidatos\n")
    print(" 6 - Informações dos Candidatos\n")
    print(" 7 - Informações da vaga de emprego\n")
    print(" 0 - Sair")
    
    while(True):
        opcao = input("\nDigite a opção: ")
        print("")

        if opcao == '0':
            print("\n Até uma próxima.\n")
            break
        elif opcao == '7':
            if flag_e ==0:
                print("Não existem informações sobre a vaga no grafo. Por favor, execute a leitura da proposta primeiramente.")
            else:
                print("A mostar as informações da vaga de emprego.")
                show_proposta()

        elif opcao == '6':
            if flag_c==0:
                print("Não existem candidatos no grafo. Por favor, execute a leitura de candidatos primeiramente.")
            else:
                print("A mostrar as informações de cada candidato.")
                show_candidatos()

        elif opcao == '5':
            if flag_p==0:
                print("Não existe registro de pontuações. Por favor, execute o programa antes.")
            else:
                print("A mostrar os descritivos das pontuações de cada candidato.")
                show_pontuacoes()
        elif opcao == '4':
            flag_p=1
            comparador()
        elif opcao == '3':
            flag_e=1
            leitor_proposta()
        elif opcao == '2':
            flag_c = 1
            leitor_candidatos()
        elif opcao =='1':
            flag_c = 1
            flag_p = 1
            flag_e = 1

            print("Iniciando leitor de candidaturas...")
            leitor_candidatos()
            print("\nIniciando leitor de proposta...")
            leitor_proposta()
            print("\nIniciando avaliador de candidaturas...")
            comparador()

        elif opcao =='help' or opcao=='9':
            main()
            break
        else:
            print("Opcao Invalida.\n")

main()