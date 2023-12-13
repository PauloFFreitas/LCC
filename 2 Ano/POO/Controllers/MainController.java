package Projeto.Controllers;
import Projeto.Exceptions.*;
import Projeto.Models.*;
import jdk.jshell.execution.Util;

import java.io.*;
import java.util.*;
import java.time.Year;
import java.time.LocalDate;


public class MainController implements Serializable {
    private List<Utilizador> utilizadores;
    private List<Encomenda> encomendas;
    private List<Artigo> artigos;

    private List<Transportadora> transportadoras;

    private Utilizador utilizadorAtual;
    private Encomenda encomendaAtual;


    public MainController() {
        this.utilizadores = new ArrayList<>();
        this.encomendas = new ArrayList<>();
        this.artigos = new ArrayList<>();
        this.transportadoras = new ArrayList<>();
        this.encomendaAtual = null;
    }



    public void adicionarUtilizador(String email, String nome, String morada, String numeroFiscal) throws UtilizadorExistenteException {
        boolean utilizadorExistente = utilizadores.stream()
                .anyMatch(utilizador -> utilizador.getEmail().equals(email));
        if(utilizadorExistente){
            throw new UtilizadorExistenteException("Utilizador com o email '" + email + "' já foi criado.");
        }
        Utilizador novoUtilizador = new Utilizador(email, nome, morada, numeroFiscal);
        utilizadores.add(novoUtilizador);
    }

    public void realizarLogin(String email) throws UtilizadorNaoExistenteException {
        for (Utilizador utilizador : utilizadores) {
            if (utilizador.getEmail().equals(email)) {
                utilizadorAtual = utilizador;
                this.encomendaAtual = new Encomenda(utilizadorAtual);
                return; //
            }
        }
        throw new UtilizadorNaoExistenteException("Utilizador com o email '" + email + "' não existe.");
    }

    public void adicionarSapatilhaAoUtilizador(String descricao, String marca, double precoBase, boolean isNovo,
                                               double avaliacaoEstado , int numDonosAnteriores, int tamanho, boolean temAtacadores,
                                               String cor, Year dataLancamentoColecao, boolean isPremium, String transportadora) {


        Transportadora transportadoraAtual = null;
        for (Transportadora transportadoraItem : transportadoras) {
            if (transportadoraItem.getCodigo().equals(transportadora)) {
                transportadoraAtual = transportadoraItem;
                break;
            }
        }

        if (transportadoraAtual == null) {
            return;
        }

        Sapatilha novaSapatilha = new Sapatilha(descricao, marca, precoBase, isNovo,false, avaliacaoEstado, numDonosAnteriores, 0,tamanho, temAtacadores, cor, dataLancamentoColecao, isPremium, utilizadorAtual.getCodigo(), transportadoraAtual);
        artigos.add(novaSapatilha);
    }


    public void adicionarTShirtAoUtilizador(String descricao, String marca, double precoBase, boolean isNovo, int avaliacaoEstado, int numDonosAnteriores, String tamanho, String padrao, String transportadora) {

        Transportadora transportadoraAtual = null;
        for (Transportadora transportadoraItem : transportadoras) {
            if (transportadoraItem.getCodigo().equals(transportadora)) {
                transportadoraAtual = transportadoraItem;
                break;
            }
        }

        if (transportadoraAtual == null) {
            // handle the case where the transportadora is not found
            return;
        }

        try {
            TShirt.Tamanho tamanhoEnum = TShirt.Tamanho.valueOf(tamanho.trim().toUpperCase());
            TShirt.Padrao padraoEnum = TShirt.Padrao.valueOf(padrao.trim().toUpperCase());
            TShirt novaTShirt = new TShirt(descricao, marca, precoBase, isNovo, avaliacaoEstado, numDonosAnteriores, 0, false, tamanhoEnum, padraoEnum, utilizadorAtual.getCodigo(), transportadoraAtual);
            artigos.add(novaTShirt);
        } catch (IllegalArgumentException e) {
            e.printStackTrace();
        }
    }



    public void adicionarMalaAoUtilizador(String descricao, String marca, double precoBase, boolean isNovo, double avaliacaoEstado, int numDonosAnteriores, int dimensao, String material, int anoColecao, boolean isPremium, double valorizacaoAnual, String transportadora) {

        Transportadora transportadoraAtual = null;
        for (Transportadora transportadoraItem : transportadoras) {
            if (transportadoraItem.getCodigo().equals(transportadora)) {
                transportadoraAtual = transportadoraItem;
                break;
            }
        }

        if (transportadoraAtual == null) {
            // handle the case where the transportadora is not found
            return;
        }

        Mala novaMala = new Mala(descricao, marca, precoBase, isNovo, avaliacaoEstado, numDonosAnteriores, 0, false, dimensao, material, anoColecao, isPremium, valorizacaoAnual, utilizadorAtual.getCodigo(), transportadoraAtual);
        artigos.add(novaMala);
    }


    public List<String> listarArtigosDoUtilizador() {
        List<String> listaArtigos = new ArrayList<>();

        for (Artigo artigo : artigos) {
            if (artigo.getDono() != null && artigo.getDono().equals(utilizadorAtual.getCodigo())) {
                listaArtigos.add(artigo.toString());
            }
        }
        return listaArtigos;
    }



    public void apagarArtigo(String codigoArtigo) {
        artigos.removeIf(artigo -> artigo.getCodigo().equals(codigoArtigo));
    }


    public List<String> listarSapatilhasDisponiveis() {
        List<String> sapatilhasDisponiveis = new ArrayList<>();

        for (Artigo artigo : artigos) {
            if (artigo instanceof Sapatilha && !artigo.isVendido()) {
                sapatilhasDisponiveis.add(artigo.toString());
            }
        }

        return sapatilhasDisponiveis;
    }

    public void adicionarArtigoEncomenda(String codigoArtigo) {
        for (Artigo artigo : artigos) {
            if (artigo.getCodigo().equals(codigoArtigo)) {
                encomendaAtual.adicionarArtigo(artigo);
                artigo.setisVendido(true);
                return;
            }
        }

    }

    public List<String> listarTshirtsDisponiveis() {
        List<String> tshirtsDisponiveis = new ArrayList<>();

        for (Artigo artigo : artigos) {
            if (artigo instanceof TShirt && !artigo.isVendido()) {
                tshirtsDisponiveis.add(artigo.toString());
            }
        }

        return tshirtsDisponiveis;
    }

    public List<String> listarMalasDisponiveis() {
        List<String> malasDisponiveis = new ArrayList<>();

        for (Artigo artigo : artigos) {
            if (artigo instanceof Mala && !artigo.isVendido()) {
                malasDisponiveis.add(artigo.toString());
            }
        }

        return malasDisponiveis;
    }


    public List<String> listarTransportadoraDisponiveis() {
        List<String> transportadorasDisponiveis = new ArrayList<>();

        for (Transportadora transportadora : transportadoras) {
            transportadorasDisponiveis.add(transportadora.toString());
        }
        return transportadorasDisponiveis;
    }

    public double getPrecoTotalEncomenda() {
        if (encomendaAtual == null) {
            return 0;
        }

        double totalPrecoArtigos = 0;

        for (Artigo artigo : encomendaAtual.getArtigos()) {
            totalPrecoArtigos += artigo.getPrecoFinal();
        }

        double custoExpedicao = transportadoras.stream()
                .mapToDouble(transportadora -> transportadora.calcularCustoExpedicao(encomendaAtual))
                .sum();

        return totalPrecoArtigos + custoExpedicao;
    }


    public List<String> listarArtigosEncomenda() {
        if (encomendaAtual == null) {
            return new ArrayList<>();
        }

        List<String> artigosEncomenda = new ArrayList<>();
        List<Artigo> artigos = encomendaAtual.getArtigos();

        for (Artigo artigo : artigos) {
            artigosEncomenda.add(artigo.toString());
        }

        return artigosEncomenda;
    }

    public void removerArtigoEncomenda(String codigoArtigo) {
        if (encomendaAtual == null) {
            return;
        }

        for (Artigo artigo : encomendaAtual.getArtigos()) {
            if (artigo.getCodigo().equals(codigoArtigo)) {
                artigos.removeIf(a -> a.getCodigo().equals(codigoArtigo));
                encomendaAtual.getArtigos().remove(artigo);
                return;
            }
        }
    }

    public String getUtilizadorComMaiorFaturacao(LocalDate dataInicio, LocalDate dataFim) {
        Map<String, Double> utilizadoresFaturacao = new HashMap<>();
        double maiorFaturacao = 0;
        String utilizadorComMaiorFaturacao = "";

        for (Encomenda encomenda : encomendas) {
            LocalDate dataCriacao = encomenda.getDataCriacao();
            if (dataCriacao.isAfter(dataInicio) && dataCriacao.isBefore(dataFim) && encomenda.getEstado() == Encomenda.Estado.FINALIZADA) {
                Utilizador utilizador = encomenda.getUtilizador();
                double faturacaoEncomenda = encomenda.getPrecoFinal();

                double faturacaoUtilizador = utilizadoresFaturacao.getOrDefault(utilizador.getEmail(), 0.0);
                faturacaoUtilizador += faturacaoEncomenda;
                utilizadoresFaturacao.put(utilizador.getCodigo(), faturacaoUtilizador);

                if (faturacaoUtilizador > maiorFaturacao) {
                    maiorFaturacao = faturacaoUtilizador;
                    utilizadorComMaiorFaturacao = utilizador.getCodigo();
                }
            }
        }

        return utilizadorComMaiorFaturacao;
    }




    public void adicionarTransportadora(String nome, double valorBasePequeno, double valorBaseMedio, double valorBaseGrande, double margemLucro, boolean isPremium) {
        Transportadora novaTransportadora = new Transportadora(nome, valorBasePequeno, valorBaseMedio, valorBaseGrande, margemLucro, isPremium);
        transportadoras.add(novaTransportadora);
    }

    public String obterDetalhesTransportadora(String codigoTransportadora) {
        for (Transportadora transportadora : transportadoras) {
            if (transportadora.getCodigo().equals(codigoTransportadora)) {
                return transportadora.toString();
            }
        }
        return null;
    }

    public void finalizarEncomenda() throws EncomendaNaoExistenteException {
        if (encomendaAtual != null) {
            for (Artigo artigo : encomendaAtual.getArtigos()) {
                artigo.setisVendido(true);
            }
            encomendaAtual.finalizarEncomenda();
            encomendas.add(encomendaAtual);
            encomendaAtual = new Encomenda(utilizadorAtual);
        } else {
            throw new EncomendaNaoExistenteException("Não há encomenda em andamento.");
        }
    }

    public String getTransportadoraComMaiorQuantidadeArtigosFaturados(LocalDate dataInicio, LocalDate dataFim) {
        Map<String, Integer> transportadorasQuantidadeArtigos = new HashMap<>();

        for (Encomenda encomenda : encomendas) {
            LocalDate dataCriacao = encomenda.getDataCriacao();
            if ((!dataCriacao.isBefore(dataInicio) && !dataCriacao.isAfter(dataFim)) && encomenda.getEstado() == Encomenda.Estado.FINALIZADA) {
                List<Artigo> artigosEncomenda = encomenda.getArtigos();
                for (Artigo artigo : artigosEncomenda) {
                    Transportadora transportadora = artigo.getTransportadora();
                    if(transportadora != null) {
                        String codigoTransportadora = transportadora.getCodigo();
                        if(codigoTransportadora != null) {
                            int quantidadeArtigosFaturados = transportadorasQuantidadeArtigos.getOrDefault(codigoTransportadora, 0);
                            transportadorasQuantidadeArtigos.put(codigoTransportadora, quantidadeArtigosFaturados + 1);
                        }
                    }
                }
            }
        }

        return transportadorasQuantidadeArtigos.entrySet().stream()
                .max(Map.Entry.comparingByValue())
                .map(Map.Entry::getKey)
                .orElse("");
    }





    public List<String> listarEncomendasUtilizador(String emailUtilizador) {
        List<String> listaEncomendas = new ArrayList<>();

        for (Encomenda encomenda : encomendas) {
            Utilizador utilizador = encomenda.getUtilizador();
            if (utilizador != null && emailUtilizador.equals(utilizador.getEmail())) {
                listaEncomendas.add(encomenda.getCodigo());
            }
        }

        return listaEncomendas;
    }


    public double calcularFaturacaoUtilizador(Utilizador utilizador) {
        double faturacao = 0.0;

        for (Artigo artigo : utilizador.getArtigosVendidos()) {
            faturacao += artigo.getPrecoFinal();
        }

        return faturacao;
    }


    public List<String> getUtilizadoresOrdenadosPorFaturacao() {
        List<Utilizador> utilizadoresOrdenados = new ArrayList<>(utilizadores);
        utilizadoresOrdenados.sort(Comparator.comparingDouble(this::calcularFaturacaoUtilizador).reversed());

        List<String> utilizadoresOrdenadosString = new ArrayList<>();

        for (Utilizador utilizador : utilizadoresOrdenados) {
            double faturacao = calcularFaturacaoUtilizador(utilizador);
            utilizadoresOrdenadosString.add(utilizador.getNome() + " - Faturação: " + faturacao);
        }

        return utilizadoresOrdenadosString;
    }


    public double calcularFaturacaoVintage() {
        double faturacaoTotal = 0.0;
        for (Artigo artigo : artigos) {
            if (artigo.isVendido()) {
                if (artigo.isNovo()) {
                    faturacaoTotal += 0.5;
                } else {
                    faturacaoTotal += 0.25;
                }
            }
        }
        return faturacaoTotal;
    }

    public void gravarEstado(String nomeArquivo) {
        try (ObjectOutputStream oos = new ObjectOutputStream(new FileOutputStream(nomeArquivo))) {
            oos.writeObject(utilizadores);
            oos.writeObject(encomendas);
            oos.writeObject(artigos);
            oos.writeObject(transportadoras);
        } catch (IOException e) {

        }
    }

    public void carregarEstado(String nomeArquivo) {
        try (ObjectInputStream ois = new ObjectInputStream(new FileInputStream(nomeArquivo))) {
            utilizadores = (List<Utilizador>) ois.readObject();
            encomendas = (List<Encomenda>) ois.readObject();
            artigos = (List<Artigo>) ois.readObject();
            transportadoras = (List<Transportadora>) ois.readObject();
        } catch (IOException | ClassNotFoundException e) {

        }
    }

}