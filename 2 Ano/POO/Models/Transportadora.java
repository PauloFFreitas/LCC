package Projeto.Models;

import java.io.Serializable;
import java.util.List;
import java.util.stream.Collectors;

public class Transportadora implements Serializable {
    private static int count = 1;
    private String codigo;
    private String nome;
    private boolean isPremium;
    private double valorBasePequeno;
    private double valorBaseMedio;
    private double valorBaseGrande;
    private double margemLucro;

    private List<Artigo> artigos;


    public Transportadora(String nome, double valorBasePequeno, double valorBaseMedio,
                          double valorBaseGrande, double margemLucro, boolean isPremium) {
        this.codigo = "T" + (++count);
        this.nome = nome;
        this.valorBasePequeno = valorBasePequeno;
        this.valorBaseMedio = valorBaseMedio;
        this.valorBaseGrande = valorBaseGrande;
        this.margemLucro = margemLucro;
        this.isPremium = isPremium;
    }

    public static int getCount() {
        return count;
    }

    public static void setCount(int count) {
        Transportadora.count = count;
    }

    public String getCodigo() {
        return codigo;
    }

    public void setCodigo(String codigo) {
        this.codigo = codigo;
    }

    public String getNome() {
        return nome;
    }

    public void setNome(String nome) {
        this.nome = nome;
    }
    public boolean isPremium() {
        return isPremium;
    }

    public void setPremium(boolean premium) {
        isPremium = premium;
    }

    public double getValorBasePequeno() {
        return valorBasePequeno;
    }

    public void setValorBasePequeno(double valorBasePequeno) {
        this.valorBasePequeno = valorBasePequeno;
    }

    public double getValorBaseMedio() {
        return valorBaseMedio;
    }

    public void setValorBaseMedio(double valorBaseMedio) {
        this.valorBaseMedio = valorBaseMedio;
    }

    public double getValorBaseGrande() {
        return valorBaseGrande;
    }

    public void setValorBaseGrande(double valorBaseGrande) {
        this.valorBaseGrande = valorBaseGrande;
    }

    public double getMargemLucro() {
        return margemLucro;
    }

    public void setMargemLucro(double margemLucro) {
        this.margemLucro = margemLucro;
    }


    public double calcularCustoExpedicao(Encomenda encomenda) {
        double custo = 0;
        List<Artigo> artigos = encomenda.getArtigos().stream()
                .filter(art -> art.getTransportadora().getCodigo().equals(this.codigo)).toList();
        int numArtigos = artigos.size();

        switch (encomenda.getDimensao()) {
            case GRANDE:
                custo = this.valorBaseGrande * numArtigos;
                break;
            case MEDIO:
                custo = this.valorBaseMedio * numArtigos;
                break;
            case PEQUENO:
            default:
                custo = this.valorBasePequeno * numArtigos;
                break;
        }

        if (this.isPremium) {
            custo *= 0.9;
        }


        custo *= (1 + this.margemLucro);

        return custo;
    }

    @Override
    public String toString() {
        return "Transportadora{" +
                "codigo='" + codigo + '\'' +
                ", nome='" + nome + '\'' +
                ", isPremium=" + isPremium +
                ", valorBasePequeno=" + valorBasePequeno +
                ", valorBaseMedio=" + valorBaseMedio +
                ", valorBaseGrande=" + valorBaseGrande +
                ", margemLucro=" + margemLucro +
                '}';
    }
}
