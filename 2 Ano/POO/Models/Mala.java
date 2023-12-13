package Projeto.Models;

import java.io.Serializable;
import java.time.LocalDate;

public class Mala extends ArtigoBase implements Serializable {
    private int dimensao;
    private String material;
    private int anoColecao;

    private boolean isPremium;
    private double valorizacaoAnual;


    public Mala(String descricao, String marca, double precoBase, boolean isNovo, double avaliacaoEstado,
                int numDonosAnteriores, double desconto, boolean isVendido, int dimensao, String material, int anoColecao, boolean isPremium, double valorizacaoAnual, String dono, Transportadora transportadora) {
        super(descricao, marca, precoBase, isNovo, avaliacaoEstado, numDonosAnteriores, desconto, isVendido, dono, transportadora);
        this.dimensao = dimensao;
        this.material = material;
        this.anoColecao = anoColecao;
        this.isPremium = isPremium;
        this.valorizacaoAnual = valorizacaoAnual;
    }


    public double getPrecoFinal() {
        if (isPremium) {
            int anosDesdeColecao = LocalDate.now().getYear() - anoColecao;
            double precoFinal = precoBase + (precoBase * valorizacaoAnual * anosDesdeColecao);
            return precoFinal;
        } else {
            return precoBase;
        }
    }

    public int getDimensao() {
        return dimensao;
    }

    public void setDimensao(int dimensao) {
        this.dimensao = dimensao;
    }

    public String getMaterial() {
        return material;
    }

    public void setMaterial(String material) {
        this.material = material;
    }

    public int getAnoColecao() {
        return anoColecao;
    }

    public void setAnoColecao(int anoColecao) {
        this.anoColecao = anoColecao;
    }

    public void setPremium(boolean premium) {
        isPremium = premium;
    }

    public double getValorizacaoAnual() {
        return valorizacaoAnual;
    }

    public void setValorizacaoAnual(double valorizacaoAnual) {
        this.valorizacaoAnual = valorizacaoAnual;
    }

    @Override
    public boolean isPremium() {
        return isPremium;
    }

    @Override
    public boolean isVendido() {
        return isVendido;
    }


    @Override
    public String toString() {
        return "Mala{" +
                "dimensao=" + dimensao +
                ", codigo=" + getCodigo() +
                ", material='" + material + '\'' +
                ", anoColecao=" + anoColecao +
                ", isPremium=" + isPremium +
                ", valorizacaoAnual=" + valorizacaoAnual +
                ", descricao='" + descricao + '\'' +
                ", marca='" + marca + '\'' +
                ", precoBase=" + precoBase +
                ", isNovo=" + isNovo +
                ", avaliacaoEstado=" + avaliacaoEstado +
                ", numDonosAnteriores=" + numDonosAnteriores +
                ", desconto=" + desconto +
                ", isVendido=" + isVendido +
                ", dono='" + dono + '\'' +
                '}';
    }
}