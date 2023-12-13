package Projeto.Models;

import java.io.Serializable;
import java.time.Year;

public class Sapatilha extends ArtigoBase implements Serializable {
    private int tamanho;
    private boolean temAtacadores;
    private String cor;
    private Year dataLancamentoColecao;
    private boolean isPremium;



    public Sapatilha(String descricao, String marca, double precoBase, boolean isNovo, boolean isVendido, double avaliacaoEstado, int numDonosAnteriores, double desconto, int tamanho, boolean temAtacadores, String cor, Year dataLancamentoColecao, boolean isPremium, String dono, Transportadora transportadora) {
        super(descricao, marca, precoBase, isNovo, avaliacaoEstado, numDonosAnteriores, desconto, isVendido, dono,transportadora);
        this.tamanho = tamanho;
        this.temAtacadores = temAtacadores;
        this.cor = cor;
        this.dataLancamentoColecao = dataLancamentoColecao;
        this.isPremium = isPremium;
    }

    @Override
    public double getPrecoFinal() {
        if (isPremium) {
            // Se for premium, o valor aumenta com o passar dos anos
            int anosDesdeLancamento = Year.now().getValue() - dataLancamentoColecao.getValue();
            return precoBase * (1 + 0.05 * anosDesdeLancamento);
        } else if (!isNovo || tamanho > 45) {
            // Se for usado ou novo com tamanho acima de 45, aplica a fórmula
            return precoBase - (precoBase / numDonosAnteriores * avaliacaoEstado);
        } else {
            // Se for novo e tamanho não acima de 45, retorna o preço base
            return precoBase;
        }
    }

    public int getTamanho() {
        return tamanho;
    }

    public void setTamanho(int tamanho) {
        this.tamanho = tamanho;
    }

    public boolean isTemAtacadores() {
        return temAtacadores;
    }

    public void setTemAtacadores(boolean temAtacadores) {
        this.temAtacadores = temAtacadores;
    }

    public String getCor() {
        return cor;
    }

    public void setCor(String cor) {
        this.cor = cor;
    }

    public Year getDataLancamentoColecao() {
        return dataLancamentoColecao;
    }

    public void setDataLancamentoColecao(Year dataLancamentoColecao) {
        this.dataLancamentoColecao = dataLancamentoColecao;
    }


    @Override
    public boolean isVendido() {
        return false;
    }

    @Override
    public void setEstado(String estado) {

    }

    @Override
    public void setIsNovo(boolean isNovo) {

    }

    @Override
    public void setisVendido(boolean b) {

    }

    @Override
    public boolean isPremium() {
        return isPremium;
    }

    @Override
    public String toString() {
        return "Sapatilha{" +
                "tamanho=" + tamanho +
                ", codigo=" + getCodigo() +
                ", temAtacadores=" + temAtacadores +
                ", cor='" + cor + '\'' +
                ", dataLancamentoColecao=" + dataLancamentoColecao +
                ", isPremium=" + isPremium +
                ", descricao='" + descricao + '\'' +
                ", marca='" + marca + '\'' +
                ", precoBase=" + precoBase +
                ", isNovo=" + isNovo +
                ", avaliacaoEstado=" + avaliacaoEstado +
                ", numDonosAnteriores=" + numDonosAnteriores +
                ", desconto=" + desconto +
                ", isVendido=" + isVendido +
                ", transportadora=" + transportadora +
                ", dono='" + dono + '\'' +
                '}';
    }
}
