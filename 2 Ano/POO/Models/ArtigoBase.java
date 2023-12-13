package Projeto.Models;

import java.io.Serializable;

public abstract class ArtigoBase implements Artigo, Serializable {
    private static int count = 1;
    private String codigo;
    protected String descricao;
    protected String marca;
    protected double precoBase;
    protected boolean isNovo;
    protected double avaliacaoEstado;
    protected int numDonosAnteriores;
    protected double desconto;

    protected boolean isVendido;

    protected Transportadora transportadora;

    protected String dono;

    public ArtigoBase(String descricao, String marca, double precoBase, boolean isNovo, double avaliacaoEstado, int numDonosAnteriores, double desconto, boolean isVendido, String dono, Transportadora transportadora) {
        this.codigo = "A" + (++count);
        this.descricao = descricao;
        this.marca = marca;
        this.precoBase = precoBase;
        this.isNovo = isNovo;
        this.avaliacaoEstado = avaliacaoEstado;
        this.numDonosAnteriores = numDonosAnteriores;
        this.desconto = desconto;
        this.isVendido = isVendido;
        this.dono = dono;
        this.transportadora = transportadora;
    }

    public double getPrecoFinal() {
        return precoBase * (1 - desconto);
    }

    public String getDono() {
        return dono;
    }

    public void setDono(String dono) {
        this.dono = dono;
    }

    @Override
    public String getCodigo() {
        return codigo;
    }

    public void setCodigo(String codigo) {
        this.codigo = codigo;
    }

    @Override
    public String getDescricao() {
        return descricao;
    }

    public void setDescricao(String descricao) {
        this.descricao = descricao;
    }

    @Override
    public String getMarca() {
        return marca;
    }

    public void setMarca(String marca) {
        this.marca = marca;
    }

    @Override
    public double getPrecoBase() {
        return precoBase;
    }

    public void setPrecoBase(double precoBase) {
        this.precoBase = precoBase;
    }

    public boolean isNovo() {
        return isNovo;
    }

    public void setIsNovo(boolean novo) {
        isNovo = novo;
    }

    public double getAvaliacaoEstado() {
        return avaliacaoEstado;
    }

    public void setAvaliacaoEstado(double avaliacaoEstado) {
        this.avaliacaoEstado = avaliacaoEstado;
    }

    public int getNumDonosAnteriores() {
        return numDonosAnteriores;
    }

    public void setNumDonosAnteriores(int numDonosAnteriores) {
        this.numDonosAnteriores = numDonosAnteriores;
    }

    public double getDesconto() {
        return desconto;
    }

    public void setDesconto(double desconto) {
        this.desconto = desconto;
    }

    public boolean isVendido() {
        return isVendido;
    }

    public void setisVendido(boolean vendido) {
        isVendido = vendido;
    }

    @Override
    public double getEstado() {
        return avaliacaoEstado;
    }

    public void setEstado(String estado) {
        try {
            double avaliacao = Double.parseDouble(estado);
            setAvaliacaoEstado(avaliacao);
        } catch (NumberFormatException e) {
            System.out.println("Estado inválido. Não foi possível converter para double.");
        }
    }

    public static int getCount() {
        return count;
    }

    public static void setCount(int count) {
        ArtigoBase.count = count;
    }

    public void setNovo(boolean novo) {
        isNovo = novo;
    }

    public void setVendido(boolean vendido) {
        isVendido = vendido;
    }

    public Transportadora getTransportadora() {
        return transportadora;
    }


    public void setTransportadora(Transportadora transportadora) {
        this.transportadora = transportadora;
    }

    @Override
    public boolean isPremium() {
        return false;
    }


}
