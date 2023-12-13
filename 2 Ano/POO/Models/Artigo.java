package Projeto.Models;

public interface Artigo {
    String getCodigo();
    String getDescricao();
    String getMarca();
    double getPrecoBase();
    double getPrecoFinal();
    String getDono();
    double getEstado();
    boolean isNovo();
    double getAvaliacaoEstado();
    int getNumDonosAnteriores();
    double getDesconto();
    boolean isVendido();

    void setCodigo(String codigo);
    void setDescricao(String descricao);
    void setMarca(String marca);
    void setPrecoBase(double precoBase);
    void setDono(String dono);
    void setIsNovo(boolean isNovo);
    void setAvaliacaoEstado(double avaliacaoEstado);
    void setNumDonosAnteriores(int numDonosAnteriores);
    void setDesconto(double desconto);
    void setisVendido(boolean b);

    boolean isPremium();

    Transportadora getTransportadora();
}
