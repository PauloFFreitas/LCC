package Projeto.Models;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

public class Utilizador implements Serializable {
    private static int count = 1;
    private String codigo;
    private String email;
    private String nome;
    private String morada;
    private String numeroFiscal;
    private List<Artigo> artigosAVenda;
    private List<Artigo> artigosVendidos;
    private List<Artigo> artigosAdquiridos;

    public Utilizador(String email, String nome, String morada, String numeroFiscal)  {
        this.codigo = "U" + (++count);
        this.email = email;
        this.nome = nome;
        this.morada = morada;
        this.numeroFiscal = numeroFiscal;
        this.artigosAVenda = new ArrayList<>();
        this.artigosVendidos = new ArrayList<>();
        this.artigosAdquiridos = new ArrayList<>();
    }

    public static int getCount() {
        return count;
    }

    public static void setCount(int count) {
        Utilizador.count = count;
    }

    public String getCodigo() {
        return codigo;
    }

    public void setCodigo(String codigo) {
        this.codigo = codigo;
    }

    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
    }

    public String getNome() {
        return nome;
    }

    public void setNome(String nome) {
        this.nome = nome;
    }

    public String getMorada() {
        return morada;
    }

    public void setMorada(String morada) {
        this.morada = morada;
    }

    public String getNumeroFiscal() {
        return numeroFiscal;
    }

    public void setNumeroFiscal(String numeroFiscal) {
        this.numeroFiscal = numeroFiscal;
    }

    public List<Artigo> getArtigosAVenda() {
        return artigosAVenda;
    }

    public void setArtigosAVenda(List<Artigo> artigosAVenda) {
        this.artigosAVenda = artigosAVenda;
    }

    public List<Artigo> getArtigosVendidos() {
        return artigosVendidos;
    }

    public void setArtigosVendidos(List<Artigo> artigosVendidos) {
        this.artigosVendidos = artigosVendidos;
    }

    public List<Artigo> getArtigosAdquiridos() {
        return artigosAdquiridos;
    }

    public void setArtigosAdquiridos(List<Artigo> artigosAdquiridos) {
        this.artigosAdquiridos = artigosAdquiridos;
    }

    public void adicionarArtigoAVenda(Artigo artigo) {
        this.artigosAVenda.add(artigo);
    }

    public void venderArtigo(Artigo artigo) {
        this.artigosAVenda.remove(artigo);
        this.artigosVendidos.add(artigo);
    }

    public void adquirirArtigo(Artigo artigo) {
        this.artigosAdquiridos.add(artigo);
    }



}