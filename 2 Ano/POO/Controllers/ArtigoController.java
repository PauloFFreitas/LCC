package Controllers;

import Projeto.Models.Artigo;
import Projeto.Models.Mala;
import Projeto.Models.Sapatilha;
import Projeto.Models.TShirt;

import java.util.List;
import java.util.Optional;

public class ArtigoController {
    public Artigo criarSapatilha(String codigo, String descricao, String marca, double precoBase, int tamanho, boolean possuiAtacadores, String cor, int anoLancamento, int numeroDonos, double estadoUtilizacao) {
        return new Sapatilha(codigo, descricao, marca, precoBase, tamanho, possuiAtacadores, cor, anoLancamento, numeroDonos, estadoUtilizacao);
    }

    public Artigo criarMala(String codigo, String descricao, String marca, double precoBase, String dimensao, String material, int anoColecao, boolean premium, double valorizacaoAnual) {
        return new Mala(codigo, descricao, marca, precoBase, dimensao, material, anoColecao, premium, valorizacaoAnual);
    }

    public Artigo criarTShirt(String codigo, String descricao, String marca, double precoBase, String tamanho, String padrao, boolean usada) {
        return new TShirt(codigo, descricao, marca, precoBase, tamanho, padrao, usada);
    }

    public double calcularPrecoFinal(Artigo artigo) {
        return artigo.getPrecoFinal();
    }

    public boolean removerArtigo(List<Artigo> artigos, String codigo) {
        Optional<Artigo> artigoToRemove = artigos.stream()
                .filter(artigo -> artigo.getCodigo().equals(codigo))
                .findFirst();

        if (artigoToRemove.isPresent()) {
            artigos.remove(artigoToRemove.get());
            return true;
        }

        return false;
    }
}
}