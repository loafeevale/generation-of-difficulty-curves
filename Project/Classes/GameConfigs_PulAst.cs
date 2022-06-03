using UnityEngine;
using System.Collections;

//public class GameConfigs_PulAst : IGameConfig
//{
//	public int id_level {get; set; }
//    public float tBaseTroca;
//    public float velocidadeTroca; //Tabela
//	public float tempoResposta;
//	public float numTrocas; 
//	public float jogTotais; //Tabela
//	public int numCoresErradas; //Tabela
//	public int modoControleEixoX;
//}

[System.Serializable]
public class DificuldadePulandoAsteroides
{
    public float velocidadeDeTroca;
    public int jogadasTotais;
    public int jogadasInvertidas;
    public int coresNaLista;
}
[System.Serializable]
public class TabelaPulandoAsteroides
{
    public DificuldadePulandoAsteroides dB; // Dificuldade Base

    public DificuldadePulandoAsteroides[] tb; //Tabela

    public DificuldadePulandoAsteroides inC; //Incremento por Ciclo

    [Header("Configurações de Jogo")]
    public float tBaseTroca;
    public float tempoResposta;
    public int modoControleEixoX;

    public DificuldadePulandoAsteroides ConfiguracaoAtual(int nivel)
    {
        if (nivel < 1)
            nivel = 1;

        var conf = new DificuldadePulandoAsteroides();
        var ciclo = nivel % 9 > 0 ? (int)(nivel / 9F) : nivel / 9 - 1;

        nivel -= (ciclo * 9) + 1;

        conf.velocidadeDeTroca = dB.velocidadeDeTroca + (inC.velocidadeDeTroca * ciclo) + tb[nivel].velocidadeDeTroca;
        conf.jogadasTotais = dB.jogadasTotais + (inC.jogadasTotais * ciclo) + tb[nivel].jogadasTotais;
        conf.jogadasInvertidas = dB.jogadasInvertidas + (inC.jogadasInvertidas * ciclo) + tb[nivel].jogadasInvertidas;
        conf.coresNaLista = dB.coresNaLista + (inC.coresNaLista * ciclo) + tb[nivel].coresNaLista;

        conf.coresNaLista = Mathf.Min(conf.coresNaLista, 5);//Uma cor sempre estará disponível

        return conf;
    }
}