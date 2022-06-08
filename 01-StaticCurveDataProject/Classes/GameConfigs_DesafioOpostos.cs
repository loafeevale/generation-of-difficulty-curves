using UnityEngine;
using System.Collections;

//public class GameConfigs_DesafioOpostos : IGameConfig
//{
//	public int id_level { get; set; }
//	public float distancia;
//	public float velocidade;
//	public float jogMembro;
//	public float jogHorizontal;
//	public float tempoResposta;
//	public float jogVertical;
//	public float jogNome;
//	public float jogTamanho;
//	public float jogInvertidas;
//}
[System.Serializable]
public class DificuldadeDesafioDosOpostos
{
    public float velocidade;
    public int jogInvertidas;
    public int jogTamanho;
    public int jogNome;
    public int jogVertical;
    public int jogHorizontal;
    public int jogMembro;
    public float tempoResposta;
}
[System.Serializable]
public class TabelaDesafioDosOpostos
{
    public DificuldadeDesafioDosOpostos dB; // Dificuldade Base

    public DificuldadeDesafioDosOpostos[] tb; //Tabela

    public DificuldadeDesafioDosOpostos inC; //Incremento por Ciclo

    [Header("Configurações de Jogo")]
    public float velocidadeDefault;
    public float tempoRespostaDefault;
    public float distancia;

    public DificuldadeDesafioDosOpostos ConfiguracaoAtual(int nivel)
    {
        if (nivel < 1)
            nivel = 1;

        var conf = new DificuldadeDesafioDosOpostos();
        var ciclo = nivel % 9 > 0 ? (int)(nivel / 9F) : nivel / 9 - 1;

        nivel -= (ciclo * 9) + 1;

        conf.velocidade = dB.velocidade + (inC.velocidade * ciclo) + tb[nivel].velocidade;
        conf.jogInvertidas = dB.jogInvertidas + (inC.jogInvertidas * ciclo) + tb[nivel].jogInvertidas;
        conf.jogTamanho = dB.jogTamanho + (inC.jogTamanho * ciclo) + tb[nivel].jogTamanho;
        conf.jogNome = dB.jogNome + (inC.jogNome * ciclo) + tb[nivel].jogNome;
        conf.jogVertical = dB.jogVertical + (inC.jogVertical * ciclo) + tb[nivel].jogVertical;
        conf.jogHorizontal = dB.jogHorizontal + (inC.jogHorizontal * ciclo) + tb[nivel].jogHorizontal;
        conf.jogMembro = dB.jogMembro + (inC.jogMembro * ciclo) + tb[nivel].jogMembro;
        conf.tempoResposta = dB.tempoResposta + (inC.tempoResposta * ciclo) + tb[nivel].tempoResposta;

        return conf;
    }
}