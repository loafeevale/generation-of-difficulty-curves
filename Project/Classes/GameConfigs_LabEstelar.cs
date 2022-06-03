using UnityEngine;
using System.Collections;

//public class GameConfigs_LabEstelar : IGameConfig
//{
//	public int id_level { get; set; }
//	public float velocidade;
//	public float numJogPes;
//	public float numJogMaos;
//	public float numCoresErr;
//	public float numNumsErr;
//	public float numJogComb;
//}
[System.Serializable]
public class DificuldadeLaboratorioEstelar
{
    public float velocidade;
    public float numJogMaos;
    public float numJogPes;
    public float numJogComb;
    public float numCoresErr;
    public float numNumsErr;
}
[System.Serializable]
public class TabelaLaboratorioEstelar
{
    public DificuldadeLaboratorioEstelar dB; // Dificuldade Base

    public DificuldadeLaboratorioEstelar[] tb; //Tabela

    public DificuldadeLaboratorioEstelar inC; //Incremento por Ciclo

    [Header("Configurações de Jogo")]
    public float velocidadeDefault;
    public float distAtivHor;
    public float ratioDistPes;

    public DificuldadeLaboratorioEstelar ConfiguracaoAtual(int nivel)
    {
        if (nivel < 1)
            nivel = 1;

        var conf = new DificuldadeLaboratorioEstelar();
        var ciclo = nivel % 9 > 0 ? (int)(nivel / 9F) : nivel / 9 - 1;

        nivel -= (ciclo * 9) + 1;

        conf.velocidade = dB.velocidade + (inC.velocidade * ciclo) + tb[nivel].velocidade;
        conf.numJogMaos = dB.numJogMaos + (inC.numJogMaos * ciclo) + tb[nivel].numJogMaos;
        conf.numJogPes = dB.numJogPes + (inC.numJogPes * ciclo) + tb[nivel].numJogPes;
        conf.numJogComb = dB.numJogComb + (inC.numJogComb * ciclo) + tb[nivel].numJogComb;
        conf.numCoresErr = dB.numCoresErr + (inC.numCoresErr * ciclo) + tb[nivel].numCoresErr;
        conf.numNumsErr = dB.numNumsErr + (inC.numNumsErr * ciclo) + tb[nivel].numNumsErr;

        return conf;
    }
}