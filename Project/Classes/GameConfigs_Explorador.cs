using UnityEngine;
using System.Collections;

//public class GameConfigs_Explorador : IGameConfig
//{
//    public int id_level {get;set;}

//    public float velocidade;
//    public float disSpawnObj;
//    public float altPulo;
//    public float altAgachar;
//    public float qtdePulos;
//    public float qtdeAgachar;
//    public float qtdeobstLat;
//    public float itensNCatalog;
//    public float itensTot;
//    public float itensErrados;
//}
[System.Serializable]
public class DificuldadeExplorador
{
    public float velocidade;
    public float qtdObstLat;
    public float qtdPulos;
    public float qtdAbaixar;
    public float itensNCatalog;
    public float itensTot;
    public float itensErrados;
}
[System.Serializable]
public class TabelaExplorador
{
    public DificuldadeExplorador dB; // Dificuldade Base

    public DificuldadeExplorador[] tb; //Tabela

    public DificuldadeExplorador inC; //Incremento por Ciclo

    [Header("Configurações de Jogo")]
    public float velocidadeDefault;
    public float disSpawnObj;
    public float altPulo;
    public float altAbaixar;

    public DificuldadeExplorador ConfiguracaoAtual(int nivel)
    {
        if (nivel < 1)
            nivel = 1;

        var conf = new DificuldadeExplorador();
        var ciclo = nivel % 9 > 0 ? (int)(nivel / 9F) : nivel / 9 - 1;

        nivel -= (ciclo * 9) + 1;

        conf.velocidade = dB.velocidade + (inC.velocidade * ciclo) + tb[nivel].velocidade;
        conf.qtdObstLat = dB.qtdObstLat + (inC.qtdObstLat * ciclo) + tb[nivel].qtdObstLat;
        conf.qtdPulos = dB.qtdPulos + (inC.qtdPulos * ciclo) + tb[nivel].qtdPulos;
        conf.qtdAbaixar = dB.qtdAbaixar + (inC.qtdAbaixar * ciclo) + tb[nivel].qtdAbaixar;
        conf.itensNCatalog = dB.itensNCatalog + (inC.itensNCatalog * ciclo) + tb[nivel].itensNCatalog;
        conf.itensTot = dB.itensTot + (inC.itensTot * ciclo) + tb[nivel].itensTot;
        conf.itensErrados = dB.itensErrados + (inC.itensErrados * ciclo) + tb[nivel].itensErrados;

        conf.itensNCatalog = Mathf.Min(conf.itensNCatalog, 7); //Um item sempre será catalogado

        return conf;
    }
}