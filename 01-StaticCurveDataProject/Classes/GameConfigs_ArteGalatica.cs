using UnityEngine;
using System.Collections;

//public class GameConfigs_ArteGalatica : IGameConfig
//{
//    public int id_level { get; set; }
//    public float objsSegundo;
//    public float escalaObjs;
//    public float escalaCursor;
//    public float qtdeBolinhas;
//    public float qtdeBolinhasErr;
//    public float tempoSegurMosca;
//    public float qtdeAparMosca;
//    public bool usarMaoDir;
//}
[System.Serializable]
public class DificuldadeArteGalatica
{
    public float objsSegundo;
    public int qtdBolinhas;
    public int qtdBolinhasIncorretas;
    public int qtdAparMosca;
    public float tempoSegurarMosca;
}
[System.Serializable]
public class TabelaArteGalatica
{
    public DificuldadeArteGalatica dB; // Dificuldade Base

    public DificuldadeArteGalatica[] tb; //Tabela

    public DificuldadeArteGalatica inC; //Incremento por Ciclo

    [Header("Configurações de Jogo")]
    public float escalaObjs;
    public float escalaCursor;
    public bool pbDescontaPercentual;
    public bool colisoresDuplos;
    public bool utilizarSplines;
    public float velocidadeSplines;

    public DificuldadeArteGalatica ConfiguracaoAtual(int nivel)
    {
        if (nivel < 1)
            nivel = 1;

        var conf = new DificuldadeArteGalatica();
        var ciclo = nivel % 9 > 0 ? (int)(nivel / 9F) : nivel / 9 - 1;

        nivel -= (ciclo * 9) + 1;

        conf.objsSegundo = dB.objsSegundo + (inC.objsSegundo * ciclo) + tb[nivel].objsSegundo;
        conf.qtdBolinhas = dB.qtdBolinhas + (inC.qtdBolinhas * ciclo) + tb[nivel].qtdBolinhas;
        conf.qtdBolinhasIncorretas = dB.qtdBolinhasIncorretas + (inC.qtdBolinhasIncorretas * ciclo) + tb[nivel].qtdBolinhasIncorretas;
        conf.qtdAparMosca = dB.qtdAparMosca + (inC.qtdAparMosca * ciclo) + tb[nivel].qtdAparMosca;
        conf.tempoSegurarMosca = dB.tempoSegurarMosca + (inC.tempoSegurarMosca * ciclo) + tb[nivel].tempoSegurarMosca;

        return conf;
    }
}