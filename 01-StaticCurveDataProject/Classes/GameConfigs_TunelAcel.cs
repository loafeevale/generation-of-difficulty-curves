using UnityEngine;
using System.Collections;

//public class GameConfigs_TunelAcel : IGameConfig
//{
//    public int id_level { get; set; }
//}

[System.Serializable]
public class DificuldadeTunelAcelerador
{
    public float velocidade;
    public float qntDeObst;
    public float distCamInvertida;
    public float qntDeObstInvertidos;
}

[System.Serializable]
public class TabelaTunelAcelerador
{
    public DificuldadeTunelAcelerador dB; // Dificuldade Base

    public DificuldadeTunelAcelerador[] tb; //Tabela

    public DificuldadeTunelAcelerador inC; //Incremento por Ciclo

    [Header("Configurações de Jogo")]
    public float velocidadeDefault;
    public float distObst;
    public float distCamDefault;
    public float distCamInvertidaDefault;

    public DificuldadeTunelAcelerador ConfiguracaoAtual(int nivel)
    {
        if (nivel < 1)
            nivel = 1;

        var conf = new DificuldadeTunelAcelerador();
        var ciclo = nivel % 9 > 0 ? (int)(nivel / 9F) : nivel / 9 - 1;

        nivel -= (ciclo * 9) + 1;

        conf.velocidade  = dB.velocidade + (inC.velocidade * ciclo) + tb[nivel].velocidade;
        conf.qntDeObst = dB.qntDeObst + (inC.qntDeObst * ciclo) + tb[nivel].qntDeObst;
        conf.distCamInvertida = dB.distCamInvertida + (inC.distCamInvertida * ciclo) + tb[nivel].distCamInvertida;
        conf.qntDeObstInvertidos = dB.qntDeObstInvertidos + (inC.qntDeObstInvertidos * ciclo) + tb[nivel].qntDeObstInvertidos;

        return conf;
    }
}