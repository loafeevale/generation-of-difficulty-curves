using UnityEngine;
using System.Collections;
using System.Xml.Serialization;

//public class GameConfigs_DecifCod : IGameConfig
//{
//    public int id_level { get; set; }

//    public float tBaseTroca;
//    public float velTroca;
//    public float altPulo;
//    public float distAlvosHor;
//    public int numJogMao;
//    public int numJogPe;
//    public int numTrocaPeMao;
//    public int numPalCorrTres;
//    public int numPalIncTres;
//    public int numPalCorrQuatro;
//    public int numPalIncQuatro;
//    public bool detecTempoOuEspera;
//    public float tempoTrocaInibidor;
//}

[System.Serializable]
public class DificuldadeDecifrandoCodigos
{
    public float velocidadeDeTroca;
    public int numJogMao;
    public int numJogPe;
    public int numTrocaPeMao;
    public int numPalCorrTres;
    public int numPalIncTres;
    public int numPalCorrQuatro;
    public int numPalIncQuatro;
}
[System.Serializable]
public class TabelaDecifrandoCodigos
{
    public DificuldadeDecifrandoCodigos dB; // Dificuldade Base

    public DificuldadeDecifrandoCodigos[] tb; //Tabela

    public DificuldadeDecifrandoCodigos inC; //Incremento por Ciclo

    [Header("Configurações de Jogo")]
    public float tBaseTroca;
    public float altPulo;
    public float distAlvosHor;
    public float ratioDistPes;
    public bool detecTempoOuEspera;
    public float tempoTrocaInibidor;

    public DificuldadeDecifrandoCodigos ConfiguracaoAtual(int nivel)
    {
        if (nivel < 1)
            nivel = 1;

        var conf = new DificuldadeDecifrandoCodigos();
        var ciclo = nivel % 9 > 0 ? (int)(nivel / 9F) : nivel / 9 - 1;

        nivel -= (ciclo * 9) + 1;

        conf.velocidadeDeTroca = dB.velocidadeDeTroca + (inC.velocidadeDeTroca * ciclo) + tb[nivel].velocidadeDeTroca;
        conf.numJogMao = dB.numJogMao + (inC.numJogMao * ciclo) + tb[nivel].numJogMao;
        conf.numJogPe = dB.numJogPe + (inC.numJogPe * ciclo) + tb[nivel].numJogPe;
        conf.numTrocaPeMao = dB.numTrocaPeMao + (inC.numTrocaPeMao * ciclo) + tb[nivel].numTrocaPeMao;

        conf.numPalCorrTres = dB.numPalCorrTres + (inC.numPalCorrTres * ciclo) + tb[nivel].numPalCorrTres;
        conf.numPalIncTres = dB.numPalIncTres + (inC.numPalIncTres * ciclo) + tb[nivel].numPalIncTres;
        conf.numPalCorrQuatro = dB.numPalCorrQuatro + (inC.numPalCorrQuatro * ciclo) + tb[nivel].numPalCorrQuatro;
        conf.numPalIncQuatro = dB.numPalIncQuatro + (inC.numPalIncQuatro * ciclo) + tb[nivel].numPalIncQuatro;

        return conf;
    }
}