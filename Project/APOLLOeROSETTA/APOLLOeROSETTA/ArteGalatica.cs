using APOLLOeROSETTA;
using System;
using System.Data;
using System.IO;

public static class IncrementoAG
{
    //                                           Valor Base                                                       Incremento por ciclo
    //                                                |                                                                     |
    //                                                |     |-------------------Incremento por nível------------------|     |     Peso                                                             
    //                                                |     |1      2      3      4      5      6      7      8      9|     |      |                
    //Velocidade de aparição das bolinhas             |     |                                                         |     |      |
    public static double[] Veloci = new double[12] {  1,     0,     0,     0,  0.05,  0.05,   0.1,  0.05,  0.05,  0.05,   0.1,     4 };
    //Quantidade de bolinhas certas                                                   
    public static double[] Qtdblc = new double[12] { 35,     0,     5,    10,    15,    20,    25,    27,    24,    21,    25,     1 };
    //Quantidade de bolinhas erradas(P&B)                                             
    public static double[] Qtdble = new double[12] {  1,     0,     0,     1,     1,     1,     2,     2,     1,     1,     2,     1 };
    //Aparições Mosca                                                                                                             
    public static double[] ApaMos = new double[12] {  0,     0,     1,     1,     1,     2,     3,     1,     1,     1,     1,     2 };
    //Tempo para segurar mosca                                                                                                    
    public static double[] TmpMos = new double[12] {  1,     0,     0,     0,   0.1,   0.1,   0.2,  -0.1,  -0.1,  -0.1,   0.2,     2 };

}

public class ArteGalatica
{
    //Velocidade de aparição das bolinhas 
    public double Veloci;
    //Quantidade de bolinhas certas 
    public double Qtdblc;
    //Quantidade de bolinhas erradas(P&B)  
    public double Qtdble;
    //Aparições Mosca 
    public double ApaMos;
    //Tempo para segurar mosca
    public double TmpMos;
    //Dificuldade total do nível
    public double Dificuldade;

    public void ConfiguracaoIntervalo(int nIni, int nFim)
    {
        DataSet ds = new DataSet();
        DataTable dt = new DataTable();
        dt.Columns.Add("Nível", typeof(int));
        dt.Columns.Add("Dificuldade", typeof(double));

        for (int i = nIni; i <= nFim; i++)
        {
            ConfiguracaoAtual(i);
            DataRow r = dt.NewRow();
            r[0] = i;
            r[1] = Dificuldade;
            dt.Rows.Add(r);
        }
        ds.Tables.Add(dt);
        Grafico.DesenhaGrafico(ds, "ArteGalatica", nIni, nFim);
    }

    public void ConfiguracaoAtual(int nivel)
    {
        int ciclo = (nivel / 9);
        //nvls múltiplos de 9 fazem parte do ciclo anteior...
        if ((nivel % 9) == 0)
        {
            ciclo--;
        }

        int cnivel = nivel - (ciclo * 9);
        Veloci = IncrementoAG.Veloci[0] + (IncrementoAG.Veloci[10] * ciclo) + IncrementoAG.Veloci[cnivel];
        Qtdblc = IncrementoAG.Qtdblc[0] + (IncrementoAG.Qtdblc[10] * ciclo) + IncrementoAG.Qtdblc[cnivel];
        Qtdble = IncrementoAG.Qtdble[0] + (IncrementoAG.Qtdble[10] * ciclo) + IncrementoAG.Qtdble[cnivel];
        ApaMos = IncrementoAG.ApaMos[0] + (IncrementoAG.ApaMos[10] * ciclo) + IncrementoAG.ApaMos[cnivel];
        TmpMos = IncrementoAG.TmpMos[0] + (IncrementoAG.TmpMos[10] * ciclo) + IncrementoAG.TmpMos[cnivel];

        Dificuldade = (Veloci * IncrementoAG.Veloci[11]) +
                      (Qtdblc * IncrementoAG.Qtdblc[11]) +
                      (Qtdble * IncrementoAG.Qtdble[11]) +
                      (ApaMos * IncrementoAG.ApaMos[11]) +
                      (TmpMos * IncrementoAG.TmpMos[11]);


        Console.WriteLine("{0,13}{1,13}{2,13}{3,13}{4,13}{5,13}{6,13}", nivel, Veloci, Qtdblc, Qtdble, ApaMos, TmpMos, Dificuldade);
        using (StreamWriter arquivo = new StreamWriter("ArteGalatica.csv", true))
        {
            if (nivel == 1) arquivo.WriteLine("nivel;Veloci;Qtdblc;Qtdble;ApaMos;TmpMos;Dificuldade");
            arquivo.WriteLine("{0,13};{1,13};{2,13};{3,13};{4,13};{5,13};{6,13}", nivel, Veloci, Qtdblc, Qtdble, ApaMos, TmpMos, Dificuldade);
        }
    }
}