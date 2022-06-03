using APOLLOeROSETTA;
using System;
using System.Data;
using System.IO;

public static class IncrementoEX
{
    //                                          Valor Base                                                       Incremento por ciclo 
    //                                               |                                                                     |
    //                                               |     |-------------------Incremento por nível------------------|     |     Peso            
    //                                               |     |1      2      3      4      5      6      7      8      9|     |      |              
    //Velocidade do Personagem                       |     |                                                         |     |      |
    public static double[] Veloci = new double[12] { 1,     0,     0,     0,   0.1,   0.1,   0.2,     0,     0,     0,   0.2,     3 };
    //Quantidade de Obstáculos                                                 
    public static double[] QtdObs = new double[12] { 0,     0,     1,     2,     2,     3,     3,     4,     4,     4,     3,     1 };
    //Obstáculos de Pulo                                                                                                         
    public static double[] ObsPul = new double[12] { 0,     0,     0,     0,     0,     1,     1,     1,     1,     1,     2,     1 };
    //Obstáculos de Abaixar                                                                                                      
    public static double[] ObsAba = new double[12] { 0,     0,     0,     1,     1,     2,     2,     1,     1,     1,     2,     1 };
    //Itens que não devem ser catalogados                                                                                        
    public static double[] ItsNca = new double[12] { 0,     0,     0,     0,     1,     1,     2,     1,     1,     1,     2,     2 };
    //Quantidade Total de Objetos                                                                                                
    public static double[] QtdObj = new double[12] { 6,     0,     2,     3,     3,     4,     4,     4,     4,     4,     4,     1 };
    //Aparição de Objetos Errados                                                                                                
    public static double[] ApaOer = new double[12] { 0,     0,     0,     0,     1,     2,     3,     2,     1,     2,     3,     1 };
}

public class Explorador
{

    //Velocidade do Personagem 
    public double Veloci;
    //Quantidade de Obstáculos
    public double QtdObs;
    //Obstáculos de Pulo 
    public double ObsPul;
    //Obstáculos de Abaixar   
    public double ObsAba;
    //Itens que não devem ser catalogados 
    public double ItsNca;
    //Quantidade Total de Objetos 
    public double QtdObj;
    //Aparição de Objetos Errados
    public double ApaOer;
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
        Grafico.DesenhaGrafico(ds, "Explorador", nIni, nFim);
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
        Veloci = IncrementoEX.Veloci[0] + (IncrementoEX.Veloci[10] * ciclo) + IncrementoEX.Veloci[cnivel];
        QtdObs = IncrementoEX.QtdObs[0] + (IncrementoEX.QtdObs[10] * ciclo) + IncrementoEX.QtdObs[cnivel];
        ObsPul = IncrementoEX.ObsPul[0] + (IncrementoEX.ObsPul[10] * ciclo) + IncrementoEX.ObsPul[cnivel];
        ObsAba = IncrementoEX.ObsAba[0] + (IncrementoEX.ObsAba[10] * ciclo) + IncrementoEX.ObsAba[cnivel];
        ItsNca = IncrementoEX.ItsNca[0] + (IncrementoEX.ItsNca[10] * ciclo) + IncrementoEX.ItsNca[cnivel];
        QtdObj = IncrementoEX.QtdObj[0] + (IncrementoEX.QtdObj[10] * ciclo) + IncrementoEX.QtdObj[cnivel];
        ApaOer = IncrementoEX.ApaOer[0] + (IncrementoEX.ApaOer[10] * ciclo) + IncrementoEX.ApaOer[cnivel];

        Dificuldade = (Veloci * IncrementoEX.Veloci[11]) +
                      (QtdObs * IncrementoEX.QtdObs[11]) +
                      (ObsPul * IncrementoEX.ObsPul[11]) +
                      (ObsAba * IncrementoEX.ObsAba[11]) +
                      (ItsNca * IncrementoEX.ItsNca[11]) +
                      (QtdObj * IncrementoEX.QtdObj[11]) +
                      (ApaOer * IncrementoEX.ApaOer[11]);
        Console.WriteLine("{0,13}{1,13}{2,13}{3,13}{4,13}{5,13}{6,13}{7,13}{8,13}", nivel, Veloci, QtdObs, ObsPul, ObsAba, ItsNca, QtdObj, ApaOer, Dificuldade);

        using (StreamWriter arquivo = new StreamWriter("Explorador.csv", true))
        {
            if (nivel == 1) arquivo.WriteLine("nivel;Veloci;QtdObs;ObsPul;ObsAba;ItsNca;QtdObj;ApaOer;Dificuldade");
            arquivo.WriteLine("{0,13};{1,13};{2,13};{3,13};{4,13};{5,13};{6,13};{7,13};{8,13}", nivel, Veloci, QtdObs, ObsPul, ObsAba, ItsNca, QtdObj, ApaOer, Dificuldade);
        }
    }
}