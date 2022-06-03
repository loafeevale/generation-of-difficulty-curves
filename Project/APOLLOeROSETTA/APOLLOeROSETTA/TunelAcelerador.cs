using APOLLOeROSETTA;
using System;
using System.Data;
using System.IO;

public static class IncrementoTA
{
    //
    //                                            Valor Base                                                       Incremento por ciclo
    //                                                 |                                                                     |
    //                                                 |     |-------------------Incremento por nível------------------|     |     Peso  
    //                                                 |     |1      2      3      4      5      6      7      8      9|     |      |    
    //Velocidade do Hoverboard                         |     |                                                         |     |      | 
    public static double[] Veloci = new double[12] { 1.1,    0,    0.1,   0.2,   0.2,   0.3,   0.3,   0.1,   0.1,   0.1,   0.2,     4 };
    //Quantidade de Obstáculos                                                                                                     
    public static double[] QtdObs = new double[12] {   5,    0,      2,     4,     4,     6,     6,     8,     7,     6,     6,     1 };
    //Distância da câmera invertida                                                                                                
    public static double[] DsCmIn = new double[12] { 0.9,    0,      0,   0.1,   0.1,   0.2,   0.2,   0.3,   0.1,   0.1,   0.2,     2 };
    //Quantidade de obstáculos invertidos                                                                                          
    public static double[] QtObIn = new double[12] {   1,    0,      0,     0,     1,     1,     2,     1,     1,     1,     2,     3 };

}

public class TunelAcelerador
{
    //Velocidade do Hoverboard
    public double Veloci;
    //Quantidade de Obstáculos
    public double QtdObs;
    //Distância da câmera invertida
    public double DsCmIn;
    //Quantidade de obstáculos invertidos
    public double QtObIn;
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
        Grafico.DesenhaGrafico(ds, "TunelAcelerador",nIni,nFim);
    }

    
    public void ConfiguracaoAtual(int nivel)
    {
        int ciclo = (nivel / 9);
        //nvls múltiplos de 9 fazem parte do ciclo anteior...
        if ((nivel % 9) == 0) {
            ciclo--;
        }
        
        
        
        



        int cnivel = nivel - (ciclo * 9);
        Veloci = IncrementoTA.Veloci[0] + (IncrementoTA.Veloci[10] * ciclo) + IncrementoTA.Veloci[cnivel];
        QtdObs = IncrementoTA.QtdObs[0] + (IncrementoTA.QtdObs[10] * ciclo) + IncrementoTA.QtdObs[cnivel];
        DsCmIn = IncrementoTA.DsCmIn[0] + (IncrementoTA.DsCmIn[10] * ciclo) + IncrementoTA.DsCmIn[cnivel];
        QtObIn = IncrementoTA.QtObIn[0] + (IncrementoTA.QtObIn[10] * ciclo) + IncrementoTA.QtObIn[cnivel];

        Dificuldade = (Veloci * IncrementoTA.Veloci[11]) +
                      (QtdObs * IncrementoTA.QtdObs[11]) +
                      (DsCmIn * IncrementoTA.DsCmIn[11]) +
                      (QtObIn * IncrementoTA.QtObIn[11]);


        Console.WriteLine("{0,13}{1,13}{2,13}{3,13}{4,13}{5,13}", nivel, Veloci, QtdObs, DsCmIn, QtObIn, Dificuldade);

        using (StreamWriter arquivo = new StreamWriter("TunelAcelerador.csv", true))
        {
            if (nivel == 1) arquivo.WriteLine("nivel;Veloci;QtdObs;DsCmIn;QtObIn;Dificuldade");
            arquivo.WriteLine("{0,13};{1,13};{2,13};{3,13};{4,13};{5,13}", nivel, Veloci, QtdObs, DsCmIn, QtObIn, Dificuldade);
        }
    }
}