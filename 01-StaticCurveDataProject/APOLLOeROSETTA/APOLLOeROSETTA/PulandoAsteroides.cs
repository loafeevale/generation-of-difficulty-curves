using APOLLOeROSETTA;
using System;
using System.Data;
using System.IO;

public static class IncrementoPA
{
    //
    //                                           Valor Base                                                       Incremento por ciclo
    //                                                |                                                                     |
    //                                                |     |-------------------Incremento por nível------------------|     |     Peso  
    //                                                |     |1      2      3      4      5      6      7      8      9|     |      |    
    //Velocidade de Troca dos Alvos                   |     |                                                         |     |      | 
    public static double[] Veloci = new double[12] {  1,     0,     0,     0,   0.1,   0.1,   0.2,     0,     0,     0,   0.2,     4 };
    //Jogadas Totais                                                                                                              
    public static double[] JogTot = new double[12] { 10,     0,     5,     0,     3,     3,     5,    10,    10,    10,     5,     1 };
    //Jogadas Invertidas                                                                                                          
    public static double[] JogInv = new double[12] {  0,     0,     0,     3,     4,     5,     6,     4,     3,     4,     6,     2 };
    //Cores na Lista                                                                                                              
    public static double[] CorLis = new double[12] {  0,     0,     0,     1,     1,     2,     2,     1,     1,     1,     2,     3 };
    
}

public class PulandoAsteroides
{
    //Velocidade de Troca dos Alvos
    public double Veloci;
    //Jogadas Totais
    public double JogTot;
    //Jogadas Invertidas
    public double JogInv;
    //Cores na Lista
    public double CorLis;
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
        Grafico.DesenhaGrafico(ds, "PulandoAsteroides",nIni,nFim);
    }

    
    public void ConfiguracaoAtual(int nivel)
    {
        int ciclo = (nivel / 9);
        //nvls múltiplos de 9 fazem parte do ciclo anteior...
        if ((nivel % 9) == 0) {
            ciclo--;
        }
        int cnivel = nivel - (ciclo * 9);
        Veloci = IncrementoPA.Veloci[0] + (IncrementoPA.Veloci[10] * ciclo) + IncrementoPA.Veloci[cnivel];
        JogTot = IncrementoPA.JogTot[0] + (IncrementoPA.JogTot[10] * ciclo) + IncrementoPA.JogTot[cnivel];
        JogInv = IncrementoPA.JogInv[0] + (IncrementoPA.JogInv[10] * ciclo) + IncrementoPA.JogInv[cnivel];
        CorLis = IncrementoPA.CorLis[0] + (IncrementoPA.CorLis[10] * ciclo) + IncrementoPA.CorLis[cnivel];
        
        Dificuldade = (Veloci * IncrementoPA.Veloci[11]) +
                      (JogTot * IncrementoPA.JogTot[11]) +
                      (JogInv * IncrementoPA.JogInv[11]) +
                      (CorLis * IncrementoPA.CorLis[11]);

        Console.WriteLine("{0,13}{1,13}{2,13}{3,13}{4,13}{5,13}", nivel, Veloci, JogTot, JogInv, CorLis, Dificuldade);

        using (StreamWriter arquivo = new StreamWriter("PulandoAsteroides.csv", true))
        {
            if (nivel == 1) arquivo.WriteLine("nivel;Veloci;JogTot;JogInv;CorLis;Dificuldade");
            arquivo.WriteLine("{0,13};{1,13};{2,13};{3,13};{4,13};{5,13}", nivel, Veloci, JogTot, JogInv, CorLis, Dificuldade);
        }
    }
}