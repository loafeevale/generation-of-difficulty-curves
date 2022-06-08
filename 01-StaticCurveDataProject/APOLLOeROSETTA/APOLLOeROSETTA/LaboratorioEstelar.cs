using APOLLOeROSETTA;
using System;
using System.Data;
using System.IO;

public static class IncrementoLE
{
    //
    //                                          Valor Base                                                       Incremento por ciclo
    //                                               |                                                                     |
    //                                               |     |-------------------Incremento por nível------------------|     |     Peso  
    //                                               |     |1      2      3      4      5      6      7      8      9|     |      |    
    //Vel.dos objetos                                |     |                                                         |     |      | 
    public static double[] Veloci = new double[12] { 1,     0,     0,     0,   0.1,   0.1,   0.2,     0,     0,     0,   0.2,     2 };
    //Nº de jog.mãos                                                                                                              
    public static double[] JogMao = new double[12] { 3,     0,     1,     1,     2,     1,     2,     2,     2,     1,     1,   1.5 };
    //Nº de jog.pés                                                                                                             
    public static double[] JogPes = new double[12] { 4,     0,    -1,     0,     0,     1,     1,     2,     1,     1,     1,   1.5 };
    //Combinação (mãos + pés)                                                                                                     
    public static double[] Combin = new double[12] { 0,     0,     1,     1,     1,     2,     2,     1,     0,     1,     1,     2 };
    //Cores Erradas                                                                                                               
    public static double[] CorErr = new double[12] { 0,     0,     0,     0,     0,     0,     1,     0,     1,     0,     1,   1.5 };
    //Números Errados                                                                                                           
    public static double[] NumErr = new double[12] { 0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     1,   1.5 };

}

public class LaboratorioEstelar
{
    //Vel.dos objetos
    public double Veloci;
    //Nº de jog.mãos 
    public double JogMao;
    //Nº de jog.pés  
    public double JogPes;
    //Combinação (mãos + pés) 
    public double Combin;
    //Cores Erradas   
    public double CorErr;
    //Números Errados
    public double NumErr;
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
        Grafico.DesenhaGrafico(ds, "LaboratorioEstelar", nIni, nFim);
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
        Veloci = IncrementoLE.Veloci[0] + (IncrementoLE.Veloci[10] * ciclo) + IncrementoLE.Veloci[cnivel];
        JogMao = IncrementoLE.JogMao[0] + (IncrementoLE.JogMao[10] * ciclo) + IncrementoLE.JogMao[cnivel];
        JogPes = IncrementoLE.JogPes[0] + (IncrementoLE.JogPes[10] * ciclo) + IncrementoLE.JogPes[cnivel];
        Combin = IncrementoLE.Combin[0] + (IncrementoLE.Combin[10] * ciclo) + IncrementoLE.Combin[cnivel];
        CorErr = IncrementoLE.CorErr[0] + (IncrementoLE.CorErr[10] * ciclo) + IncrementoLE.CorErr[cnivel];
        NumErr = IncrementoLE.NumErr[0] + (IncrementoLE.NumErr[10] * ciclo) + IncrementoLE.NumErr[cnivel];

        Dificuldade = (Veloci * IncrementoLE.Veloci[11]) +
                      (JogMao * IncrementoLE.JogMao[11]) +
                      (JogPes * IncrementoLE.JogPes[11]) +
                      (Combin * IncrementoLE.Combin[11]) +
                      (CorErr * IncrementoLE.CorErr[11]) +
                      (NumErr * IncrementoLE.NumErr[11]);
                      
        Console.WriteLine("{0,13}{1,13}{2,13}{3,13}{4,13}{5,13}{6,13}{7,13}", nivel, Veloci, JogMao, JogPes, Combin, CorErr, NumErr, Dificuldade);
        
        using (StreamWriter arquivo = new StreamWriter("LaboratorioEstelar.csv", true))
        {
            if (nivel == 1) arquivo.WriteLine("nivel;Veloci;JogMao;JogPes;Combin;CorErr;NumErr;Dificuldade");
            arquivo.WriteLine("{0,13};{1,13};{2,13};{3,13};{4,13};{5,13};{6,13};{7,13}", nivel, Veloci, JogMao, JogPes, Combin, CorErr, NumErr, Dificuldade);
        }
    }
}