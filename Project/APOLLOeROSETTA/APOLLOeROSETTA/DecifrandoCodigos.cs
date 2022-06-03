using APOLLOeROSETTA;
using System;
using System.Data;
using System.IO;

public static class IncrementoDC
{

    //                                          Valor Base                                                       Incremento por ciclo
    //                                               |                                                                     |
    //                                               |     |-------------------Incremento por nível------------------|     |     Peso            
    //                                               |     |1      2      3      4      5      6      7      8      9|     |      |              
    //Velocidade de Troca dos Alvos                  |     |                                                         |     |      |
    public static double[] Veloci = new double[12] { 1,     0,     0,     0,   0.1,   0.1,   0.2,     0,     0,     0,   0.2,     2 };
    //Número de jogadas mãos                                                                                                   
    public static double[] JogMao = new double[12] { 6,     0,     0,     1,     2,     3,     4,     3,     3,     3,     5,     1 };
    //Número de jogadas pés                                                                                                    
    public static double[] JogPes = new double[12] { 2,     0,     1,     2,     3,     4,     5,     4,     4,     4,     4,     1 };
    //Combinação (mãos + pés)                                                                                                  
    public static double[] Combin = new double[12] { 2,     0,     1,     2,     3,     4,     5,     4,     4,     4,     4,     2 };
    //Núm palavras corretas 3 Síl                                                                                              
    public static double[] NmPlc3 = new double[12] { 0,     0,     1,     1,     0,     1,     2,     2,     2,     2,     1,     1 };
    //Núm palavras corretas 4 Síl                                                                                              
    public static double[] NmPlc4 = new double[12] { 0,     0,     0,     1,     1,     1,     1,     2,     2,     2,     1,     1 };
    //Núm. palavras incorretas 3 sil                                                                                           
    public static double[] NmPli3 = new double[12] { 0,     0,     1,     1,     1,     1,     1,     1,     1,     0,     1,     1 };
    //Núm. palavras incorretas 4 sil                                                                                           
    public static double[] NmPli4 = new double[12] { 0,     0,     0,     0,     1,     1,     1,     1,     0,     0,     1,     1 };
}

public class DecifrandoCodigos
{
    public double Veloci;
    //Número de jogadas mãos
    public double JogMao;
    //Número de jogadas pés
    public double JogPes;
    //Combinação (mãos + pés)
    public double Combin;
    //Núm palavras corretas 3 Síl
    public double NmPlc3;
    //Núm palavras corretas 4 Síl
    public double NmPlc4;
    //Núm. palavras incorretas 3 sil
    public double NmPli3;
    //Núm. palavras incorretas 4 sil
    public double NmPli4;
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
        Grafico.DesenhaGrafico(ds, "DecifrandoCodigos",nIni,nFim);
    }

    
    public void ConfiguracaoAtual(int nivel)
    {
        int ciclo = (nivel / 9);
        //nvls múltiplos de 9 fazem parte do ciclo anteior...
        if ((nivel % 9) == 0) {
            ciclo--;
        }
        
        int cnivel = nivel - (ciclo * 9);
        Veloci = IncrementoDC.Veloci[0] + (IncrementoDC.Veloci[10] * ciclo) + IncrementoDC.Veloci[cnivel];
        JogMao = IncrementoDC.JogMao[0] + (IncrementoDC.JogMao[10] * ciclo) + IncrementoDC.JogMao[cnivel];
        JogPes = IncrementoDC.JogPes[0] + (IncrementoDC.JogPes[10] * ciclo) + IncrementoDC.JogPes[cnivel];
        Combin = IncrementoDC.Combin[0] + (IncrementoDC.Combin[10] * ciclo) + IncrementoDC.Combin[cnivel];
        NmPlc3 = IncrementoDC.NmPlc3[0] + (IncrementoDC.NmPlc3[10] * ciclo) + IncrementoDC.NmPlc3[cnivel];
        NmPlc4 = IncrementoDC.NmPlc4[0] + (IncrementoDC.NmPlc4[10] * ciclo) + IncrementoDC.NmPlc4[cnivel];
        NmPli3 = IncrementoDC.NmPli3[0] + (IncrementoDC.NmPli3[10] * ciclo) + IncrementoDC.NmPli3[cnivel];
        NmPli4 = IncrementoDC.NmPli4[0] + (IncrementoDC.NmPli4[10] * ciclo) + IncrementoDC.NmPli4[cnivel];

        Dificuldade = (Veloci * IncrementoDC.Veloci[11]) +
                      (JogMao * IncrementoDC.JogMao[11]) +
                      (JogPes * IncrementoDC.JogPes[11]) +
                      (Combin * IncrementoDC.Combin[11]) +
                      (NmPlc3 * IncrementoDC.NmPlc3[11]) +
                      (NmPlc4 * IncrementoDC.NmPlc4[11]) +
                      (NmPli3 * IncrementoDC.NmPli3[11]) +
                      (NmPli4 * IncrementoDC.NmPli4[11]);

        Console.WriteLine("{0,13}{1,13}{2,13}{3,13}{4,13}{5,13}{6,13}{7,13}{8,13}{9,13}", nivel, Veloci, JogMao, JogPes, Combin, NmPlc3, NmPlc4, NmPli3, NmPli4, Dificuldade);
        using (StreamWriter arquivo = new StreamWriter("DecifrandoCodigos.csv", true))
        {
            if (nivel == 1) arquivo.WriteLine("nivel;Veloci;JogMao;JogPes;Combin;NmPlc3;NmPlc4;NmPli3;NmPli4;Dificuldade");
            arquivo.WriteLine("{0,13};{1,13};{2,13};{3,13};{4,13};{5,13};{6,13};{7,13};{8,13};{9,13}", nivel, Veloci, JogMao, JogPes, Combin, NmPlc3, NmPlc4, NmPli3, NmPli4, Dificuldade);
        }
    }
}