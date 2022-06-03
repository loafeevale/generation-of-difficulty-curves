using APOLLOeROSETTA;
using System;
using System.Data;
using System.IO;

public static class IncrementoDO
{
    //                                          Valor Base                                                       Incremento por ciclo
    //                                               |                                                                     |
    //                                               |     |-------------------Incremento por nível------------------|     |     Peso                                                           
    //                                               |     |1      2      3      4      5      6      7      8      9|     |      |              
    //Velocidade                                     |     |                                                         |     |      | 
    public static double[] Veloci = new double[12] { 1,     0,     0,   0.1,   0.1,   0.2,   0.2,     0,     0,     0,   0.2,     3 };
    //Alternância Mestres                                                                                                      
    public static double[] AltMes = new double[12] { 0,     0,     1,     1,     2,     2,     3,     2,     2,     2,     1,     2 };
    //Característica Fisica                                                                                                    
    public static double[] CtrFis = new double[12] { 2,     0,    -1,    -1,    -1,     0,    -1,     0,    -2,    -1,     1,     1 };
    //Nome do Objeto                                                                                                           
    public static double[] NmeObj = new double[12] { 1,     0,     0,     1,     1,     1,     1,     1,     1,     1,     1,   0.5 };
    //Posição                                                                                                                  
    public static double[] Psicao = new double[12] { 2,     0,     0,    -1,     0,    -1,     0,     0,     0,     0,     1,   0.5 };
    //Lateralidade                                                                                                             
    public static double[] Latera = new double[12] { 1,     0,     0,     1,     0,     1,     1,     1,     1,     1,     1,     1 };
    //Partes do Corpo                                                                                                          
    public static double[] PtsCrp = new double[12] { 1,     0,     0,     0,     1,     1,     1,     1,     1,     1,     1,   0.5 };
    //Tempo de Resposta                                                                                                        
    public static double[] TmpRsp = new double[12] { 1,     0,     0,     0,     0,     0,   0.1,     0,     0,     0,   0.1,   1.5 };
}

public class DesafioDosOpostos
{
    //Velocidade
    public double Veloci;
    //Alternância Mestres
    public double AltMes;
    //Característica Fisica
    public double CtrFis;
    //Nome do Objeto
    public double NmeObj;
    //Posição
    public double Psicao;
    //Lateralidade
    public double Latera;
    //Partes do Corpo
    public double PtsCrp;
    //Tempo de Resposta
    public double TmpRsp;
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
        Grafico.DesenhaGrafico(ds, "DesafioOpostos",nIni,nFim);
    }

    
    public void ConfiguracaoAtual(int nivel)
    {
        int ciclo = (nivel / 9);
        //nvls múltiplos de 9 fazem parte do ciclo anteior...
        if ((nivel % 9) == 0) {
            ciclo--;
        }
        int cnivel = nivel - (ciclo * 9);
        Veloci = IncrementoDO.Veloci[0] + (IncrementoDO.Veloci[10] * ciclo) + IncrementoDO.Veloci[cnivel];
        AltMes = IncrementoDO.AltMes[0] + (IncrementoDO.AltMes[10] * ciclo) + IncrementoDO.AltMes[cnivel];
        CtrFis = IncrementoDO.CtrFis[0] + (IncrementoDO.CtrFis[10] * ciclo) + IncrementoDO.CtrFis[cnivel];
        NmeObj = IncrementoDO.NmeObj[0] + (IncrementoDO.NmeObj[10] * ciclo) + IncrementoDO.NmeObj[cnivel];
        Psicao = IncrementoDO.Psicao[0] + (IncrementoDO.Psicao[10] * ciclo) + IncrementoDO.Psicao[cnivel];
        Latera = IncrementoDO.Latera[0] + (IncrementoDO.Latera[10] * ciclo) + IncrementoDO.Latera[cnivel];
        PtsCrp = IncrementoDO.PtsCrp[0] + (IncrementoDO.PtsCrp[10] * ciclo) + IncrementoDO.PtsCrp[cnivel];
        TmpRsp = IncrementoDO.TmpRsp[0] + (IncrementoDO.TmpRsp[10] * ciclo) + IncrementoDO.TmpRsp[cnivel];

        Dificuldade = (Veloci * IncrementoDO.Veloci[11]) +
                      (AltMes * IncrementoDO.AltMes[11]) +
                      (CtrFis * IncrementoDO.CtrFis[11]) +
                      (NmeObj * IncrementoDO.NmeObj[11]) +
                      (Psicao * IncrementoDO.Psicao[11]) +
                      (Latera * IncrementoDO.Latera[11]) +
                      (PtsCrp * IncrementoDO.PtsCrp[11]) +
                      (TmpRsp * IncrementoDO.TmpRsp[11]);

        Console.WriteLine("{0,13}{1,13}{2,13}{3,13}{4,13}{5,13}{6,13}{7,13}{8,13}{9,13}", nivel, Veloci, AltMes, CtrFis, NmeObj, Psicao, Latera, PtsCrp, TmpRsp, Dificuldade);
        using (StreamWriter arquivo = new StreamWriter("DesafioOpostos.csv", true))
        {
            if (nivel == 1 ) arquivo.WriteLine("nivel;Veloci;AltMes;CtrFis;NmeObj;Psicao;Latera;PtsCrp;TmpRsp;Dificuldade");
            arquivo.WriteLine("{0,13};{1,13};{2,13};{3,13};{4,13};{5,13};{6,13};{7,13};{8,13};{9,13}", nivel, Veloci, AltMes, CtrFis, NmeObj, Psicao, Latera, PtsCrp, TmpRsp, Dificuldade);
        }
    }
}