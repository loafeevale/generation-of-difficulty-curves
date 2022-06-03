using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace APOLLOeROSETTA
{
    class Program
    {
        static void Main(string[] args)
        {
            
            ArteGalatica artegalatica = new ArteGalatica();
            DecifrandoCodigos decifrandocodigos = new DecifrandoCodigos();
            DesafioDosOpostos desafiodosopostos = new DesafioDosOpostos();
            Explorador explorador = new Explorador();
            LaboratorioEstelar laboratorioestelar = new LaboratorioEstelar();
            PulandoAsteroides pulandoasteroides = new PulandoAsteroides();
            TunelAcelerador tunelacelerador = new TunelAcelerador();
            
            
            desafiodosopostos.ConfiguracaoIntervalo(1, 63);
            artegalatica.ConfiguracaoIntervalo(1, 63);
            decifrandocodigos.ConfiguracaoIntervalo(1, 63);
            explorador.ConfiguracaoIntervalo(1, 63);
            laboratorioestelar.ConfiguracaoIntervalo(1, 63);
            pulandoasteroides.ConfiguracaoIntervalo(1, 63);
            tunelacelerador.ConfiguracaoIntervalo(1, 63);

            Console.ReadKey();            
        }
    }
}
