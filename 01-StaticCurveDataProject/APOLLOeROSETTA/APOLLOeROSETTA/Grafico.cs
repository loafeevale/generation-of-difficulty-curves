using System.Data;
using System.Drawing;
using System.Windows.Forms.DataVisualization.Charting;

namespace APOLLOeROSETTA
{
    public static class Grafico
    {
        public static void DesenhaGrafico(DataSet ds, string jogo, int nIni, int nFim)
        {
            int qtdNiv = nFim - nIni;
            //prepare chart control...
            Chart chart = new Chart();
            chart.DataSource = ds.Tables[0];
            chart.Width = qtdNiv * 20;
            chart.Height = qtdNiv * 15;
            //create serie...
            Series serie1 = new Series();
            serie1.Name = "Curva";

            serie1.ChartType = SeriesChartType.Line;
            serie1.BorderDashStyle = ChartDashStyle.Solid;
            serie1.IsValueShownAsLabel = true;
            serie1.XValueMember = "Nível";
            serie1.YValueMembers = "Dificuldade";
            serie1.MarkerStyle = MarkerStyle.Circle;
            serie1.Color = Color.DarkBlue;

            chart.Series.Add(serie1);


            //create chartareas...
            ChartArea ca = new ChartArea();
            ca.Name = "ChartArea1";
            ca.BackColor = Color.White;
            ca.BorderColor = Color.FromArgb(0, 0, 0);
            ca.BorderWidth = 0;

            ca.AxisX = new Axis();
            ca.AxisY = new Axis();
            ca.AxisX.Minimum = 1;
            ca.AxisX.Interval = 1;
            ca.AxisY.Interval = 1;
            ca.AxisX.MajorGrid.LineDashStyle = ChartDashStyle.Dash;
            ca.AxisY.MajorGrid.LineDashStyle = ChartDashStyle.Dash;
            ca.AxisY.MajorGrid.LineColor = Color.LightGray;
            ca.AxisX.MajorGrid.LineColor = Color.LightGray;
            chart.ChartAreas.Add(ca);




            //databind...
            chart.DataBind();
            //save result...

            int a = 0;
            foreach (var ponto in ds.Tables[0].Rows)
            {
                a = int.Parse(((DataRow)ponto).ItemArray[0].ToString());
                if ((a + 1) % 9 == 0)
                {
                    try
                    {
                        serie1.Points[a - 2 - nIni].MarkerColor = Color.Red;
                        serie1.Points[a - 2 - nIni].MarkerSize = 10;
                        serie1.Points[a - 2 - nIni].MarkerBorderColor = Color.DarkBlue;

                        serie1.Points[a - 1 - nIni].MarkerColor = Color.DarkBlue;
                        serie1.Points[a - 1 - nIni].Color = Color.DarkGreen;
                        serie1.Points[a - 1 - nIni].MarkerBorderColor = Color.Red;
                        serie1.Points[a - 1 - nIni].BorderWidth = 5;
                        serie1.Points[a - 1 - nIni].MarkerBorderWidth = 2;

                        serie1.Points[a - nIni].MarkerColor = Color.DarkBlue;
                        serie1.Points[a - nIni].Color = Color.DarkGreen;
                        serie1.Points[a - nIni].MarkerBorderColor = Color.Red;
                        serie1.Points[a - nIni].BorderWidth = 5;
                        serie1.Points[a - nIni].MarkerBorderWidth = 2;


                        serie1.Points[a + 1 - nIni].MarkerColor = Color.DarkBlue;
                        serie1.Points[a + 1 - nIni].Color = Color.DarkGreen;
                        serie1.Points[a + 1 - nIni].MarkerBorderColor = Color.Red;
                        serie1.Points[a + 1 - nIni].BorderWidth = 5;
                        serie1.Points[a + 1 - nIni].MarkerBorderWidth = 2;

                        serie1.Points[a + 2 - nIni].MarkerColor = Color.DarkBlue;
                        serie1.Points[a + 2 - nIni].MarkerBorderColor = Color.DarkGreen;
                        serie1.Points[a + 2 - nIni].MarkerSize = 10;
                        serie1.Points[a + 2 - nIni].MarkerBorderWidth = 3;
                    }
                    catch { }

                }
            }

            chart.SaveImage(@"Dificuldade_" + jogo + "_" + nIni + "a" + nFim + ".png", ChartImageFormat.Png);
        }

    }
}
