# Model for Automatic Generation of Difficulty Curves in Digital Games

## Executável

> Executável disponível em: _/ExecutableAndResults/**APOLLOeROSETTA.exe**_

Ao rodar o executável mencionado acima, ele gera os arquivos que estão na pasta:

![](/_Images/Image_01.png)

Os arquivos CSV contém dados de dificuldade de cada nivel, do 1 ao 63 – mas o algortimo consegue rodas a lógica até “N”.

Abaixo um exemplo da interação do algoritmo para o "Desafio dos Opostos Cósmicos".

![](/_Images/Image_02.png)

Para todos os minijogos a estrutura dos algoritmos é a mesma. Ele recebe uma tabela com valores necessários para gerar a curva desenhada pelo projeto do jogo.

Tomando como base o _mini game_ "Desafio dos Opostos Cósmicos", temos a seguinte tabela base:

![](/_Images/Image_03.png)

- Valor Base: número inicial que a característica irá ter, no nível 1;
- Incremento por nível: A cada nível, o valor da característica será incrementado por este valor. O nível é sempre um múltiplo de 9. 
- Incremento por ciclo: A cada 9 níveis, o algoritmo irá incrementar o valor da característica com o valor desta coluna.
- Peso: Peso que a característica tem, na composição final da dificuldade. A soma é 10.

![](/_Images/Image_04.png)

Exemplo de processamento na característica Velocidade:

- Ciclo 1
  - Nível 1: (Valor Base) + 0 = 1
  - Nível 2: (Valor Base) + 0 = 1
  - Nível 3: (Valor Base) + 0.1 = 1.1
  - Nível 4: (Valor Base) + 0.1 = 1.1
  - Nível 5: (Valor Base) + 0.2 = 1.2
  - Nível 6: (Valor Base) + 0.2 = 1.2
  - Nível 7: (Valor Base) + 0 = 1
  - Nível 8: (Valor Base) + 0 = 1
  - Nível 9: (Valor Base) + 0 = 1

- Ciclo 2
  - Nível 10: (Valor Base) + (Incremento por Ciclo) + 0 = 1.2
  - Nível 11: (Valor Base) + (Incremento por Ciclo) + 0 = 1.2
  - Nível 12: (Valor Base) + (Incremento por Ciclo) + 0.1 = 1.3
  - Nível 13: (Valor Base) + (Incremento por Ciclo) + 0.1 = 1.3
  - Nível 14: (Valor Base) + (Incremento por Ciclo) + 0.2 = 1.4
  - Nível 15: (Valor Base) + (Incremento por Ciclo) + 0.2 = 1.4
  - Nível 16: (Valor Base) + (Incremento por Ciclo) + 0 = 1.2
  - Nível 17: (Valor Base) + (Incremento por Ciclo) + 0 = 1.2
  - Nível 18: (Valor Base) + (Incremento por Ciclo) + 0 = 1.2

E assim sucessivamente até o número máximo indicado de níveis, na chamada da função _ConfiguracaoIntervalo_.

Da mesma forma acontece nas outras colunas.

O valor da linha do gráfico é o resultado da aplicação do valor de cada característica ao seu respectivo peso.

Resultado do processamento do algoritmo para os parâmetros do jogo "Desafio dos Opostos Cósmicos".

![](/_Images/Image_05.png)

![](/_Images/Image_06.png)
