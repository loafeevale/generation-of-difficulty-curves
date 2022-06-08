###### Cliente: TC553 - João Batista Mossmann
####### Início: 20/04/2018
####### Autor: Luiz Henrique,

rm(list=ls())

### =============================== 
### Instalando e Carregando pacotes
### =============================== 

source("Y:/Funções ABG.R") 

require(gamlss)
require(geepack)
require(contrast)
require(gtools)
require(Hmisc)
require(lme4)
require(RColorBrewer)
require(reshape2)
require(ggplot2)
require(RColorBrewer)
require(arm)
require(lmerTest)
require(MuMIn)

### =============================== 
### Puxando banco de dados
### =============================== 

dados0 <- read.csv2("Y:/TC553 - João Batista Mossmann/TM448 - João Batista Mossmann/Dados2.csv")
head(dados0); dim(dados0)

#### Banco de dados alunos - Desconsiderar a aluna Luana Claro Rosa e as equipes Num Sei e Os 4 astronautas

dados <- subset(dados0, dados0$Nome != "Luana Claro Rosa" & dados0$Nome != "Num Sei" & dados0$Nome != "Os 4 Astronautas")
head(dados); dim(dados)

###================
### Modelo completo
###================

Performance2 = log(ifelse(dados$Performance<=0.3,0.3, dados$Performance)*100)
dados$Performance2 = Performance2

DP<- dados$ValorDificuldade*dados$Performance2
dados$DP = DP

model1 = lmer(LevelID~DP+(DP|NomeGame), data=dados)
summary(model1)
ranef(model1)

AIC(model1)
cor.test(predict(model1, type="response"), LevelID2)$estimate^2

plot(predict(model1, type="response"), DP)
plot(predict(model1, type="response"), LevelID2)

###=================================
### Modelo só com primeira tentativa
###=================================

dados1 = subset(dados, dados$Tentativa==1)

Performance2 = log(ifelse(dados1$Performance<=0.3,0.3, dados1$Performance)*100)
dados1$Performance2 = Performance2

DP<- dados1$ValorDificuldade*dados1$Performance2
dados1$DP = DP

model2 = lmer(LevelID~DP+(DP|NomeGame), data=dados1)
summary(model2)
ranef(model2)

AIC(model2)
cor.test(predict(model2, type="response"), dados1$LevelID)$estimate^2

###===============================
### Modelo só sem a jogadora Luana 
###===============================

dadossL = subset(dados, dados$Nome!="Luana Claro Rosa")

Performance2 = log(ifelse(dadossL$Performance<=0.3,0.3, dadossL$Performance)*100)
dadossL$Performance2 = Performance2

DP<- dadossL$ValorDificuldade*dadossL$Performance2
dadossL$DP = DP

model2 = lmer(LevelID~DP+(DP|NomeGame), data=dadossL)
summary(model2)
ranef(model2)















