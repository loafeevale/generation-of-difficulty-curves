###### Cliente: TC553 - João Batista Mossmann
####### Início: 04/12/2017
####### Autor: Luana

rm(list=ls())
options(OutDec=",")

### =============================== 
### Instalando e Carregando pacotes
### =============================== 

source("Y:/Estágio/Funções ABG.R") 

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

cor <- brewer.pal(n=8, "Dark2")

### =============================== 
### Puxando banco de dados
### =============================== 

dados0 <- read.csv2("Y:/00 - Consultoria/TC553 - João Batista Mossmann/TM448 - João Batista Mossmann/Dados2.csv")
head(dados0); dim(dados0)

#### Banco de dados alunos - Desconsiderar a aluna Luana Claro Rosa e as equipes Num Sei e Os 4 astronautas

dados <- subset(dados0, dados0$Nome != "Luana Claro Rosa" & dados0$Nome != "Num Sei" & dados0$Nome != "Os 4 Astronautas")
head(dados); dim(dados)

### =============================== 
### Recategorizando as variáveis
### =============================== 

dados$taxa<- (dados$QtdAcertos/dados$QtdDesafio)
dados$taxaInv<- 1/(dados$taxa+1)
dados$taxaLog<- log(dados$taxa+1)

dados$taxa2<- dados$taxa^2
dados$taxaInv2<- dados$taxaInv^2
dados$taxaLog2<- dados$taxaLog^2

dados$taxaMa<- ifelse(dados$taxa>=0.7, dados$taxa, 0)
dados$taxaMe<- ifelse(dados$taxa<0.7, dados$taxa, 0)

dados$taxaInvMa<- ifelse(dados$taxa>=0.7, dados$taxaInv, 0)
dados$taxaInvMe<- ifelse(dados$taxa<0.7, dados$taxaInv, 0)

dados$taxaLogMa<- ifelse(dados$taxa>=0.7, dados$taxaLog, 0)
dados$taxaLogMe<- ifelse(dados$taxa<0.7, dados$taxaLog, 0)

NomeGame1<- rep(unique(dados$NomeGame), rep(101,7))
Taxaaux<- (seq(0, 1, 0.01))
TaxaauxInv<- 1/(Taxaaux+1)
TaxaauxLog<- log(Taxaaux+1)

Taxaaux2<- Taxaaux^2
TaxaauxInv2<- TaxaauxInv^2
TaxaauxLog2<- TaxaauxLog^2

TaxaauxMa<- ifelse(Taxaaux>=0.7, Taxaaux, 0)
TaxaauxMe<- ifelse(Taxaaux<0.7, Taxaaux, 0)

TaxaauxInvMa<- ifelse(Taxaaux>=0.7, TaxaauxInv, 0)
TaxaauxInvMe<- ifelse(Taxaaux<0.7, TaxaauxInv, 0)

TaxaauxLogMa<- ifelse(Taxaaux>=0.7, TaxaauxLog, 0)
TaxaauxLogMe<- ifelse(Taxaaux<0.7, TaxaauxLog, 0)

Newdata<- data.frame(NomeGame1, Taxaaux, TaxaauxInv, TaxaauxLog,
Taxaaux2, TaxaauxInv2, TaxaauxLog2, TaxaauxMa, TaxaauxMe,
TaxaauxInvMa, TaxaauxInvMe, TaxaauxLogMa, TaxaauxLogMe)

colnames(Newdata)<- c("NomeGame", "taxa", "taxaInv", "taxaLog", 
"taxa2", "taxaInv2", "taxaLog2", "taxaMa", "taxaMe",
"taxaInvMa", "taxaInvMe", "taxaLogMa", "taxaLogMe")

setwd("Y:/00 - Consultoria/TC553 - João Batista Mossmann/Gráficos")

F1<- function(modelo, ylim, main){

Newdata$pred<- predict(modelo, newdata=Newdata, type="response")
Newdata1<- subset(Newdata, Newdata$NomeGame=="arteGalacticaData")
Newdata2<- subset(Newdata, Newdata$NomeGame=="decifrandoCodigosData")
Newdata3<- subset(Newdata, Newdata$NomeGame=="desafioDosOpostosData")
Newdata4<- subset(Newdata, Newdata$NomeGame=="exploradorData")
Newdata5<- subset(Newdata, Newdata$NomeGame=="laboratorioData")
Newdata6<- subset(Newdata, Newdata$NomeGame=="pulandoAsteroidesData")
Newdata7<- subset(Newdata, Newdata$NomeGame=="tunelAceleradorData")

plot(Newdata1$pred ~ Newdata1$taxa, main=main, type="l", ylim=ylim)
lines(Newdata2$pred ~ Newdata2$taxa, type="l")
lines(Newdata3$pred ~ Newdata3$taxa, type="l")
lines(Newdata4$pred ~ Newdata4$taxa, type="l")
lines(Newdata5$pred ~ Newdata5$taxa, type="l")
lines(Newdata6$pred ~ Newdata6$taxa, type="l")
lines(Newdata7$pred ~ Newdata7$taxa, type="l")
}

### ================================================================== ###
### ======================== Banco Completo ========================== ###
### ================================================================== ###

### ================================================================== ###
### ============================ Identity ============================ ###
### ================================================================== ###

### =========================
### Taxa normal 
### =========================

### Taxa normal  - Taxa ###

mIn0<- glmer(ValorDificuldade ~ taxa +
		  (1|NomeGame),family=Gamma(link="identity"), data=dados)

mIn1<- glmer(ValorDificuldade ~ taxa +
		  (taxa|NomeGame),family=Gamma(link="identity"), data=dados)

anova(mIn1, mIn0) #mIn1

summary(mIn1)
cor.test(predict(mIn1, type="response"), dados$ValorDificuldade)$estimate^2
AIC(mIn1)

jpeg(filename = "plot1.jpg", width = 480, height = 480)
F1(mIn1, main="mIn1", ylim=c(0,100))
dev.off()

### Taxa normal - quadrado ###

mIn2<- glmer(ValorDificuldade ~ taxa + taxa2 +
		  (1|NomeGame),family=Gamma(link="identity"), data=dados)

mIn3<- glmer(ValorDificuldade ~ taxa + taxa2 +
		  (taxa|NomeGame),family=Gamma(link="identity"), data=dados)

mIn4<- glmer(ValorDificuldade ~ taxa + taxa2 +
		  (taxa2|NomeGame),family=Gamma(link="identity"), data=dados)

anova(mIn3, mIn2) #mIn3
anova(mIn4, mIn2) #mIn4

mIn5<- glmer(ValorDificuldade ~ taxa + taxa2 +
		  (taxa + taxa2|NomeGame),family=Gamma(link="identity"), data=dados)

anova(mIn5, mIn3) #mIn5
anova(mIn5, mIn4) #mIn4

summary(mIn4)
cor.test(predict(mIn4, type="response"), dados$ValorDificuldade)$estimate^2
AIC(mIn4)
jpeg(filename = "plot2.jpg", width = 480, height = 480)
F1(mIn5, main="mIn3", ylim=c(0,100))
dev.off()

### Taxa normal - nó ###

mIn6<- glmer(ValorDificuldade ~ taxaMa + taxaMe +
		  (1|NomeGame),family=Gamma(link="identity"), data=dados)

mIn7<- glmer(ValorDificuldade ~ taxaMa + taxaMe +
		  (taxaMa|NomeGame),family=Gamma(link="identity"), data=dados)

mIn8<- glmer(ValorDificuldade ~ taxaMa + taxaMe +
		  (taxaMe|NomeGame),family=Gamma(link="identity"), data=dados)

anova(mIn7, mIn6) #mIn7
anova(mIn8, mIn6) #mIn8

mIn9<- glmer(ValorDificuldade ~ taxaMa + taxaMe +
		  (taxaMa + taxaMe|NomeGame),family=Gamma(link="identity"), data=dados)

anova(mIn9, mIn7) #mIn7
anova(mIn9, mIn8) #mIn9

summary(mIn7)
cor.test(predict(mIn7, type="response"), dados$ValorDificuldade)$estimate^2
AIC(mIn7)
jpeg(filename = "plot3.jpg", width = 480, height = 480)
F1(mIn7, main="mIn7", ylim=c(0,100))
dev.off()

### ===================
### Taxa inversa
### ===================

### Taxa inversa  - Taxa ###

mIi0<- glmer(ValorDificuldade ~ taxaInv +
		  (1|NomeGame),family=Gamma(link="identity"), data=dados)

mIi1<- glmer(ValorDificuldade ~ taxaInv +
		  (taxaInv|NomeGame),family=Gamma(link="identity"), data=dados)

anova(mIi1, mIi0) #mIi1

summary(mIi1)
cor.test(predict(mIi1, type="response"), dados$ValorDificuldade)$estimate^2
AIC(mIi1)
jpeg(filename = "plot4.jpg", width = 480, height = 480)
F1(mIi1, main="mIi1", ylim=c(0,100))
dev.off()

### Taxa inversa - quadrado ###

mIi2<- glmer(ValorDificuldade ~ taxaInv + taxaInv2 +
		  (1|NomeGame),family=Gamma(link="identity"), data=dados)

mIi3<- glmer(ValorDificuldade ~ taxaInv + taxaInv2 +
		  (taxaInv|NomeGame),family=Gamma(link="identity"), data=dados)

mIi4<- glmer(ValorDificuldade ~ taxaInv + taxaInv2 +
		  (taxaInv2|NomeGame),family=Gamma(link="identity"), data=dados)

anova(mIi3, mIi2) #mIi3
anova(mIi4, mIi2) #mIi4

mIi5<- glmer(ValorDificuldade ~ taxaInv + taxaInv2 +
		  (taxaInv + taxaInv2|NomeGame),family=Gamma(link="identity"), data=dados)

anova(mIi5, mIi3) #mIi5
anova(mIi5, mIi4) #mIi5

summary(mIi5)
cor.test(predict(mIi5, type="response"), dados$ValorDificuldade)$estimate^2
AIC(mIi5)
jpeg(filename = "plot5.jpg", width = 480, height = 480)
F1(mIi5, main="mIi5", ylim=c(0,100))
dev.off()

### Taxa inversa - nó ###

mIi6<- glmer(ValorDificuldade ~ taxaInvMa + taxaInvMe +
		  (1|NomeGame),family=Gamma(link="identity"), data=dados)

mIi7<- glmer(ValorDificuldade ~ taxaInvMa + taxaInvMe +
		  (taxaInvMa|NomeGame),family=Gamma(link="identity"), data=dados)

mIi8<- glmer(ValorDificuldade ~ taxaInvMa + taxaInvMe +
		  (taxaInvMe|NomeGame),family=Gamma(link="identity"), data=dados)

anova(mIi7, mIi6) #mIi7
anova(mIi8, mIi6) #mIi8

mIi9<- glmer(ValorDificuldade ~ taxaInvMa + taxaInvMe +
		  (taxaInvMa + taxaInvMe|NomeGame),family=Gamma(link="identity"), data=dados)

anova(mIi9, mIi7) #mIi7
anova(mIi9, mIi8) #mIi8

summary(mIi8)
cor.test(predict(mIi8, type="response"), dados$ValorDificuldade)$estimate^2
AIC(mIi8)
jpeg(filename = "plot6.jpg", width = 480, height = 480)
F1(mIi8, main="mIi8", ylim=c(0,100))
dev.off()

### ===================
### Taxa log
### ===================

### Taxa log  - Taxa ###

mIl0<- glmer(ValorDificuldade ~ taxaLog +
		  (1|NomeGame),family=Gamma(link="identity"), data=dados)

mIl1<- glmer(ValorDificuldade ~ taxaLog +
		  (taxaLog|NomeGame),family=Gamma(link="identity"), data=dados)

anova(mIl1, mIl0) #mIl1

summary(mIl1)
cor.test(predict(mIl1, type="response"), dados$ValorDificuldade)$estimate^2
AIC(mIl1)
jpeg(filename = "plot7.jpg", width = 480, height = 480)
F1(mIl1, main="mIl1", ylim=c(0,100))
dev.off()

### Taxa log - quadrado ###

mIl2<- glmer(ValorDificuldade ~ taxaLog + taxaLog2 +
		  (1|NomeGame),family=Gamma(link="identity"), data=dados)

mIl3<- glmer(ValorDificuldade ~ taxaLog + taxaLog2 +
		  (taxaLog|NomeGame),family=Gamma(link="identity"), data=dados)

mIl4<- glmer(ValorDificuldade ~ taxaLog + taxaLog2 +
		  (taxaLog2|NomeGame),family=Gamma(link="identity"), data=dados)

anova(mIl3, mIl2) #mIl3
anova(mIl4, mIl2) #mIl4

mIl5<- glmer(ValorDificuldade ~ taxaLog + taxaLog2 +
		  (taxaLog + taxaLog2|NomeGame),family=Gamma(link="identity"), data=dados)

anova(mIl5, mIl3) #mIl5
anova(mIl5, mIl4) #mIl4

summary(mIl4)
cor.test(predict(mIl4, type="response"), dados$ValorDificuldade)$estimate^2
AIC(mIl4)
jpeg(filename = "plot8.jpg", width = 480, height = 480)
F1(mIl4, main="mIl4", ylim=c(0,100))
dev.off()

basic(dados$ValorDificuldade)

### Taxa log - nó ###

mIl6<- glmer(ValorDificuldade ~ taxaLogMa + taxaLogMe +
		  (1|NomeGame),family=Gamma(link="identity"), data=dados)

mIl7<- glmer(ValorDificuldade ~ taxaLogMa + taxaLogMe +
		  (taxaLogMa|NomeGame),family=Gamma(link="identity"), data=dados)

mIl8<- glmer(ValorDificuldade ~ taxaLogMa + taxaLogMe +
		  (taxaLogMe|NomeGame),family=Gamma(link="identity"), data=dados)

anova(mIl7, mIl6) #mIl7
anova(mIl8, mIl6) #mIl8

mIl9<- glmer(ValorDificuldade ~ taxaLogMa + taxaLogMe +
		  (taxaLogMa + taxaLogMe|NomeGame),family=Gamma(link="identity"), data=dados)

anova(mIl9, mIl7) #mIl7
anova(mIl9, mIl8) #mIl8

summary(mIl7)
cor.test(predict(mIl7, type="response"), dados$ValorDificuldade)$estimate^2
AIC(mIl7)
jpeg(filename = "plot9.jpg", width = 480, height = 480)
F1(mIl7, main="mIl7", ylim=c(0,100))
dev.off()

### ================================================================== ###
### ============================== Log =============================== ###
### ================================================================== ###

### =========================
### Taxa normal 
### =========================

### Taxa normal  - Taxa ###

mLn0<- glmer(ValorDificuldade ~ taxa +
		  (1|NomeGame),family=Gamma(link="log"), data=dados)

mLn1<- glmer(ValorDificuldade ~ taxa +
		  (taxa|NomeGame),family=Gamma(link="log"), data=dados)

anova(mLn1, mLn0) #mLn1

summary(mLn1)
cor.test(predict(mLn1, type="response"), dados$ValorDificuldade)$estimate^2
AIC(mLn1)
jpeg(filename = "plot10.jpg", width = 480, height = 480)
F1(mLn1, main="mLn1", ylim=c(0,100))
dev.off()

### Taxa normal - quadrado ###

mLn2<- glmer(ValorDificuldade ~ taxa + taxa2 +
		  (1|NomeGame),family=Gamma(link="log"), data=dados)

mLn3<- glmer(ValorDificuldade ~ taxa + taxa2 +
		  (taxa|NomeGame),family=Gamma(link="log"), data=dados)

mLn4<- glmer(ValorDificuldade ~ taxa + taxa2 +
		  (taxa2|NomeGame),family=Gamma(link="log"), data=dados)

anova(mLn3, mLn2) #mLn3
anova(mLn4, mLn2) #mLn4

mLn5<- glmer(ValorDificuldade ~ taxa + taxa2 +
		  (taxa + taxa2|NomeGame),family=Gamma(link="log"), data=dados)

anova(mLn5, mLn3) #mLn3
anova(mLn5, mLn4) #mLn4

AIC(mLn3, mLn4)

summary(mLn4)
cor.test(predict(mLn4, type="response"), dados$ValorDificuldade)$estimate^2
AIC(mLn4)
jpeg(filename = "plot11.jpg", width = 480, height = 480)
F1(mLn4, main="mLn4", ylim=c(0,100))
dev.off()

### Taxa normal - nó ###

mLn6<- glmer(ValorDificuldade ~ taxaMa + taxaMe +
		  (1|NomeGame),family=Gamma(link="log"), data=dados)

mLn7<- glmer(ValorDificuldade ~ taxaMa + taxaMe +
		  (taxaMa|NomeGame),family=Gamma(link="log"), data=dados)

mLn8<- glmer(ValorDificuldade ~ taxaMa + taxaMe +
		  (taxaMe|NomeGame),family=Gamma(link="log"), data=dados)

anova(mLn7, mLn6) #mLn7
anova(mLn8, mLn6) #mLn6

mLn9<- glmer(ValorDificuldade ~ taxaMa + taxaMe +
		  (taxaMa + taxaMe|NomeGame),family=Gamma(link="log"), data=dados)

anova(mLn9, mLn7) #mLn9
anova(mLn9, mLn8) #mLn9

summary(mLn9)
cor.test(predict(mLn9, type="response"), dados$ValorDificuldade)$estimate^2
AIC(mLn9)
jpeg(filename = "plot12.jpg", width = 480, height = 480)
F1(mLn9, main="mLn9", ylim=c(0,100))
dev.off()

### ===================
### Taxa inversa
### ===================

### Taxa inversa  - Taxa ###

mLi0<- glmer(ValorDificuldade ~ taxaInv +
		  (1|NomeGame),family=Gamma(link="log"), data=dados)

mLi1<- glmer(ValorDificuldade ~ taxaInv +
		  (taxaInv|NomeGame),family=Gamma(link="log"), data=dados)

anova(mLi1, mLi0) #mLi1

summary(mLi1)
cor.test(predict(mLi1, type="response"), dados$ValorDificuldade)$estimate^2
AIC(mLi1)
jpeg(filename = "plot13.jpg", width = 480, height = 480)
F1(mLi1, main="mLi1", ylim=c(0,100))
dev.off()

### Taxa inversa - quadrado ###

mLi2<- glmer(ValorDificuldade ~ taxaInv + taxaInv2 +
		  (1|NomeGame),family=Gamma(link="log"), data=dados)

mLi3<- glmer(ValorDificuldade ~ taxaInv + taxaInv2 +
		  (taxaInv|NomeGame),family=Gamma(link="log"), data=dados)

mLi4<- glmer(ValorDificuldade ~ taxaInv + taxaInv2 +
		  (taxaInv2|NomeGame),family=Gamma(link="log"), data=dados)

anova(mLi3, mLi2) #mLi3
anova(mLi4, mLi2) #mLi4

mLi5<- glmer(ValorDificuldade ~ taxaInv + taxaInv2 +
		  (taxaInv + taxaInv2|NomeGame),family=Gamma(link="log"), data=dados)

anova(mLi5, mLi3) #mLi3
anova(mLi5, mLi4) #mLi4

summary(mLi3)
cor.test(predict(mLi3, type="response"), dados$ValorDificuldade)$estimate^2
AIC(mLi3)
jpeg(filename = "plot14.jpg", width = 480, height = 480)
F1(mLi3, main="mLi3", ylim=c(0,100))
dev.off()

### Taxa inversa - nó ###

mLi6<- glmer(ValorDificuldade ~ taxaInvMa + taxaInvMe +
		  (1|NomeGame),family=Gamma(link="log"), data=dados)

mLi7<- glmer(ValorDificuldade ~ taxaInvMa + taxaInvMe +
		  (taxaInvMa|NomeGame),family=Gamma(link="log"), data=dados)

mLi8<- glmer(ValorDificuldade ~ taxaInvMa + taxaInvMe +
		  (taxaInvMe|NomeGame),family=Gamma(link="log"), data=dados)

anova(mLi7, mLi6) #mLi7
anova(mLi8, mLi6) #mLi8

mLi9<- glmer(ValorDificuldade ~ taxaInvMa + taxaInvMe +
		  (taxaInvMa + taxaInvMe|NomeGame),family=Gamma(link="log"), data=dados)

anova(mLi9, mLi7) #mLi7
anova(mLi9, mLi8) #mLi8

AIC(mLi7, mLi8)

summary(mLi8)
cor.test(predict(mLi8, type="response"), dados$ValorDificuldade)$estimate^2
AIC(mLi8)
jpeg(filename = "plot15.jpg", width = 480, height = 480)
F1(mLi8, main="mLi8", ylim=c(0,100))
dev.off()

### ===================
### Taxa log
### ===================

### Taxa log  - Taxa ###

mLl0<- glmer(ValorDificuldade ~ taxaLog +
		  (1|NomeGame),family=Gamma(link="log"), data=dados)

mLl1<- glmer(ValorDificuldade ~ taxaLog +
		  (taxaLog|NomeGame),family=Gamma(link="log"), data=dados)

anova(mLl1, mLl0) #mLl1

summary(mLl1)
cor.test(predict(mLl1, type="response"), dados$ValorDificuldade)$estimate^2
AIC(mLl1)
jpeg(filename = "plot16.jpg", width = 480, height = 480)
F1(mLl1, main="mLl1", ylim=c(0,100))
dev.off()

### Taxa log - quadrado ###

mLl2<- glmer(ValorDificuldade ~ taxaLog + taxaLog2 +
		  (1|NomeGame),family=Gamma(link="log"), data=dados)

mLl3<- glmer(ValorDificuldade ~ taxaLog + taxaLog2 +
		  (taxaLog|NomeGame),family=Gamma(link="log"), data=dados)

mLl4<- glmer(ValorDificuldade ~ taxaLog + taxaLog2 +
		  (taxaLog2|NomeGame),family=Gamma(link="log"), data=dados)

anova(mLl3, mLl2) #mLl3
anova(mLl4, mLl2) #mLl4

mLl5<- glmer(ValorDificuldade ~ taxaLog + taxaLog2 +
		  (taxaLog + taxaLog2|NomeGame),family=Gamma(link="log"), data=dados)

anova(mLl5, mLl3) #mLl3
anova(mLl5, mLl4) #mLl4

AIC(mLl3, mLl4)

summary(mLl4)
cor.test(predict(mLl4, type="response"), dados$ValorDificuldade)$estimate^2
AIC(mLl4)
jpeg(filename = "plot17.jpg", width = 480, height = 480)
F1(mLl4, main="mLl4", ylim=c(0,100))
dev.off()

### Taxa log - nó ###

mLl6<- glmer(ValorDificuldade ~ taxaLogMa + taxaLogMe +
		  (1|NomeGame),family=Gamma(link="log"), data=dados)

mLl7<- glmer(ValorDificuldade ~ taxaLogMa + taxaLogMe +
		  (taxaLogMa|NomeGame),family=Gamma(link="log"), data=dados)

mLl8<- glmer(ValorDificuldade ~ taxaLogMa + taxaLogMe +
		  (taxaLogMe|NomeGame),family=Gamma(link="log"), data=dados)

anova(mLl7, mLl6) #mLl7
anova(mLl8, mLl6) #mLl6

mLl9<- glmer(ValorDificuldade ~ taxaLogMa + taxaLogMe +
		  (taxaLogMa + taxaLogMe|NomeGame),family=Gamma(link="log"), data=dados)

anova(mLl9, mLl7) #mLl7
anova(mLl9, mLl8) #mLl8

summary(mLl7)
cor.test(predict(mLl7, type="response"), dados$ValorDificuldade)$estimate^2
AIC(mLl7)
jpeg(filename = "plot18.jpg", width = 480, height = 480)
F1(mLl7, main="mLl7", ylim=c(0,100))
dev.off()

### ================================================================== ###
### ============================= SQRT =============================== ###
### ================================================================== ###

### =========================
### Taxa normal 
### =========================

### Taxa normal  - Taxa ###

mSn0<- glmer(ValorDificuldade ~ taxa +
		  (1|NomeGame),family=Gamma(link="sqrt"), data=dados)

mSn1<- glmer(ValorDificuldade ~ taxa +
		  (taxa|NomeGame),family=Gamma(link="sqrt"), data=dados)

anova(mSn1, mSn0) #mSn1

summary(mSn1)
cor.test(predict(mSn1, type="response"), dados$ValorDificuldade)$estimate^2
AIC(mSn1)
jpeg(filename = "plot19.jpg", width = 480, height = 480)
F1(mSn1, main="mSn1", ylim=c(0,100))
dev.off()

### Taxa normal - quadrado ###

mSn2<- glmer(ValorDificuldade ~ taxa + taxa2 +
		  (1|NomeGame),family=Gamma(link="sqrt"), data=dados)

mSn3<- glmer(ValorDificuldade ~ taxa + taxa2 +
		  (taxa|NomeGame),family=Gamma(link="sqrt"), data=dados)

mSn4<- glmer(ValorDificuldade ~ taxa + taxa2 +
		  (taxa2|NomeGame),family=Gamma(link="sqrt"), data=dados)

anova(mSn3, mSn2) #mSn3
anova(mSn4, mSn2) #mSn4

mSn5<- glmer(ValorDificuldade ~ taxa + taxa2 +
		  (taxa + taxa2|NomeGame),family=Gamma(link="sqrt"), data=dados)

anova(mSn5, mSn3) #mSn3
anova(mSn5, mSn4) #mSn4

AIC(mSn3, mSn4)

summary(mSn4)
cor.test(predict(mSn4, type="response"), dados$ValorDificuldade)$estimate^2
AIC(mSn4)
jpeg(filename = "plot20.jpg", width = 480, height = 480)
F1(mSn4, main="mSn4", ylim=c(0,100))
dev.off()

### Taxa normal - nó ###

mSn6<- glmer(ValorDificuldade ~ taxaMa + taxaMe +
		  (1|NomeGame),family=Gamma(link="sqrt"), data=dados)

mSn7<- glmer(ValorDificuldade ~ taxaMa + taxaMe +
		  (taxaMa|NomeGame),family=Gamma(link="sqrt"), data=dados)

mSn8<- glmer(ValorDificuldade ~ taxaMa + taxaMe +
		  (taxaMe|NomeGame),family=Gamma(link="sqrt"), data=dados)

anova(mSn7, mSn6) #mSn7
anova(mSn8, mSn6) #mSn6

mSn9<- glmer(ValorDificuldade ~ taxaMa + taxaMe +
		  (taxaMa + taxaMe|NomeGame),family=Gamma(link="sqrt"), data=dados)

anova(mSn9, mSn7) #mSn7
anova(mSn9, mSn8) #mSn9

summary(mSn7)
cor.test(predict(mSn7, type="response"), dados$ValorDificuldade)$estimate^2
AIC(mSn7)
jpeg(filename = "plot21.jpg", width = 480, height = 480)
F1(mSn7, main="mSn7", ylim=c(0,100))
dev.off()

### ===================
### Taxa inversa
### ===================

### Taxa inversa  - Taxa ###

mSi0<- glmer(ValorDificuldade ~ taxaInv +
		  (1|NomeGame),family=Gamma(link="sqrt"), data=dados)

mSi1<- glmer(ValorDificuldade ~ taxaInv +
		  (taxaInv|NomeGame),family=Gamma(link="sqrt"), data=dados)

anova(mSi1, mSi0) #mSi1

summary(mSi1)
cor.test(predict(mSi1, type="response"), dados$ValorDificuldade)$estimate^2
AIC(mSi1)
jpeg(filename = "plot22.jpg", width = 480, height = 480)
F1(mSi1, main="mSi1", ylim=c(0,100))
dev.off()

### Taxa inversa - quadrado ###

mSi2<- glmer(ValorDificuldade ~ taxaInv + taxaInv2 +
		  (1|NomeGame),family=Gamma(link="sqrt"), data=dados)

mSi3<- glmer(ValorDificuldade ~ taxaInv + taxaInv2 +
		  (taxaInv|NomeGame),family=Gamma(link="sqrt"), data=dados)

mSi4<- glmer(ValorDificuldade ~ taxaInv + taxaInv2 +
		  (taxaInv2|NomeGame),family=Gamma(link="sqrt"), data=dados)

anova(mSi3, mSi2) #mSi3
anova(mSi4, mSi2) #mSi4

mSi5<- glmer(ValorDificuldade ~ taxaInv + taxaInv2 +
		  (taxaInv + taxaInv2|NomeGame),family=Gamma(link="sqrt"), data=dados)

anova(mSi5, mSi3) #mSi3
anova(mSi5, mSi4) #mSi5

summary(mSi3)
cor.test(predict(mSi3, type="response"), dados$ValorDificuldade)$estimate^2
AIC(mSi3)
jpeg(filename = "plot23.jpg", width = 480, height = 480)
F1(mSi3, main="mSi3", ylim=c(0,100))
dev.off()

### Taxa inversa - nó ###

mSi6<- glmer(ValorDificuldade ~ taxaInvMa + taxaInvMe +
		  (1|NomeGame),family=Gamma(link="sqrt"), data=dados)

mSi7<- glmer(ValorDificuldade ~ taxaInvMa + taxaInvMe +
		  (taxaInvMa|NomeGame),family=Gamma(link="sqrt"), data=dados)

mSi8<- glmer(ValorDificuldade ~ taxaInvMa + taxaInvMe +
		  (taxaInvMe|NomeGame),family=Gamma(link="sqrt"), data=dados)

anova(mSi7, mSi6) #mSi7
anova(mSi8, mSi6) #mSi8

mSi9<- glmer(ValorDificuldade ~ taxaInvMa + taxaInvMe +
		  (taxaInvMa + taxaInvMe|NomeGame),family=Gamma(link="sqrt"), data=dados)

anova(mSi9, mSi7) #mSi7
anova(mSi9, mSi8) #mSi8

AIC(mSi7, mSi8)

summary(mSi8)
cor.test(predict(mSi8, type="response"), dados$ValorDificuldade)$estimate^2
AIC(mSi8)
jpeg(filename = "plot24.jpg", width = 480, height = 480)
F1(mSi8, main="mSi8", ylim=c(0,100))
dev.off()

### ===================
### Taxa log
### ===================

### Taxa log  - Taxa ###

mSl0<- glmer(ValorDificuldade ~ taxaLog +
		  (1|NomeGame),family=Gamma(link="sqrt"), data=dados)

mSl1<- glmer(ValorDificuldade ~ taxaLog +
		  (taxaLog|NomeGame),family=Gamma(link="sqrt"), data=dados)

anova(mSl1, mSl0) #mSl1

summary(mSl1)
cor.test(predict(mSl1, type="response"), dados$ValorDificuldade)$estimate^2
AIC(mSl1)
jpeg(filename = "plot25.jpg", width = 480, height = 480)
F1(mSl1, main="mSl1", ylim=c(0,100))
dev.off()

### Taxa log - quadrado ###

mSl2<- glmer(ValorDificuldade ~ taxaLog + taxaLog2 +
		  (1|NomeGame),family=Gamma(link="sqrt"), data=dados)

mSl3<- glmer(ValorDificuldade ~ taxaLog + taxaLog2 +
		  (taxaLog|NomeGame),family=Gamma(link="sqrt"), data=dados)

mSl4<- glmer(ValorDificuldade ~ taxaLog + taxaLog2 +
		  (taxaLog2|NomeGame),family=Gamma(link="sqrt"), data=dados)

anova(mSl3, mSl2) #mSl3
anova(mSl4, mSl2) #mSl4

mSl5<- glmer(ValorDificuldade ~ taxaLog + taxaLog2 +
		  (taxaLog + taxaLog2|NomeGame),family=Gamma(link="sqrt"), data=dados)

anova(mSl5, mSl3) #mSl3
anova(mSl5, mSl4) #mSl4

AIC(mSl3, mSl4)

summary(mSl4)
cor.test(predict(mSl4, type="response"), dados$ValorDificuldade)$estimate^2
AIC(mSl4)
jpeg(filename = "plot26.jpg", width = 480, height = 480)
F1(mSl4, main="mSl4", ylim=c(0,100))
dev.off()

### Taxa log - nó ###

mSl6<- glmer(ValorDificuldade ~ taxaLogMa + taxaLogMe +
		  (1|NomeGame),family=Gamma(link="sqrt"), data=dados)

mSl7<- glmer(ValorDificuldade ~ taxaLogMa + taxaLogMe +
		  (taxaLogMa|NomeGame),family=Gamma(link="sqrt"), data=dados)

mSl8<- glmer(ValorDificuldade ~ taxaLogMa + taxaLogMe +
		  (taxaLogMe|NomeGame),family=Gamma(link="sqrt"), data=dados)

anova(mSl7, mSl6) #mSl7
anova(mSl8, mSl6) #mSl6

mSl9<- glmer(ValorDificuldade ~ taxaLogMa + taxaLogMe +
		  (taxaLogMa + taxaLogMe|NomeGame),family=Gamma(link="sqrt"), data=dados)

anova(mSl9, mSl7) #mSl7
anova(mSl9, mSl8) #mSl8

summary(mSl7)
cor.test(predict(mSl7, type="response"), dados$ValorDificuldade)$estimate^2
AIC(mSl7)
jpeg(filename = "plot27.jpg", width = 480, height = 480)
F1(mSl7, main="mSl7", ylim=c(0,100))
dev.off()

### ================================================================== ###
### ============================ Inverse ============================= ###
### ================================================================== ###

### =========================
### Taxa normal 
### =========================

### Taxa normal  - Taxa ###

mINn0<- glmer(ValorDificuldade ~ taxa +
		  (1|NomeGame),family=Gamma(link="inverse"), data=dados)

mINn1<- glmer(ValorDificuldade ~ taxa +
		  (taxa|NomeGame),family=Gamma(link="inverse"), data=dados)

anova(mINn1, mINn0) #mINn1

summary(mINn1)
cor.test(predict(mINn1, type="response"), dados$ValorDificuldade)$estimate^2
AIC(mINn1)
jpeg(filename = "plot28.jpg", width = 480, height = 480)
F1(mINn1, main="mINn1", ylim=c(0,100))
dev.off()

### Taxa normal - quadrado ###

mINn2<- glmer(ValorDificuldade ~ taxa + taxa2 +
		  (1|NomeGame),family=Gamma(link="inverse"), data=dados)

mINn3<- glmer(ValorDificuldade ~ taxa + taxa2 +
		  (taxa|NomeGame),family=Gamma(link="inverse"), data=dados)

mINn4<- glmer(ValorDificuldade ~ taxa + taxa2 +
		  (taxa2|NomeGame),family=Gamma(link="inverse"), data=dados)

anova(mINn3, mINn2) #mINn3
anova(mINn4, mINn2) #mINn4

mINn5<- glmer(ValorDificuldade ~ taxa + taxa2 +
		  (taxa + taxa2|NomeGame),family=Gamma(link="inverse"), data=dados)

anova(mINn5, mINn3) #mINn3
anova(mINn5, mINn4) #mINn4

AIC(mINn3, mINn4)

summary(mINn3)
cor.test(predict(mINn3, type="response"), dados$ValorDificuldade)$estimate^2
AIC(mINn3)
jpeg(filename = "plot29.jpg", width = 480, height = 480)
F1(mINn3, main="mINn3", ylim=c(0,100))
dev.off()

### Taxa normal - nó ###

mINn6<- glmer(ValorDificuldade ~ taxaMa + taxaMe +
		  (1|NomeGame),family=Gamma(link="inverse"), data=dados)

mINn7<- glmer(ValorDificuldade ~ taxaMa + taxaMe +
		  (taxaMa|NomeGame),family=Gamma(link="inverse"), data=dados)

mINn8<- glmer(ValorDificuldade ~ taxaMa + taxaMe +
		  (taxaMe|NomeGame),family=Gamma(link="inverse"), data=dados)

anova(mINn7, mINn6) #mINn7
anova(mINn8, mINn6) #mINn6

mINn9<- glmer(ValorDificuldade ~ taxaMa + taxaMe +
		  (taxaMa + taxaMe|NomeGame),family=Gamma(link="inverse"), data=dados)

anova(mINn9, mINn7) #mINn7
anova(mINn9, mINn8) #mINn9

summary(mINn9)
cor.test(predict(mINn9, type="response"), dados$ValorDificuldade)$estimate^2
AIC(mINn9)
jpeg(filename = "plot30.jpg", width = 480, height = 480)
F1(mINn9, main="mINn9", ylim=c(0,100))
dev.off()

### ===================
### Taxa inversa
### ===================

### Taxa inversa  - Taxa ###

mINi0<- glmer(ValorDificuldade ~ taxaInv +
		  (1|NomeGame),family=Gamma(link="inverse"), data=dados)

mINi1<- glmer(ValorDificuldade ~ taxaInv +
		  (taxaInv|NomeGame),family=Gamma(link="inverse"), data=dados)

anova(mINi1, mINi0) #mINi1

summary(mINi1)
cor.test(predict(mINi1, type="response"), dados$ValorDificuldade)$estimate^2
AIC(mINi1)
jpeg(filename = "plot31.jpg", width = 480, height = 480)
F1(mINi1, main="mINi1", ylim=c(0,100))
dev.off()

### Taxa inversa - quadrado ###

mINi2<- glmer(ValorDificuldade ~ taxaInv + taxaInv2 +
		  (1|NomeGame),family=Gamma(link="inverse"), data=dados)

mINi3<- glmer(ValorDificuldade ~ taxaInv + taxaInv2 +
		  (taxaInv|NomeGame),family=Gamma(link="inverse"), data=dados)

mINi4<- glmer(ValorDificuldade ~ taxaInv + taxaInv2 +
		  (taxaInv2|NomeGame),family=Gamma(link="inverse"), data=dados)

anova(mINi3, mINi2) #mINi3
anova(mINi4, mINi2) #mINi4

mINi5<- glmer(ValorDificuldade ~ taxaInv + taxaInv2 +
		  (taxaInv + taxaInv2|NomeGame),family=Gamma(link="inverse"), data=dados)

anova(mINi5, mINi3) #mINi3
anova(mINi5, mINi4) #mINi4

AIC(mINi3, mINi4)

summary(mINi3)
cor.test(predict(mINi3, type="response"), dados$ValorDificuldade)$estimate^2
AIC(mINi3)
jpeg(filename = "plot32.jpg", width = 480, height = 480)
F1(mINi3, main="mINi3", ylim=c(0,100))
dev.off()

### Taxa inversa - nó ###

mINi6<- glmer(ValorDificuldade ~ taxaInvMa + taxaInvMe +
		  (1|NomeGame),family=Gamma(link="inverse"), data=dados)

mINi7<- glmer(ValorDificuldade ~ taxaInvMa + taxaInvMe +
		  (taxaInvMa|NomeGame),family=Gamma(link="inverse"), data=dados)

mINi8<- glmer(ValorDificuldade ~ taxaInvMa + taxaInvMe +
		  (taxaInvMe|NomeGame),family=Gamma(link="inverse"), data=dados)

anova(mINi7, mINi6) #mINi7
anova(mINi8, mINi6) #mINi8

mINi9<- glmer(ValorDificuldade ~ taxaInvMa + taxaInvMe +
		  (taxaInvMa + taxaInvMe|NomeGame),family=Gamma(link="inverse"), data=dados)

anova(mINi9, mINi7) #mINi9
anova(mINi9, mINi8) #mINi9

summary(mINi9)
cor.test(predict(mINi9, type="response"), dados$ValorDificuldade)$estimate^2
AIC(mINi9)
jpeg(filename = "plot33.jpg", width = 480, height = 480)
F1(mINi9, main="mINi9", ylim=c(0,100))
dev.off()

### ===================
### Taxa log
### ===================

### Taxa log  - Taxa ###

mINl0<- glmer(ValorDificuldade ~ taxaLog +
		  (1|NomeGame),family=Gamma(link="inverse"), data=dados)

mINl1<- glmer(ValorDificuldade ~ taxaLog +
		  (taxaLog|NomeGame),family=Gamma(link="inverse"), data=dados)

anova(mINl1, mINl0) #mINl1

summary(mINl1)
cor.test(predict(mINl1, type="response"), dados$ValorDificuldade)$estimate^2
AIC(mINl1)
jpeg(filename = "plot34.jpg", width = 480, height = 480)
F1(mINl1, main="mINl1", ylim=c(0,100))
dev.off()

### Taxa log - quadrado ###

mINl2<- glmer(ValorDificuldade ~ taxaLog + taxaLog2 +
		  (1|NomeGame),family=Gamma(link="inverse"), data=dados)

mINl3<- glmer(ValorDificuldade ~ taxaLog + taxaLog2 +
		  (taxaLog|NomeGame),family=Gamma(link="inverse"), data=dados)

mINl4<- glmer(ValorDificuldade ~ taxaLog + taxaLog2 +
		  (taxaLog2|NomeGame),family=Gamma(link="inverse"), data=dados)

anova(mINl3, mINl2) #mINl3
anova(mINl4, mINl2) #mINl4

mINl5<- glmer(ValorDificuldade ~ taxaLog + taxaLog2 +
		  (taxaLog + taxaLog2|NomeGame),family=Gamma(link="inverse"), data=dados)

anova(mINl5, mINl3) #mINl3
anova(mINl5, mINl4) #mINl4

AIC(mINl3, mINl4)

summary(mINl3)
cor.test(predict(mINl3, type="response"), dados$ValorDificuldade)$estimate^2
AIC(mINl3)
jpeg(filename = "plot35.jpg", width = 480, height = 480)
F1(mINl3, main="mINl3", ylim=c(0,100))
dev.off()

### Taxa log - nó ###

mINl6<- glmer(ValorDificuldade ~ taxaLogMa + taxaLogMe +
		  (1|NomeGame),family=Gamma(link="inverse"), data=dados)

mINl7<- glmer(ValorDificuldade ~ taxaLogMa + taxaLogMe +
		  (taxaLogMa|NomeGame),family=Gamma(link="inverse"), data=dados)

mINl8<- glmer(ValorDificuldade ~ taxaLogMa + taxaLogMe +
		  (taxaLogMe|NomeGame),family=Gamma(link="inverse"), data=dados)

anova(mINl7, mINl6) #mINl7
anova(mINl8, mINl6) #mINl6

mINl9<- glmer(ValorDificuldade ~ taxaLogMa + taxaLogMe +
		  (taxaLogMa + taxaLogMe|NomeGame),family=Gamma(link="inverse"), data=dados)

anova(mINl9, mINl7) #mINl9
anova(mINl9, mINl8) #mINl9

summary(mINl9)
cor.test(predict(mINl9, type="response"), dados$ValorDificuldade)$estimate^2
AIC(mINl9)
jpeg(filename = "plot36.jpg", width = 480, height = 480)
F1(mINl9, main="mINl9", ylim=c(0,100))
dev.off()

### ================================================================== ###
### ========================= Banco Sucesso ========================== ###
### ================================================================== ###

dados1<- subset(dados, dados$Sucess==1)

### ================================================================== ###
### ============================ Identity ============================ ###
### ================================================================== ###

### =========================
### Taxa normal 
### =========================

### Taxa normal  - Taxa ###

AmIn0<- glmer(ValorDificuldade ~ taxa +
		  (1|NomeGame),family=Gamma(link="identity"), data=dados1)

AmIn1<- glmer(ValorDificuldade ~ taxa +
		  (taxa|NomeGame),family=Gamma(link="identity"), data=dados1)

anova(AmIn1, AmIn0) #AmIn1

summary(AmIn1)
cor.test(predict(AmIn1, type="response"), dados1$ValorDificuldade)$estimate^2
AIC(AmIn1)

jpeg(filename = "Aplot1.jpg", width = 480, height = 480)
F1(AmIn1, main="AmIn1", ylim=c(0,100))
dev.off()

### Taxa normal - quadrado ###

AmIn2<- glmer(ValorDificuldade ~ taxa + taxa2 +
		  (1|NomeGame),family=Gamma(link="identity"), data=dados1)

AmIn3<- glmer(ValorDificuldade ~ taxa + taxa2 +
		  (taxa|NomeGame),family=Gamma(link="identity"), data=dados1)

AmIn4<- glmer(ValorDificuldade ~ taxa + taxa2 +
		  (taxa2|NomeGame),family=Gamma(link="identity"), data=dados1)

anova(AmIn3, AmIn2) #AmIn3
anova(AmIn4, AmIn2) #AmIn4

AmIn5<- glmer(ValorDificuldade ~ taxa + taxa2 +
		  (taxa + taxa2|NomeGame),family=Gamma(link="identity"), data=dados1)

anova(AmIn5, AmIn3) #AmIn3
anova(AmIn5, AmIn4) #AmIn4

AIC(AmIn3, AmIn4)

summary(AmIn4)
cor.test(predict(AmIn4, type="response"), dados1$ValorDificuldade)$estimate^2
AIC(AmIn4)
jpeg(filename = "Aplot2.jpg", width = 480, height = 480)
F1(AmIn4, main="AmIn4", ylim=c(0,100))
dev.off()

### ===================
### Taxa inversa
### ===================

### Taxa inversa  - Taxa ###

AmIi0<- glmer(ValorDificuldade ~ taxaInv +
		  (1|NomeGame),family=Gamma(link="identity"), data=dados1)

AmIi1<- glmer(ValorDificuldade ~ taxaInv +
		  (taxaInv|NomeGame),family=Gamma(link="identity"), data=dados1)

anova(AmIi1, AmIi0) #AmIi1

summary(AmIi1)
cor.test(predict(AmIi1, type="response"), dados1$ValorDificuldade)$estimate^2
AIC(AmIi1)
jpeg(filename = "Aplot4.jpg", width = 480, height = 480)
F1(AmIi1, main="AmIi1", ylim=c(0,100))
dev.off()

### Taxa inversa - quadrado ###

AmIi2<- glmer(ValorDificuldade ~ taxaInv + taxaInv2 +
		  (1|NomeGame),family=Gamma(link="identity"), data=dados1)

AmIi3<- glmer(ValorDificuldade ~ taxaInv + taxaInv2 +
		  (taxaInv|NomeGame),family=Gamma(link="identity"), data=dados1)

AmIi4<- glmer(ValorDificuldade ~ taxaInv + taxaInv2 +
		  (taxaInv2|NomeGame),family=Gamma(link="identity"), data=dados1)

anova(AmIi3, AmIi2) #AmIi3
anova(AmIi4, AmIi2) #AmIi4

AmIi5<- glmer(ValorDificuldade ~ taxaInv + taxaInv2 +
		  (taxaInv + taxaInv2|NomeGame),family=Gamma(link="identity"), data=dados1)

anova(AmIi5, AmIi3) #AmIi3
anova(AmIi5, AmIi4) #AmIi4

AIC(AmIi3, AmIi4)

summary(AmIi4)
cor.test(predict(AmIi4, type="response"), dados1$ValorDificuldade)$estimate^2
AIC(AmIi4)
jpeg(filename = "Aplot4.jpg", width = 480, height = 480)
F1(AmIi4, main="AmIi4", ylim=c(0,100))
dev.off()

### ===================
### Taxa log
### ===================

### Taxa log  - Taxa ###

AmIl0<- glmer(ValorDificuldade ~ taxaLog +
		  (1|NomeGame),family=Gamma(link="identity"), data=dados1)

AmIl1<- glmer(ValorDificuldade ~ taxaLog +
		  (taxaLog|NomeGame),family=Gamma(link="identity"), data=dados1)

anova(AmIl1, AmIl0) #AmIl1

summary(AmIl1)
cor.test(predict(AmIl1, type="response"), dados1$ValorDificuldade)$estimate^2
AIC(AmIl1)
jpeg(filename = "Aplot7.jpg", width = 480, height = 480)
F1(AmIl1, main="AmIl1", ylim=c(0,100))
dev.off()

### Taxa log - quadrado ###

AmIl2<- glmer(ValorDificuldade ~ taxaLog + taxaLog2 +
		  (1|NomeGame),family=Gamma(link="identity"), data=dados1)

AmIl3<- glmer(ValorDificuldade ~ taxaLog + taxaLog2 +
		  (taxaLog|NomeGame),family=Gamma(link="identity"), data=dados1)

AmIl4<- glmer(ValorDificuldade ~ taxaLog + taxaLog2 +
		  (taxaLog2|NomeGame),family=Gamma(link="identity"), data=dados1)

anova(AmIl3, AmIl2) #AmIl3
anova(AmIl4, AmIl2) #AmIl4

AmIl5<- glmer(ValorDificuldade ~ taxaLog + taxaLog2 +
		  (taxaLog + taxaLog2|NomeGame),family=Gamma(link="identity"), data=dados1)

anova(AmIl5, AmIl3) #AmIl3
anova(AmIl5, AmIl4) #AmIl4

AIC(AmIl3, AmIl4)

summary(AmIl4)
cor.test(predict(AmIl4, type="response"), dados1$ValorDificuldade)$estimate^2
AIC(AmIl4)
jpeg(filename = "Aplot8.jpg", width = 480, height = 480)
F1(AmIl4, main="AmIl4", ylim=c(0,100))
dev.off()

### ================================================================== ###
### ============================== Log =============================== ###
### ================================================================== ###

### =========================
### Taxa normal 
### =========================

### Taxa normal  - Taxa ###

AmLn0<- glmer(ValorDificuldade ~ taxa +
		  (1|NomeGame),family=Gamma(link="log"), data=dados1)

AmLn1<- glmer(ValorDificuldade ~ taxa +
		  (taxa|NomeGame),family=Gamma(link="log"), data=dados1)

anova(AmLn1, AmLn0) #AmLn1

summary(AmLn0)
cor.test(predict(AmLn0, type="response"), dados1$ValorDificuldade)$estimate^2
AIC(AmLn0)
jpeg(filename = "Aplot10.jpg", width = 480, height = 480)
F1(AmLn0, main="AmLn0", ylim=c(0,100))
dev.off()

### Taxa normal - quadrado ###

AmLn2<- glmer(ValorDificuldade ~ taxa + taxa2 +
		  (1|NomeGame),family=Gamma(link="log"), data=dados1)

AmLn3<- glmer(ValorDificuldade ~ taxa + taxa2 +
		  (taxa|NomeGame),family=Gamma(link="log"), data=dados1)

AmLn4<- glmer(ValorDificuldade ~ taxa + taxa2 +
		  (taxa2|NomeGame),family=Gamma(link="log"), data=dados1)

anova(AmLn3, AmLn2) #AmLn2
anova(AmLn4, AmLn2) #AmLn2

summary(AmLn2)
cor.test(predict(AmLn2, type="response"), dados1$ValorDificuldade)$estimate^2
AIC(AmLn2)
jpeg(filename = "Aplot11.jpg", width = 280, height = 280)
F1(AmLn2, main="AmLn2", ylim=c(0,100))
dev.off()

### ===================
### Taxa inversa
### ===================

### Taxa inversa  - Taxa ###

AmLi0<- glmer(ValorDificuldade ~ taxaInv +
		  (1|NomeGame),family=Gamma(link="log"), data=dados1)

AmLi1<- glmer(ValorDificuldade ~ taxaInv +
		  (taxaInv|NomeGame),family=Gamma(link="log"), data=dados1)

anova(AmLi1, AmLi0) #AmLi1

summary(AmLi0)
cor.test(predict(AmLi0, type="response"), dados1$ValorDificuldade)$estimate^2
AIC(AmLi0)
jpeg(filename = "Aplot13.jpg", width = 480, height = 480)
F1(AmLi0, main="AmLi0", ylim=c(0,100))
dev.off()

### Taxa inversa - quadrado ###

AmLi2<- glmer(ValorDificuldade ~ taxaInv + taxaInv2 +
		  (1|NomeGame),family=Gamma(link="log"), data=dados1)

AmLi3<- glmer(ValorDificuldade ~ taxaInv + taxaInv2 +
		  (taxaInv|NomeGame),family=Gamma(link="log"), data=dados1)

AmLi4<- glmer(ValorDificuldade ~ taxaInv + taxaInv2 +
		  (taxaInv2|NomeGame),family=Gamma(link="log"), data=dados1)

anova(AmLi3, AmLi2) #AmLi2
anova(AmLi4, AmLi2) #AmLi2

summary(AmLi2)
cor.test(predict(AmLi2, type="response"), dados1$ValorDificuldade)$estimate^2
AIC(AmLi2)
jpeg(filename = "Aplot14.jpg", width = 480, height = 480)
F1(AmLi2, main="AmLi2", ylim=c(0,100))
dev.off()

### ===================
### Taxa log
### ===================

### Taxa log  - Taxa ###

AmLl0<- glmer(ValorDificuldade ~ taxaLog +
		  (1|NomeGame),family=Gamma(link="log"), data=dados1)

AmLl1<- glmer(ValorDificuldade ~ taxaLog +
		  (taxaLog|NomeGame),family=Gamma(link="log"), data=dados1)

anova(AmLl1, AmLl0) #AmLl0

summary(AmLl0)
cor.test(predict(AmLl0, type="response"), dados1$ValorDificuldade)$estimate^2
AIC(AmLl0)
jpeg(filename = "Aplot16.jpg", width = 480, height = 480)
F1(AmLl0, main="AmLl0", ylim=c(0,100))
dev.off()

### Taxa log - quadrado ###

AmLl2<- glmer(ValorDificuldade ~ taxaLog + taxaLog2 +
		  (1|NomeGame),family=Gamma(link="log"), data=dados1)

AmLl3<- glmer(ValorDificuldade ~ taxaLog + taxaLog2 +
		  (taxaLog|NomeGame),family=Gamma(link="log"), data=dados1)

AmLl4<- glmer(ValorDificuldade ~ taxaLog + taxaLog2 +
		  (taxaLog2|NomeGame),family=Gamma(link="log"), data=dados1)

anova(AmLl3, AmLl2) #AmLl2
anova(AmLl4, AmLl2) #AmLl2

summary(AmLl2)
cor.test(predict(AmLl2, type="response"), dados1$ValorDificuldade)$estimate^2
AIC(AmLl2)
jpeg(filename = "Aplot17.jpg", width = 480, height = 480)
F1(AmLl2, main="AmLl2", ylim=c(0,100))
dev.off()

### ================================================================== ###
### ============================= SQRT =============================== ###
### ================================================================== ###

### =========================
### Taxa normal 
### =========================

### Taxa normal  - Taxa ###

AmSn0<- glmer(ValorDificuldade ~ taxa +
		  (1|NomeGame),family=Gamma(link="sqrt"), data=dados1)

AmSn1<- glmer(ValorDificuldade ~ taxa +
		  (taxa|NomeGame),family=Gamma(link="sqrt"), data=dados1)

anova(AmSn1, AmSn0) #AmSn1

summary(AmSn1)
cor.test(predict(AmSn1, type="response"), dados1$ValorDificuldade)$estimate^2
AIC(AmSn1)
jpeg(filename = "Aplot19.jpg", width = 480, height = 480)
F1(AmSn1, main="AmSn1", ylim=c(0,100))
dev.off()

### Taxa normal - quadrado ###

AmSn2<- glmer(ValorDificuldade ~ taxa + taxa2 +
		  (1|NomeGame),family=Gamma(link="sqrt"), data=dados1)

AmSn3<- glmer(ValorDificuldade ~ taxa + taxa2 +
		  (taxa|NomeGame),family=Gamma(link="sqrt"), data=dados1)

AmSn4<- glmer(ValorDificuldade ~ taxa + taxa2 +
		  (taxa2|NomeGame),family=Gamma(link="sqrt"), data=dados1)

anova(AmSn3, AmSn2) #AmSn3
anova(AmSn4, AmSn2) #AmSn4

AmSn5<- glmer(ValorDificuldade ~ taxa + taxa2 +
		  (taxa + taxa2|NomeGame),family=Gamma(link="sqrt"), data=dados1)

anova(AmSn5, AmSn3) #AmSn3
anova(AmSn5, AmSn4) #AmSn4

AIC(AmSn3, AmSn4)

summary(AmSn3)
cor.test(predict(AmSn3, type="response"), dados1$ValorDificuldade)$estimate^2
AIC(AmSn3)
jpeg(filename = "Aplot20.jpg", width = 480, height = 480)
F1(AmSn3, main="AmSn3", ylim=c(0,100))
dev.off()

### ===================
### Taxa inversa
### ===================

### Taxa inversa  - Taxa ###

AmSi0<- glmer(ValorDificuldade ~ taxaInv +
		  (1|NomeGame),family=Gamma(link="sqrt"), data=dados1)

AmSi1<- glmer(ValorDificuldade ~ taxaInv +
		  (taxaInv|NomeGame),family=Gamma(link="sqrt"), data=dados1)

anova(AmSi1, AmSi0) #AmSi1

summary(AmSi1)
cor.test(predict(AmSi1, type="response"), dados1$ValorDificuldade)$estimate^2
AIC(AmSi1)
jpeg(filename = "Aplot22.jpg", width = 480, height = 480)
F1(AmSi1, main="AmSi1", ylim=c(0,100))
dev.off()

### Taxa inversa - quadrado ###

AmSi2<- glmer(ValorDificuldade ~ taxaInv + taxaInv2 +
		  (1|NomeGame),family=Gamma(link="sqrt"), data=dados1)

AmSi3<- glmer(ValorDificuldade ~ taxaInv + taxaInv2 +
		  (taxaInv|NomeGame),family=Gamma(link="sqrt"), data=dados1)

AmSi4<- glmer(ValorDificuldade ~ taxaInv + taxaInv2 +
		  (taxaInv2|NomeGame),family=Gamma(link="sqrt"), data=dados1)

anova(AmSi3, AmSi2) #AmSi3
anova(AmSi4, AmSi2) #AmSi4

AmSi5<- glmer(ValorDificuldade ~ taxaInv + taxaInv2 +
		  (taxaInv + taxaInv2|NomeGame),family=Gamma(link="sqrt"), data=dados1)

anova(AmSi5, AmSi3) #AmSi3
anova(AmSi5, AmSi4) #AmSi4

AIC(AmSi3, AmSi4)

summary(AmSi3)
cor.test(predict(AmSi3, type="response"), dados1$ValorDificuldade)$estimate^2
AIC(AmSi3)
jpeg(filename = "Aplot23.jpg", width = 480, height = 480)
F1(AmSi3, main="AmSi3", ylim=c(0,100))
dev.off()

### ===================
### Taxa log
### ===================

### Taxa log  - Taxa ###

AmSl0<- glmer(ValorDificuldade ~ taxaLog +
		  (1|NomeGame),family=Gamma(link="sqrt"), data=dados1)

AmSl1<- glmer(ValorDificuldade ~ taxaLog +
		  (taxaLog|NomeGame),family=Gamma(link="sqrt"), data=dados1)

anova(AmSl1, AmSl0) #AmSl1

summary(AmSl1)
cor.test(predict(AmSl1, type="response"), dados1$ValorDificuldade)$estimate^2
AIC(AmSl1)
jpeg(filename = "Aplot25.jpg", width = 480, height = 480)
F1(AmSl1, main="AmSl1", ylim=c(0,100))
dev.off()

### Taxa log - quadrado ###

AmSl2<- glmer(ValorDificuldade ~ taxaLog + taxaLog2 +
		  (1|NomeGame),family=Gamma(link="sqrt"), data=dados1)

AmSl3<- glmer(ValorDificuldade ~ taxaLog + taxaLog2 +
		  (taxaLog|NomeGame),family=Gamma(link="sqrt"), data=dados1)

AmSl4<- glmer(ValorDificuldade ~ taxaLog + taxaLog2 +
		  (taxaLog2|NomeGame),family=Gamma(link="sqrt"), data=dados1)

anova(AmSl3, AmSl2) #AmSl3
anova(AmSl4, AmSl2) #AmSl4

AmSl5<- glmer(ValorDificuldade ~ taxaLog + taxaLog2 +
		  (taxaLog + taxaLog2|NomeGame),family=Gamma(link="sqrt"), data=dados1)

anova(AmSl5, AmSl3) #AmSl3
anova(AmSl5, AmSl4) #AmSl4

AIC(AmSl3, AmSl4)

summary(AmSl3)
cor.test(predict(AmSl3, type="response"), dados1$ValorDificuldade)$estimate^2
AIC(AmSl3)
jpeg(filename = "Aplot26.jpg", width = 480, height = 480)
F1(AmSl3, main="AmSl3", ylim=c(0,100))
dev.off()

### ================================================================== ###
### ============================ Inverse ============================= ###
### ================================================================== ###

### =========================
### Taxa normal 
### =========================

### Taxa normal  - Taxa ###

AmInn0<- glmer(ValorDificuldade ~ taxa +
		  (1|NomeGame),family=Gamma(link="inverse"), data=dados1)

AmInn1<- glmer(ValorDificuldade ~ taxa +
		  (taxa|NomeGame),family=Gamma(link="inverse"), data=dados1)

anova(AmInn1, AmInn0) #AmInn0

summary(AmInn0)
cor.test(predict(AmInn0, type="response"), dados1$ValorDificuldade)$estimate^2
AIC(AmInn0)
jpeg(filename = "Aplot28.jpg", width = 480, height = 480)
F1(AmInn0, main="AmInn0", ylim=c(0,100))
dev.off()

### Taxa normal - quadrado ###

AmInn2<- glmer(ValorDificuldade ~ taxa + taxa2 +
		  (1|NomeGame),family=Gamma(link="inverse"), data=dados1)

AmInn3<- glmer(ValorDificuldade ~ taxa + taxa2 +
		  (taxa|NomeGame),family=Gamma(link="inverse"), data=dados1)

AmInn4<- glmer(ValorDificuldade ~ taxa + taxa2 +
		  (taxa2|NomeGame),family=Gamma(link="inverse"), data=dados1)

anova(AmInn3, AmInn2) #AmInn2
anova(AmInn4, AmInn2) #AmInn2

summary(AmInn2)
cor.test(predict(AmInn2, type="response"), dados1$ValorDificuldade)$estimate^2
AIC(AmInn2)
jpeg(filename = "Aplot29.jpg", width = 480, height = 480)
F1(AmInn2, main="AmInn2", ylim=c(0,100))
dev.off()

### ===================
### Taxa inversa
### ===================

### Taxa inversa  - Taxa ###

AmIni0<- glmer(ValorDificuldade ~ taxaInv +
		  (1|NomeGame),family=Gamma(link="inverse"), data=dados1)

AmIni1<- glmer(ValorDificuldade ~ taxaInv +
		  (taxaInv|NomeGame),family=Gamma(link="inverse"), data=dados1)

anova(AmIni1, AmIni0) #AmIni0

summary(AmIni0)
cor.test(predict(AmIni0, type="response"), dados1$ValorDificuldade)$estimate^2
AIC(AmIni0)
jpeg(filename = "Aplot31.jpg", width = 480, height = 480)
F1(AmIni0, main="AmIni0", ylim=c(0,100))
dev.off()

### Taxa inversa - quadrado ###

AmIni2<- glmer(ValorDificuldade ~ taxaInv + taxaInv2 +
		  (1|NomeGame),family=Gamma(link="inverse"), data=dados1)

AmIni3<- glmer(ValorDificuldade ~ taxaInv + taxaInv2 +
		  (taxaInv|NomeGame),family=Gamma(link="inverse"), data=dados1)

AmIni4<- glmer(ValorDificuldade ~ taxaInv + taxaInv2 +
		  (taxaInv2|NomeGame),family=Gamma(link="inverse"), data=dados1)

anova(AmIni3, AmIni2) #AmIni2
anova(AmIni4, AmIni2) #AmIni2

summary(AmIni2)
cor.test(predict(AmIni2, type="response"), dados1$ValorDificuldade)$estimate^2
AIC(AmIni2)
jpeg(filename = "Aplot32.jpg", width = 480, height = 480)
F1(AmIni2, main="AmIni2", ylim=c(0,100))
dev.off()

### ===================
### Taxa log
### ===================

### Taxa log  - Taxa ###

AmInl0<- glmer(ValorDificuldade ~ taxaLog +
		  (1|NomeGame),family=Gamma(link="inverse"), data=dados1)

AmInl1<- glmer(ValorDificuldade ~ taxaLog +
		  (taxaLog|NomeGame),family=Gamma(link="inverse"), data=dados1)

anova(AmInl1, AmInl0) #AmInl0

summary(AmInl0)
cor.test(predict(AmInl0, type="response"), dados1$ValorDificuldade)$estimate^2
AIC(AmInl0)
jpeg(filename = "Aplot34.jpg", width = 480, height = 480)
F1(AmInl0, main="AmInl0", ylim=c(0,100))
dev.off()

### Taxa log - quadrado ###

AmInl2<- glmer(ValorDificuldade ~ taxaLog + taxaLog2 +
		  (1|NomeGame),family=Gamma(link="inverse"), data=dados1)

AmInl3<- glmer(ValorDificuldade ~ taxaLog + taxaLog2 +
		  (taxaLog|NomeGame),family=Gamma(link="inverse"), data=dados1)

AmInl4<- glmer(ValorDificuldade ~ taxaLog + taxaLog2 +
		  (taxaLog2|NomeGame),family=Gamma(link="inverse"), data=dados1)

anova(AmInl3, AmInl2) #AmInl2
anova(AmInl4, AmInl2) #AmInl2

summary(AmInl2)
cor.test(predict(AmInl2, type="response"), dados1$ValorDificuldade)$estimate^2
AIC(AmInl2)
jpeg(filename = "Aplot35.jpg", width = 480, height = 480)
F1(AmInl2, main="AmInl2", ylim=c(0,100))
dev.off()

### ================================================================== ###
### ======================== Banco Tentativa ========================= ###
### ================================================================== ###

dados2<- subset(dados, dados$Tentativa==1)

### ================================================================== ###
### ============================ Identity ============================ ###
### ================================================================== ###

### =========================
### Taxa normal 
### =========================

### Taxa normal  - Taxa ###

TmIn0<- glmer(ValorDificuldade ~ taxa +
		  (1|NomeGame),family=Gamma(link="identity"), data=dados2)

TmIn1<- glmer(ValorDificuldade ~ taxa +
		  (taxa|NomeGame),family=Gamma(link="identity"), data=dados2)

anova(TmIn1, TmIn0) #TmIn1

summary(TmIn1)
cor.test(predict(TmIn1, type="response"), dados2$ValorDificuldade)$estimate^2
AIC(TmIn1)

jpeg(filename = "Tplot1.jpg", width = 480, height = 480)
F1(TmIn1, main="TmIn1", ylim=c(0,100))
dev.off()

### Taxa normal - quadrado ###

TmIn2<- glmer(ValorDificuldade ~ taxa + taxa2 +
		  (1|NomeGame) + (1|LevelID),family=Gamma(link="identity"), data=dados2)

TmIn3<- glmer(ValorDificuldade ~ taxa + taxa2 +
		  (taxa|NomeGame) + (1|LevelID),family=Gamma(link="identity"), data=dados2)

TmIn4<- glmer(ValorDificuldade ~ taxa + taxa2 +
		  (taxa2|NomeGame) + (1|LevelID),family=Gamma(link="identity"), data=dados2)

anova(TmIn3, TmIn2) #TmIn3
anova(TmIn4, TmIn2) #TmIn4

TmIn5<- glmer(ValorDificuldade ~ taxa + taxa2 +
		  (taxa + taxa2|NomeGame) + (1|LevelID),family=Gamma(link="identity"), data=dados2)

anova(TmIn5, TmIn3) #TmIn3
anova(TmIn5, TmIn4) #TmIn4

AIC(TmIn3, TmIn4)

summary(TmIn3)
cor.test(predict(TmIn3, type="response"), dados2$ValorDificuldade)$estimate^2
AIC(TmIn3)
jpeg(filename = "Tplot2.jpg", width = 480, height = 480)
F1(TmIn5, main="TmIn3", ylim=c(0,100))
dev.off()

### Taxa normal - nó ###

TmIn6<- glmer(ValorDificuldade ~ taxaMa + taxaMe +
		  (1|NomeGame),family=Gamma(link="identity"), data=dados2)

TmIn7<- glmer(ValorDificuldade ~ taxaMa + taxaMe +
		  (taxaMa|NomeGame),family=Gamma(link="identity"), data=dados2)

TmIn8<- glmer(ValorDificuldade ~ taxaMa + taxaMe +
		  (taxaMe|NomeGame),family=Gamma(link="identity"), data=dados2)

anova(TmIn7, TmIn6) #TmIn7
anova(TmIn8, TmIn6) #TmIn8

TmIn9<- glmer(ValorDificuldade ~ taxaMa + taxaMe +
		  (taxaMa + taxaMe|NomeGame),family=Gamma(link="identity"), data=dados2)

anova(TmIn9, TmIn7) #TmIn9
anova(TmIn9, TmIn8) #TmIn9

summary(TmIn9)
cor.test(predict(TmIn9, type="response"), dados2$ValorDificuldade)$estimate^2
AIC(TmIn9)
jpeg(filename = "Tplot3.jpg", width = 480, height = 480)
F1(TmIn9, main="TmIn9", ylim=c(0,100))
dev.off()

### ===================
### Taxa inversa
### ===================

### Taxa inversa  - Taxa ###

TmIi0<- glmer(ValorDificuldade ~ taxaInv +
		  (1|NomeGame),family=Gamma(link="identity"), data=dados2)

TmIi1<- glmer(ValorDificuldade ~ taxaInv +
		  (taxaInv|NomeGame),family=Gamma(link="identity"), data=dados2)

anova(TmIi1, TmIi0) #TmIi1

summary(TmIi1)
cor.test(predict(TmIi1, type="response"), dados2$ValorDificuldade)$estimate^2
AIC(TmIi1)
jpeg(filename = "Tplot4.jpg", width = 480, height = 480)
F1(TmIi1, main="TmIi1", ylim=c(0,100))
dev.off()

### Taxa inversa - quadrado ###

TmIi2<- glmer(ValorDificuldade ~ taxaInv + taxaInv2 +
		  (1|NomeGame),family=Gamma(link="identity"), data=dados2)

TmIi3<- glmer(ValorDificuldade ~ taxaInv + taxaInv2 +
		  (taxaInv|NomeGame),family=Gamma(link="identity"), data=dados2)

TmIi4<- glmer(ValorDificuldade ~ taxaInv + taxaInv2 +
		  (taxaInv2|NomeGame),family=Gamma(link="identity"), data=dados2)

anova(TmIi3, TmIi2) #TmIi3
anova(TmIi4, TmIi2) #TmIi4

TmIi5<- glmer(ValorDificuldade ~ taxaInv + taxaInv2 +
		  (taxaInv + taxaInv2|NomeGame),family=Gamma(link="identity"), data=dados2)

anova(TmIi5, TmIi3) #TmIi3
anova(TmIi5, TmIi4) #TmIi4

summary(TmIi3)
cor.test(predict(TmIi3, type="response"), dados2$ValorDificuldade)$estimate^2
AIC(TmIi3)
jpeg(filename = "Tplot3.jpg", width = 480, height = 480)
F1(TmIi3, main="TmIi3", ylim=c(0,100))
dev.off()

### Taxa inversa - nó ###

TmIi6<- glmer(ValorDificuldade ~ taxaInvMa + taxaInvMe +
		  (1|NomeGame),family=Gamma(link="identity"), data=dados2)

TmIi7<- glmer(ValorDificuldade ~ taxaInvMa + taxaInvMe +
		  (taxaInvMa|NomeGame),family=Gamma(link="identity"), data=dados2)

TmIi8<- glmer(ValorDificuldade ~ taxaInvMa + taxaInvMe +
		  (taxaInvMe|NomeGame),family=Gamma(link="identity"), data=dados2)

anova(TmIi7, TmIi6) #TmIi7
anova(TmIi8, TmIi6) #TmIi8

TmIi9<- glmer(ValorDificuldade ~ taxaInvMa + taxaInvMe +
		  (taxaInvMa + taxaInvMe|NomeGame),family=Gamma(link="identity"), data=dados2)

anova(TmIi9, TmIi7) #TmIi7
anova(TmIi9, TmIi8) #TmIi8

summary(TmIi8)
cor.test(predict(TmIi8, type="response"), dados2$ValorDificuldade)$estimate^2
AIC(TmIi8)
jpeg(filename = "Tplot6.jpg", width = 480, height = 480)
F1(TmIi8, main="TmIi8", ylim=c(0,100))
dev.off()

### ===================
### Taxa log
### ===================

### Taxa log  - Taxa ###

TmIl0<- glmer(ValorDificuldade ~ taxaLog +
		  (1|NomeGame),family=Gamma(link="identity"), data=dados2)

TmIl1<- glmer(ValorDificuldade ~ taxaLog +
		  (taxaLog|NomeGame),family=Gamma(link="identity"), data=dados2)

anova(TmIl1, TmIl0) #TmIl1

summary(TmIl1)
cor.test(predict(TmIl1, type="response"), dados2$ValorDificuldade)$estimate^2
AIC(TmIl1)
jpeg(filename = "Tplot7.jpg", width = 480, height = 480)
F1(TmIl1, main="TmIl1", ylim=c(0,100))
dev.off()

### Taxa log - quadrado ###

TmIl2<- glmer(ValorDificuldade ~ taxaLog + taxaLog2 +
		  (1|NomeGame) + (1|LevelID),family=Gamma(link="identity"), data=dados2)

TmIl3<- glmer(ValorDificuldade ~ taxaLog + taxaLog2 +
		  (taxaLog|NomeGame) + (1|LevelID),family=Gamma(link="identity"), data=dados2)

TmIl4<- glmer(ValorDificuldade ~ taxaLog + taxaLog2 +
		  (taxaLog2|NomeGame) + (1|LevelID),family=Gamma(link="identity"), data=dados2)

anova(TmIl3, TmIl2) #TmIl3
anova(TmIl4, TmIl2) #TmIl4

TmIl5<- glmer(ValorDificuldade ~ taxaLog + taxaLog2 +
		  (taxaLog + taxaLog2|NomeGame) + (1|LevelID),family=Gamma(link="identity"), data=dados2)

anova(TmIl5, TmIl3) #TmIl3
anova(TmIl5, TmIl4) #TmIl4

AIC(TmIl3, TmIl4)

summary(TmIl3)
cor.test(predict(TmIl3, type="response"), dados2$ValorDificuldade)$estimate^2
AIC(TmIl3)
jpeg(filename = "Tplot8.jpg", width = 480, height = 480)
F1(TmIl3, main="TmIl3", ylim=c(0,100))
dev.off()

### Taxa log - nó ###

TmIl6<- glmer(ValorDificuldade ~ taxaLogMa + taxaLogMe +
		  (1|NomeGame),family=Gamma(link="identity"), data=dados2)

TmIl7<- glmer(ValorDificuldade ~ taxaLogMa + taxaLogMe +
		  (taxaLogMa|NomeGame),family=Gamma(link="identity"), data=dados2)

TmIl8<- glmer(ValorDificuldade ~ taxaLogMa + taxaLogMe +
		  (taxaLogMe|NomeGame),family=Gamma(link="identity"), data=dados2)

anova(TmIl7, TmIl6) #TmIl7
anova(TmIl8, TmIl6) #TmIl8

TmIl9<- glmer(ValorDificuldade ~ taxaLogMa + taxaLogMe +
		  (taxaLogMa + taxaLogMe|NomeGame),family=Gamma(link="identity"), data=dados2)

anova(TmIl9, TmIl7) #TmIl9
anova(TmIl9, TmIl8) #TmIl9

summary(TmIl9)
cor.test(predict(TmIl9, type="response"), dados2$ValorDificuldade)$estimate^2
AIC(TmIl9)
jpeg(filename = "Tplot9.jpg", width = 480, height = 480)
F1(TmIl9, main="TmIl9", ylim=c(0,100))
dev.off()

### ================================================================== ###
### ============================== Log =============================== ###
### ================================================================== ###

### =========================
### Taxa normal 
### =========================

### Taxa normal  - Taxa ###

TmLn0<- glmer(ValorDificuldade ~ taxa +
		  (1|NomeGame) + (1|LevelID),family=Gamma(link="log"), data=dados2)

TmLn1<- glmer(ValorDificuldade ~ taxa +
		  (taxa|NomeGame) + (1|LevelID),family=Gamma(link="log"), data=dados2)

anova(TmLn1, TmLn0) #TmLn1

summary(TmLn1)
cor.test(predict(TmLn1, type="response"), dados2$ValorDificuldade)$estimate^2
AIC(TmLn1)
jpeg(filename = "Tplot10.jpg", width = 480, height = 480)
F1(TmLn1, main="TmLn1", ylim=c(0,100))
dev.off()

### Taxa normal - quadrado ###

TmLn2<- glmer(ValorDificuldade ~ taxa + taxa2 +
		  (1|NomeGame) + (1|LevelID),family=Gamma(link="log"), data=dados2)

TmLn3<- glmer(ValorDificuldade ~ taxa + taxa2 +
		  (taxa|NomeGame) + (1|LevelID),family=Gamma(link="log"), data=dados2)

TmLn4<- glmer(ValorDificuldade ~ taxa + taxa2 +
		  (taxa2|NomeGame) + (1|LevelID),family=Gamma(link="log"), data=dados2)

anova(TmLn3, TmLn2) #TmLn3
anova(TmLn4, TmLn2) #TmLn4

TmLn5<- glmer(ValorDificuldade ~ taxa + taxa2 +
		  (taxa + taxa2|NomeGame) + (1|LevelID),family=Gamma(link="log"), data=dados2)

anova(TmLn5, TmLn3) #TmLn3
anova(TmLn5, TmLn4) #TmLn4

AIC(TmLn3, TmLn4)

summary(TmLn3)
cor.test(predict(TmLn3, type="response"), dados2$ValorDificuldade)$estimate^2
AIC(TmLn3)
jpeg(filename = "Tplot11.jpg", width = 480, height = 480)
F1(TmLn3, main="TmLn3", ylim=c(0,100))
dev.off()

### Taxa normal - nó ###

TmLn6<- glmer(ValorDificuldade ~ taxaMa + taxaMe +
		  (1|NomeGame),family=Gamma(link="log"), data=dados2)

TmLn7<- glmer(ValorDificuldade ~ taxaMa + taxaMe +
		  (taxaMa|NomeGame),family=Gamma(link="log"), data=dados2)

TmLn8<- glmer(ValorDificuldade ~ taxaMa + taxaMe +
		  (taxaMe|NomeGame),family=Gamma(link="log"), data=dados2)

anova(TmLn7, TmLn6) #TmLn6
anova(TmLn8, TmLn6) #TmLn6

summary(TmLn6)
cor.test(predict(TmLn6, type="response"), dados2$ValorDificuldade)$estimate^2
AIC(TmLn6)
jpeg(filename = "Tplot12.jpg", width = 480, height = 480)
F1(TmLn6, main="TmLn6", ylim=c(0,100))
dev.off()

### ===================
### Taxa inversa
### ===================

### Taxa inversa  - Taxa ###

TmLi0<- glmer(ValorDificuldade ~ taxaInv +
		  (1|NomeGame),family=Gamma(link="log"), data=dados2)

TmLi1<- glmer(ValorDificuldade ~ taxaInv +
		  (taxaInv|NomeGame),family=Gamma(link="log"), data=dados2)

anova(TmLi1, TmLi0) #TmLi1

summary(TmLi1)
cor.test(predict(TmLi1, type="response"), dados2$ValorDificuldade)$estimate^2
AIC(TmLi1)
jpeg(filename = "Tplot13.jpg", width = 480, height = 480)
F1(TmLi1, main="TmLi1", ylim=c(0,100))
dev.off()

### Taxa inversa - quadrado ###

TmLi2<- glmer(ValorDificuldade ~ taxaInv + taxaInv2 +
		  (1|NomeGame),family=Gamma(link="log"), data=dados2)

TmLi3<- glmer(ValorDificuldade ~ taxaInv + taxaInv2 +
		  (taxaInv|NomeGame),family=Gamma(link="log"), data=dados2)

TmLi4<- glmer(ValorDificuldade ~ taxaInv + taxaInv2 +
		  (taxaInv2|NomeGame),family=Gamma(link="log"), data=dados2)

anova(TmLi3, TmLi2) #TmLi3
anova(TmLi4, TmLi2) #TmLi4

TmLi5<- glmer(ValorDificuldade ~ taxaInv + taxaInv2 +
		  (taxaInv + taxaInv2|NomeGame),family=Gamma(link="log"), data=dados2)

anova(TmLi5, TmLi3) #TmLi3
anova(TmLi5, TmLi4) #TmLi4

AIC(TmLi3, TmLi4)

summary(TmLi3)
cor.test(predict(TmLi3, type="response"), dados2$ValorDificuldade)$estimate^2
AIC(TmLi3)
jpeg(filename = "Tplot14.jpg", width = 480, height = 480)
F1(TmLi3, main="TmLi3", ylim=c(0,100))
dev.off()

### Taxa inversa - nó ###

TmLi6<- glmer(ValorDificuldade ~ taxaInvMa + taxaInvMe +
		  (1|NomeGame),family=Gamma(link="log"), data=dados2)

TmLi7<- glmer(ValorDificuldade ~ taxaInvMa + taxaInvMe +
		  (taxaInvMa|NomeGame),family=Gamma(link="log"), data=dados2)

TmLi8<- glmer(ValorDificuldade ~ taxaInvMa + taxaInvMe +
		  (taxaInvMe|NomeGame),family=Gamma(link="log"), data=dados2)

anova(TmLi7, TmLi6) #TmLi6
anova(TmLi8, TmLi6) #TmLi6

summary(TmLi6)
cor.test(predict(TmLi6, type="response"), dados2$ValorDificuldade)$estimate^2
AIC(TmLi6)
jpeg(filename = "Tplot15.jpg", width = 480, height = 480)
F1(TmLi6, main="TmLi6", ylim=c(0,100))
dev.off()

### ===================
### Taxa log
### ===================

### Taxa log  - Taxa ###

TmLl0<- glmer(ValorDificuldade ~ taxaLog +
		  (1|NomeGame) + (1|LevelID),family=Gamma(link="log"), data=dados2)

TmLl1<- glmer(ValorDificuldade ~ taxaLog +
		  (taxaLog|NomeGame) + (1|LevelID),family=Gamma(link="log"), data=dados2)

anova(TmLl1, TmLl0) #TmLl1

summary(TmLl1)
cor.test(predict(TmLl1, type="response"), dados2$ValorDificuldade)$estimate^2
AIC(TmLl1)
jpeg(filename = "Tplot16.jpg", width = 480, height = 480)
F1(TmLl1, main="TmLl1", ylim=c(0,100))
dev.off()

### Taxa log - quadrado ###

TmLl2<- glmer(ValorDificuldade ~ taxaLog + taxaLog2 +
		  (1|NomeGame) + (1|LevelID),family=Gamma(link="log"), data=dados2)

TmLl3<- glmer(ValorDificuldade ~ taxaLog + taxaLog2 +
		  (taxaLog|NomeGame) + (1|LevelID),family=Gamma(link="log"), data=dados2)

TmLl4<- glmer(ValorDificuldade ~ taxaLog + taxaLog2 +
		  (taxaLog2|NomeGame) + (1|LevelID),family=Gamma(link="log"), data=dados2)

anova(TmLl3, TmLl2) #TmLl3
anova(TmLl4, TmLl2) #TmLl4

TmLl5<- glmer(ValorDificuldade ~ taxaLog + taxaLog2 +
		  (taxaLog + taxaLog2|NomeGame) + (1|LevelID),family=Gamma(link="log"), data=dados2)

anova(TmLl5, TmLl3) #TmLl3
anova(TmLl5, TmLl4) #TmLl4

AIC(TmLl3, TmLl4)

summary(TmLl3)
cor.test(predict(TmLl3, type="response"), dados2$ValorDificuldade)$estimate^2
AIC(TmLl3)
jpeg(filename = "Tplot17.jpg", width = 480, height = 480)
F1(TmLl3, main="TmLl3", ylim=c(0,100))
dev.off()

### Taxa log - nó ###

TmLl6<- glmer(ValorDificuldade ~ taxaLogMa + taxaLogMe +
		  (1|NomeGame),family=Gamma(link="log"), data=dados2)

TmLl7<- glmer(ValorDificuldade ~ taxaLogMa + taxaLogMe +
		  (taxaLogMa|NomeGame),family=Gamma(link="log"), data=dados2)

TmLl8<- glmer(ValorDificuldade ~ taxaLogMa + taxaLogMe +
		  (taxaLogMe|NomeGame),family=Gamma(link="log"), data=dados2)

anova(TmLl7, TmLl6) #TmLl6
anova(TmLl8, TmLl6) #TmLl6

summary(TmLl6)
cor.test(predict(TmLl6, type="response"), dados2$ValorDificuldade)$estimate^2
AIC(TmLl6)
jpeg(filename = "Tplot18.jpg", width = 480, height = 480)
F1(TmLl6, main="TmLl6", ylim=c(0,100))
dev.off()

### ================================================================== ###
### ============================= SQRT =============================== ###
### ================================================================== ###

### =========================
### Taxa normal 
### =========================

### Taxa normal  - Taxa ###

TmSn0<- glmer(ValorDificuldade ~ taxa +
		  (1|NomeGame) + (1|LevelID),family=Gamma(link="sqrt"), data=dados2)

TmSn1<- glmer(ValorDificuldade ~ taxa +
		  (taxa|NomeGame) + (1|LevelID),family=Gamma(link="sqrt"), data=dados2)

anova(TmSn1, TmSn0) #TmSn1

summary(TmSn1)
cor.test(predict(TmSn1, type="response"), dados2$ValorDificuldade)$estimate^2
AIC(TmSn1)
jpeg(filename = "Tplot19.jpg", width = 480, height = 480)
F1(TmSn1, main="TmSn1", ylim=c(0,100))
dev.off()

### Taxa normal - quadrado ###

TmSn2<- glmer(ValorDificuldade ~ taxa + taxa2 +
		  (1|NomeGame) + (1 | LevelID),family=Gamma(link="sqrt"), data=dados2)

TmSn3<- glmer(ValorDificuldade ~ taxa + taxa2 +
		  (taxa|NomeGame) + (1 | LevelID),family=Gamma(link="sqrt"), data=dados2)

TmSn4<- glmer(ValorDificuldade ~ taxa + taxa2 +
		  (taxa2|NomeGame) + (1 | LevelID),family=Gamma(link="sqrt"), data=dados2)

anova(TmSn3, TmSn2) #TmSn3
anova(TmSn4, TmSn2) #TmSn4

TmSn5<- glmer(ValorDificuldade ~ taxa + taxa2 +
		  (taxa + taxa2|NomeGame) + (1 | LevelID),family=Gamma(link="sqrt"), data=dados2)

anova(TmSn5, TmSn3) #TmSn3
anova(TmSn5, TmSn4) #TmSn4

AIC(TmSn3, TmSn4)

summary(TmSn4)
cor.test(predict(TmSn4, type="response"), dados2$ValorDificuldade)$estimate^2
AIC(TmSn4)
jpeg(filename = "Tplot20.jpg", width = 480, height = 480)
F1(TmSn4, main="TmSn4", ylim=c(0,100))
dev.off()

### Taxa normal - nó ###

TmSn6<- glmer(ValorDificuldade ~ taxaMa + taxaMe +
		  (1|NomeGame),family=Gamma(link="sqrt"), data=dados2)

TmSn7<- glmer(ValorDificuldade ~ taxaMa + taxaMe +
		  (taxaMa|NomeGame),family=Gamma(link="sqrt"), data=dados2)

TmSn8<- glmer(ValorDificuldade ~ taxaMa + taxaMe +
		  (taxaMe|NomeGame),family=Gamma(link="sqrt"), data=dados2)

anova(TmSn7, TmSn6) #TmSn7
anova(TmSn8, TmSn6) #TmSn6

TmSn9<- glmer(ValorDificuldade ~ taxaMa + taxaMe +
		  (taxaMa + taxaMe|NomeGame),family=Gamma(link="sqrt"), data=dados2)

anova(TmSn9, TmSn7) #TmSn9
anova(TmSn9, TmSn8) #TmSn9

summary(TmSn9)
cor.test(predict(TmSn9, type="response"), dados2$ValorDificuldade)$estimate^2
AIC(TmSn9)
jpeg(filename = "Tplot21.jpg", width = 480, height = 480)
F1(TmSn9, main="TmSn9", ylim=c(0,100))
dev.off()

### ===================
### Taxa inversa
### ===================

### Taxa inversa  - Taxa ###

TmSi0<- glmer(ValorDificuldade ~ taxaInv +
		  (1|NomeGame),family=Gamma(link="sqrt"), data=dados2)

TmSi1<- glmer(ValorDificuldade ~ taxaInv +
		  (taxaInv|NomeGame),family=Gamma(link="sqrt"), data=dados2)

anova(TmSi1, TmSi0) #TmSi1

summary(TmSi1)
cor.test(predict(TmSi1, type="response"), dados2$ValorDificuldade)$estimate^2
AIC(TmSi1)
jpeg(filename = "Tplot22.jpg", width = 480, height = 480)
F1(TmSi1, main="TmSi1", ylim=c(0,100))
dev.off()

### Taxa inversa - quadrado ###

TmSi2<- glmer(ValorDificuldade ~ taxaInv + taxaInv2 +
		  (1|NomeGame),family=Gamma(link="sqrt"), data=dados2)

TmSi3<- glmer(ValorDificuldade ~ taxaInv + taxaInv2 +
		  (taxaInv|NomeGame),family=Gamma(link="sqrt"), data=dados2)

TmSi4<- glmer(ValorDificuldade ~ taxaInv + taxaInv2 +
		  (taxaInv2|NomeGame),family=Gamma(link="sqrt"), data=dados2)

anova(TmSi3, TmSi2) #TmSi3
anova(TmSi4, TmSi2) #TmSi4

TmSi5<- glmer(ValorDificuldade ~ taxaInv + taxaInv2 +
		  (taxaInv + taxaInv2|NomeGame),family=Gamma(link="sqrt"), data=dados2)

anova(TmSi5, TmSi3) #TmSi3
anova(TmSi5, TmSi4) #TmSi4

AIC(TmSi3, TmSi4)

summary(TmSi3)
cor.test(predict(TmSi3, type="response"), dados2$ValorDificuldade)$estimate^2
AIC(TmSi3)
jpeg(filename = "Tplot23.jpg", width = 480, height = 480)
F1(TmSi3, main="TmSi3", ylim=c(0,100))
dev.off()

### Taxa inversa - nó ###

TmSi6<- glmer(ValorDificuldade ~ taxaInvMa + taxaInvMe +
		  (1|NomeGame),family=Gamma(link="sqrt"), data=dados2)

TmSi7<- glmer(ValorDificuldade ~ taxaInvMa + taxaInvMe +
		  (taxaInvMa|NomeGame),family=Gamma(link="sqrt"), data=dados2)

TmSi8<- glmer(ValorDificuldade ~ taxaInvMa + taxaInvMe +
		  (taxaInvMe|NomeGame),family=Gamma(link="sqrt"), data=dados2)

anova(TmSi7, TmSi6) #TmSi6
anova(TmSi8, TmSi6) #TmSi6

summary(TmSi6)
cor.test(predict(TmSi6, type="response"), dados2$ValorDificuldade)$estimate^2
AIC(TmSi6)
jpeg(filename = "Tplot24.jpg", width = 480, height = 480)
F1(TmSi6, main="TmSi6", ylim=c(0,100))
dev.off()

### ===================
### Taxa log
### ===================

### Taxa log  - Taxa ###

mSl0<- glmer(ValorDificuldade ~ taxaLog +
		  (1|NomeGame) + (1|LevelID),family=Gamma(link="sqrt"), data=dados2)

mSl1<- glmer(ValorDificuldade ~ taxaLog +
		  (taxaLog|NomeGame) + (1|LevelID),family=Gamma(link="sqrt"), data=dados2)

anova(mSl1, mSl0) #mSl1

summary(mSl1)
cor.test(predict(mSl1, type="response"), dados2$ValorDificuldade)$estimate^2
AIC(mSl1)
jpeg(filename = "Tplot25.jpg", width = 480, height = 480)
F1(mSl1, main="mSl1", ylim=c(0,100))
dev.off()

### Taxa log - quadrado ###

mSl2<- glmer(ValorDificuldade ~ taxaLog + taxaLog2 +
		  (1|NomeGame) + (1|LevelID),family=Gamma(link="sqrt"), data=dados2)

mSl3<- glmer(ValorDificuldade ~ taxaLog + taxaLog2 +
		  (taxaLog|NomeGame) + (1|LevelID),family=Gamma(link="sqrt"), data=dados2)

mSl4<- glmer(ValorDificuldade ~ taxaLog + taxaLog2 +
		  (taxaLog2|NomeGame) + (1|LevelID),family=Gamma(link="sqrt"), data=dados2)

anova(mSl3, mSl2) #mSl3
anova(mSl4, mSl2) #mSl4

mSl5<- glmer(ValorDificuldade ~ taxaLog + taxaLog2 +
		  (taxaLog + taxaLog2|NomeGame) + (1|LevelID),family=Gamma(link="sqrt"), data=dados2)

anova(mSl5, mSl3) #mSl3
anova(mSl5, mSl4) #mSl4

AIC(mSl3, mSl4)

summary(mSl4)
cor.test(predict(mSl4, type="response"), dados2$ValorDificuldade)$estimate^2
AIC(mSl4)
jpeg(filename = "Tplot26.jpg", width = 480, height = 480)
F1(mSl4, main="mSl4", ylim=c(0,100))
dev.off()

### Taxa log - nó ###

mSl6<- glmer(ValorDificuldade ~ taxaLogMa + taxaLogMe +
		  (1|NomeGame),family=Gamma(link="sqrt"), data=dados2)

mSl7<- glmer(ValorDificuldade ~ taxaLogMa + taxaLogMe +
		  (taxaLogMa|NomeGame),family=Gamma(link="sqrt"), data=dados2)

mSl8<- glmer(ValorDificuldade ~ taxaLogMa + taxaLogMe +
		  (taxaLogMe|NomeGame),family=Gamma(link="sqrt"), data=dados2)

anova(mSl7, mSl6) #mSl7
anova(mSl8, mSl6) #mSl6

mSl9<- glmer(ValorDificuldade ~ taxaLogMa + taxaLogMe +
		  (taxaLogMa + taxaLogMe|NomeGame),family=Gamma(link="sqrt"), data=dados2)

anova(mSl9, mSl7) #mSl9
anova(mSl9, mSl8) #mSl9

summary(mSl9)
cor.test(predict(mSl9, type="response"), dados2$ValorDificuldade)$estimate^2
AIC(mSl9)
jpeg(filename = "Tplot27.jpg", width = 480, height = 480)
F1(mSl9, main="mSl9", ylim=c(0,100))
dev.off()

### ================================================================== ###
### ============================ Inverse ============================= ###
### ================================================================== ###

### =========================
### Taxa normal 
### =========================

### Taxa normal  - Taxa ###

TmInn0<- glmer(ValorDificuldade ~ taxa +
		  (1|NomeGame),family=Gamma(link="inverse"), data=dados2)

TmInn1<- glmer(ValorDificuldade ~ taxa +
		  (taxa|NomeGame),family=Gamma(link="inverse"), data=dados2)

anova(TmInn1, TmInn0) #TmInn1

summary(TmInn1)
cor.test(predict(TmInn1, type="response"), dados2$ValorDificuldade)$estimate^2
AIC(TmInn1)
jpeg(filename = "Tplot28.jpg", width = 480, height = 480)
F1(TmInn1, main="TmInn1", ylim=c(0,100))
dev.off()

### Taxa normal - quadrado ###

TmInn2<- glmer(ValorDificuldade ~ taxa + taxa2 +
		  (1|NomeGame),family=Gamma(link="inverse"), data=dados2)

TmInn3<- glmer(ValorDificuldade ~ taxa + taxa2 +
		  (taxa|NomeGame),family=Gamma(link="inverse"), data=dados2)

TmInn4<- glmer(ValorDificuldade ~ taxa + taxa2 +
		  (taxa2|NomeGame),family=Gamma(link="inverse"), data=dados2)

anova(TmInn3, TmInn2) #TmInn3
anova(TmInn4, TmInn2) #TmInn4

TmInn5<- glmer(ValorDificuldade ~ taxa + taxa2 +
		  (taxa + taxa2|NomeGame),family=Gamma(link="inverse"), data=dados2)

anova(TmInn5, TmInn3) #TmInn3
anova(TmInn5, TmInn4) #TmInn4

AIC(TmInn3, TmInn4)

summary(TmInn3)
cor.test(predict(TmInn3, type="response"), dados2$ValorDificuldade)$estimate^2
AIC(TmInn3)
jpeg(filename = "Tplot29.jpg", width = 480, height = 480)
F1(TmInn3, main="TmInn3", ylim=c(0,100))
dev.off()

### Taxa normal - nó ###

TmInn6<- glmer(ValorDificuldade ~ taxaMa + taxaMe +
		  (1|NomeGame),family=Gamma(link="inverse"), data=dados2)

TmInn7<- glmer(ValorDificuldade ~ taxaMa + taxaMe +
		  (taxaMa|NomeGame),family=Gamma(link="inverse"), data=dados2)

TmInn8<- glmer(ValorDificuldade ~ taxaMa + taxaMe +
		  (taxaMe|NomeGame),family=Gamma(link="inverse"), data=dados2)

anova(TmInn7, TmInn6) #TmInn6
anova(TmInn8, TmInn6) #TmInn6

summary(TmInn6)
cor.test(predict(TmInn6, type="response"), dados2$ValorDificuldade)$estimate^2
AIC(TmInn6)
jpeg(filename = "Tplot30.jpg", width = 480, height = 480)
F1(TmInn6, main="TmInn6", ylim=c(0,100))
dev.off()

### ===================
### Taxa inversa
### ===================

### Taxa inversa  - Taxa ###

TmIni0<- glmer(ValorDificuldade ~ taxaInv +
		  (1|NomeGame),family=Gamma(link="inverse"), data=dados2)

TmIni1<- glmer(ValorDificuldade ~ taxaInv +
		  (taxaInv|NomeGame),family=Gamma(link="inverse"), data=dados2)

anova(TmIni1, TmIni0) #TmIni0

summary(TmIni0)
cor.test(predict(TmIni0, type="response"), dados2$ValorDificuldade)$estimate^2
AIC(TmIni0)
jpeg(filename = "Tplot31.jpg", width = 480, height = 480)
F1(TmIni0, main="TmIni0", ylim=c(0,100))
dev.off()

### Taxa inversa - quadrado ###

TmIni2<- glmer(ValorDificuldade ~ taxaInv + taxaInv2 +
		  (1|NomeGame),family=Gamma(link="inverse"), data=dados2)

TmIni3<- glmer(ValorDificuldade ~ taxaInv + taxaInv2 +
		  (taxaInv|NomeGame),family=Gamma(link="inverse"), data=dados2)

TmIni4<- glmer(ValorDificuldade ~ taxaInv + taxaInv2 +
		  (taxaInv2|NomeGame),family=Gamma(link="inverse"), data=dados2)

anova(TmIni3, TmIni2) #TmIni2
anova(TmIni4, TmIni2) #TmIni4

TmIni5<- glmer(ValorDificuldade ~ taxaInv + taxaInv2 +
		  (taxaInv + taxaInv2|NomeGame),family=Gamma(link="inverse"), data=dados2)

anova(TmIni5, TmIni3) #TmIni5
anova(TmIni5, TmIni4) #TmIni4

summary(TmIni4)
cor.test(predict(TmIni4, type="response"), dados2$ValorDificuldade)$estimate^2
AIC(TmIni4)
jpeg(filename = "Tplot42.jpg", width = 480, height = 480)
F1(TmIni4, main="TmIni4", ylim=c(0,100))
dev.off()

### Taxa inversa - nó ###

TmIni6<- glmer(ValorDificuldade ~ taxaInvMa + taxaInvMe +
		  (1|NomeGame),family=Gamma(link="inverse"), data=dados2)

TmIni7<- glmer(ValorDificuldade ~ taxaInvMa + taxaInvMe +
		  (taxaInvMa|NomeGame),family=Gamma(link="inverse"), data=dados2)

TmIni8<- glmer(ValorDificuldade ~ taxaInvMa + taxaInvMe +
		  (taxaInvMe|NomeGame),family=Gamma(link="inverse"), data=dados2)

anova(TmIni7, TmIni6) #TmIni6
anova(TmIni8, TmIni6) #TmIni6

summary(TmIni6)
cor.test(predict(TmIni6, type="response"), dados2$ValorDificuldade)$estimate^2
AIC(TmIni6)
jpeg(filename = "Tplot33.jpg", width = 480, height = 480)
F1(TmIni6, main="TmIni6", ylim=c(0,100))
dev.off()

### ===================
### Taxa log
### ===================

### Taxa log  - Taxa ###

TmInl0<- glmer(ValorDificuldade ~ taxaLog +
		  (1|NomeGame),family=Gamma(link="inverse"), data=dados2)

TmInl1<- glmer(ValorDificuldade ~ taxaLog +
		  (taxaLog|NomeGame),family=Gamma(link="inverse"), data=dados2)

anova(TmInl1, TmInl0) #TmInl1

summary(TmInl1)
cor.test(predict(TmInl1, type="response"), dados2$ValorDificuldade)$estimate^2
AIC(TmInl1)
jpeg(filename = "Tplot34.jpg", width = 480, height = 480)
F1(TmInl1, main="TmInl1", ylim=c(0,100))
dev.off()

### Taxa log - quadrado ###

TmInl2<- glmer(ValorDificuldade ~ taxaLog + taxaLog2 +
		  (1|NomeGame),family=Gamma(link="inverse"), data=dados2)

TmInl3<- glmer(ValorDificuldade ~ taxaLog + taxaLog2 +
		  (taxaLog|NomeGame),family=Gamma(link="inverse"), data=dados2)

TmInl4<- glmer(ValorDificuldade ~ taxaLog + taxaLog2 +
		  (taxaLog2|NomeGame),family=Gamma(link="inverse"), data=dados2)

anova(TmInl3, TmInl2) #TmInl3
anova(TmInl4, TmInl2) #TmInl4

TmInl5<- glmer(ValorDificuldade ~ taxaLog + taxaLog2 +
		  (taxaLog + taxaLog2|NomeGame),family=Gamma(link="inverse"), data=dados2)

anova(TmInl5, TmInl3) #TmInl3
anova(TmInl5, TmInl4) #TmInl4

AIC(TmInl3, TmInl4)

summary(TmInl3)
cor.test(predict(TmInl3, type="response"), dados2$ValorDificuldade)$estimate^2
AIC(TmInl3)
jpeg(filename = "Tplot35.jpg", width = 480, height = 480)
F1(TmInl3, main="TmInl3", ylim=c(0,100))
dev.off()

### Taxa log - nó ###

TmInl6<- glmer(ValorDificuldade ~ taxaLogMa + taxaLogMe +
		  (1|NomeGame),family=Gamma(link="inverse"), data=dados2)

TmInl7<- glmer(ValorDificuldade ~ taxaLogMa + taxaLogMe +
		  (taxaLogMa|NomeGame),family=Gamma(link="inverse"), data=dados2)

TmInl8<- glmer(ValorDificuldade ~ taxaLogMa + taxaLogMe +
		  (taxaLogMe|NomeGame),family=Gamma(link="inverse"), data=dados2)

anova(TmInl7, TmInl6) #TmInl6
anova(TmInl8, TmInl6) #TmInl6

summary(TmInl6)
cor.test(predict(TmInl6, type="response"), dados2$ValorDificuldade)$estimate^2
AIC(TmInl6)
jpeg(filename = "Tplot36.jpg", width = 480, height = 480)
F1(TmInl6, main="TmInl6", ylim=c(0,100))
dev.off()

###########################################################################
##########################################################################

### ================================================================== ###
### ======================== Banco Tentativa ========================= ###
### ================================================================== ###

dados2<- subset(dados, dados$Tentativa==1)

### ================================================================== ###
### ============================ Identity ============================ ###
### ================================================================== ###

### =========================
### Taxa normal 
### =========================

### Taxa normal  - Taxa ###

TmIn0<- glmer(ValorDificuldade ~ taxa +
		  (1|NomeGame),family=Gamma(link="identity"), data=dados2)

TmIn1<- glmer(ValorDificuldade ~ taxa +
		  (taxa|NomeGame),family=Gamma(link="identity"), data=dados2)

anova(TmIn1, TmIn0) #TmIn1

summary(TmIn1)
cor.test(predict(TmIn1, type="response"), dados2$ValorDificuldade)$estimate^2
AIC(TmIn1)

jpeg(filename = "Tplot1.jpg", width = 480, height = 480)
F1(TmIn1, main="TmIn1", ylim=c(0,100))
dev.off()

### Taxa normal - quadrado ###

TmIn2<- glmer(ValorDificuldade ~ taxa + taxa2 +
		  (1|NomeGame),family=Gamma(link="identity"), data=dados2)

TmIn3<- glmer(ValorDificuldade ~ taxa + taxa2 +
		  (taxa|NomeGame),family=Gamma(link="identity"), data=dados2)

TmIn4<- glmer(ValorDificuldade ~ taxa + taxa2 +
		  (taxa2|NomeGame),family=Gamma(link="identity"), data=dados2)

anova(TmIn3, TmIn2) #TmIn3
anova(TmIn4, TmIn2) #TmIn4

TmIn5<- glmer(ValorDificuldade ~ taxa + taxa2 +
		  (taxa + taxa2|NomeGame),family=Gamma(link="identity"), data=dados2)

anova(TmIn5, TmIn3) #TmIn3
anova(TmIn5, TmIn4) #TmIn4

AIC(TmIn3, TmIn4)

summary(TmIn3)
cor.test(predict(TmIn3, type="response"), dados2$ValorDificuldade)$estimate^2
AIC(TmIn3)
jpeg(filename = "Tplot2.jpg", width = 480, height = 480)
F1(TmIn5, main="TmIn3", ylim=c(0,100))
dev.off()

### ===================
### Taxa log
### ===================

### Taxa log  - Taxa ###

TmIl0<- glmer(ValorDificuldade ~ taxaLog +
		  (1|NomeGame),family=Gamma(link="identity"), data=dados2)

TmIl1<- glmer(ValorDificuldade ~ taxaLog +
		  (taxaLog|NomeGame),family=Gamma(link="identity"), data=dados2)

anova(TmIl1, TmIl0) #TmIl1

summary(TmIl1)
cor.test(predict(TmIl1, type="response"), dados2$ValorDificuldade)$estimate^2
AIC(TmIl1)
jpeg(filename = "Tplot7.jpg", width = 480, height = 480)
F1(TmIl1, main="TmIl1", ylim=c(0,100))
dev.off()

### Taxa log - quadrado ###

TmIl2<- glmer(ValorDificuldade ~ taxaLog + taxaLog2 +
		  (1|NomeGame),family=Gamma(link="identity"), data=dados2)

TmIl3<- glmer(ValorDificuldade ~ taxaLog + taxaLog2 +
		  (taxaLog|NomeGame),family=Gamma(link="identity"), data=dados2)

TmIl4<- glmer(ValorDificuldade ~ taxaLog + taxaLog2 +
		  (taxaLog2|NomeGame),family=Gamma(link="identity"), data=dados2)

anova(TmIl3, TmIl2) #TmIl3
anova(TmIl4, TmIl2) #TmIl4

TmIl5<- glmer(ValorDificuldade ~ taxaLog + taxaLog2 +
		  (taxaLog + taxaLog2|NomeGame),family=Gamma(link="identity"), data=dados2)

anova(TmIl5, TmIl3) #TmIl3
anova(TmIl5, TmIl4) #TmIl4

AIC(TmIl3, TmIl4)

summary(TmIl3)
cor.test(predict(TmIl3, type="response"), dados2$ValorDificuldade)$estimate^2
AIC(TmIl3)
jpeg(filename = "Tplot8.jpg", width = 480, height = 480)
F1(TmIl3, main="TmIl3", ylim=c(0,100))
dev.off()

### ================================================================== ###
### ============================== Log =============================== ###
### ================================================================== ###

### =========================
### Taxa normal 
### =========================

### Taxa normal  - Taxa ###

TmLn0<- glmer(ValorDificuldade ~ taxa +
		  (1|NomeGame),family=Gamma(link="log"), data=dados2)

TmLn1<- glmer(ValorDificuldade ~ taxa +
		  (taxa|NomeGame),family=Gamma(link="log"), data=dados2)

anova(TmLn1, TmLn0) #TmLn1

summary(TmLn1)
cor.test(predict(TmLn1, type="response"), dados2$ValorDificuldade)$estimate^2
AIC(TmLn1)
jpeg(filename = "Tplot10.jpg", width = 480, height = 480)
F1(TmLn1, main="TmLn1", ylim=c(0,100))
dev.off()

### Taxa normal - quadrado ###

TmLn2<- glmer(ValorDificuldade ~ taxa + taxa2 +
		  (1|NomeGame),family=Gamma(link="log"), data=dados2)

TmLn3<- glmer(ValorDificuldade ~ taxa + taxa2 +
		  (taxa|NomeGame),family=Gamma(link="log"), data=dados2)

TmLn4<- glmer(ValorDificuldade ~ taxa + taxa2 +
		  (taxa2|NomeGame),family=Gamma(link="log"), data=dados2)

anova(TmLn3, TmLn2) #TmLn3
anova(TmLn4, TmLn2) #TmLn4

TmLn5<- glmer(ValorDificuldade ~ taxa + taxa2 +
		  (taxa + taxa2|NomeGame),family=Gamma(link="log"), data=dados2)

anova(TmLn5, TmLn3) #TmLn3
anova(TmLn5, TmLn4) #TmLn4

AIC(TmLn3, TmLn4)

summary(TmLn3)
cor.test(predict(TmLn3, type="response"), dados2$ValorDificuldade)$estimate^2
AIC(TmLn3)
jpeg(filename = "Tplot11.jpg", width = 480, height = 480)
F1(TmLn3, main="TmLn3", ylim=c(0,100))
dev.off()

dados$taxac<- dados$taxa-0.7
dados$taxac2<- dados$taxac^2
dados2<- subset(dados, dados$Tentativa==1)

par(mar=c(4,4,2,2))
Newdata$pred<- predict(TmLn3, newdata=Newdata, type="response")
Newdata1<- subset(Newdata, Newdata$NomeGame=="arteGalacticaData")
Newdata2<- subset(Newdata, Newdata$NomeGame=="decifrandoCodigosData")
Newdata3<- subset(Newdata, Newdata$NomeGame=="desafioDosOpostosData")
Newdata4<- subset(Newdata, Newdata$NomeGame=="exploradorData")
Newdata5<- subset(Newdata, Newdata$NomeGame=="laboratorioData")
Newdata6<- subset(Newdata, Newdata$NomeGame=="pulandoAsteroidesData")
Newdata7<- subset(Newdata, Newdata$NomeGame=="tunelAceleradorData")

svg(filename = "Gráfico.svg", width = 9, height = 7)
plot(Newdata1$pred ~ Newdata1$taxa, main="", type="l", col="#d9f0a3", ylim=c(0,100), ylab="Dificuldade", xlab="Performance", lwd=2)
lines(Newdata2$pred ~ Newdata2$taxa, type="l", col="#addd8e", lwd=2)
lines(Newdata3$pred ~ Newdata3$taxa, type="l", col="#7fcdbb", lwd=2)
lines(Newdata4$pred ~ Newdata4$taxa, type="l", col="#41ab5d", lwd=2)

lines(Newdata5$pred ~ Newdata5$taxa, type="l", col="#238443", lwd=2)
lines(Newdata6$pred ~ Newdata6$taxa, type="l", lwd=2, col="#253494")
lines(Newdata7$pred ~ Newdata7$taxa, type="l", lwd=2, col="#1d91c0")

legend("topleft", c("Arte Galáctica", "Decifrando Códigos", "Desafio dos Opostos"
,"Explorador", "Laboratório", "Pulando Asteroides", "Túnel Acelerador"), lty=1,
col=c("#d9f0a3","#addd8e", "#7fcdbb", "#41ab5d", "#238443", "#253494", "#1d91c0"),
lwd=2, bty="n", cex=0.9)
dev.off()

mod<- glm(ValorDificuldade ~ taxa,family=Gamma(link="sqrt"), data=dados2)
y1<- predict(mod, type="response")
x1<- summary(mod)$coef[1,1]
x2<- summary(mod)$coef[2,1]

y2<- (x1+x2*dados2$taxa)^2

cbind(y1, y2)

### ===================
### Taxa log
### ===================

### Taxa log  - Taxa ###

TmLl0<- glmer(ValorDificuldade ~ taxaLog +
		  (1|NomeGame),family=Gamma(link="log"), data=dados2)

TmLl1<- glmer(ValorDificuldade ~ taxaLog +
		  (taxaLog|NomeGame),family=Gamma(link="log"), data=dados2)

anova(TmLl1, TmLl0) #TmLl1

summary(TmLl1)
cor.test(predict(TmLl1, type="response"), dados2$ValorDificuldade)$estimate^2
AIC(TmLl1)
jpeg(filename = "Tplot16.jpg", width = 480, height = 480)
F1(TmLl1, main="TmLl1", ylim=c(0,100))
dev.off()

### Taxa log - quadrado ###

TmLl2<- glmer(ValorDificuldade ~ taxaLog + taxaLog2 +
		  (1|NomeGame),family=Gamma(link="log"), data=dados2)

TmLl3<- glmer(ValorDificuldade ~ taxaLog + taxaLog2 +
		  (taxaLog|NomeGame),family=Gamma(link="log"), data=dados2)

TmLl4<- glmer(ValorDificuldade ~ taxaLog + taxaLog2 +
		  (taxaLog2|NomeGame),family=Gamma(link="log"), data=dados2)

anova(TmLl3, TmLl2) #TmLl3
anova(TmLl4, TmLl2) #TmLl4

TmLl5<- glmer(ValorDificuldade ~ taxaLog + taxaLog2 +
		  (taxaLog + taxaLog2|NomeGame),family=Gamma(link="log"), data=dados2)

anova(TmLl5, TmLl3) #TmLl3
anova(TmLl5, TmLl4) #TmLl4

AIC(TmLl3, TmLl4)

summary(TmLl3)
cor.test(predict(TmLl3, type="response"), dados2$ValorDificuldade)$estimate^2
AIC(TmLl3)
jpeg(filename = "Tplot17.jpg", width = 480, height = 480)
F1(TmLl3, main="TmLl3", ylim=c(0,100))
dev.off()

### ================================================================== ###
### ============================= SQRT =============================== ###
### ================================================================== ###

### =========================
### Taxa normal 
### =========================

### Taxa normal  - Taxa ###

TmSn0<- glmer(ValorDificuldade ~ taxa +
		  (1|NomeGame),family=Gamma(link="sqrt"), data=dados2)

TmSn1<- glmer(ValorDificuldade ~ taxa +
		  (taxa|NomeGame),family=Gamma(link="sqrt"), data=dados2)

anova(TmSn1, TmSn0) #TmSn1

summary(TmSn1)
cor.test(predict(TmSn1, type="response"), dados2$ValorDificuldade)$estimate^2
AIC(TmSn1)
jpeg(filename = "Tplot19.jpg", width = 480, height = 480)
F1(TmSn1, main="TmSn1", ylim=c(0,100))
dev.off()

### Taxa normal - quadrado ###

TmSn2<- glmer(ValorDificuldade ~ taxa + taxa2 +
		  (1|NomeGame),family=Gamma(link="sqrt"), data=dados2)

TmSn3<- glmer(ValorDificuldade ~ taxa + taxa2 +
		  (taxa|NomeGame),family=Gamma(link="sqrt"), data=dados2)

TmSn4<- glmer(ValorDificuldade ~ taxa + taxa2 +
		  (taxa2|NomeGame),family=Gamma(link="sqrt"), data=dados2)

anova(TmSn3, TmSn2) #TmSn3
anova(TmSn4, TmSn2) #TmSn4

TmSn5<- glmer(ValorDificuldade ~ taxa + taxa2 +
		  (taxa + taxa2|NomeGame),family=Gamma(link="sqrt"), data=dados2)

anova(TmSn5, TmSn3) #TmSn3
anova(TmSn5, TmSn4) #TmSn4

AIC(TmSn3, TmSn4)

summary(TmSn4)
cor.test(predict(TmSn4, type="response"), dados2$ValorDificuldade)$estimate^2
AIC(TmSn4)
jpeg(filename = "Tplot20.jpg", width = 480, height = 480)
F1(TmSn4, main="TmSn4", ylim=c(0,100))
dev.off()

### ===================
### Taxa log
### ===================

### Taxa log  - Taxa ###

mSl0<- glmer(ValorDificuldade ~ taxaLog +
		  (1|NomeGame),family=Gamma(link="sqrt"), data=dados2)

mSl1<- glmer(ValorDificuldade ~ taxaLog +
		  (taxaLog|NomeGame),family=Gamma(link="sqrt"), data=dados2)

anova(mSl1, mSl0) #mSl1

summary(mSl1)
cor.test(predict(mSl1, type="response"), dados2$ValorDificuldade)$estimate^2
AIC(mSl1)
jpeg(filename = "Tplot25.jpg", width = 480, height = 480)
F1(mSl1, main="mSl1", ylim=c(0,100))
dev.off()

### Taxa log - quadrado ###

mSl2<- glmer(ValorDificuldade ~ taxaLog + taxaLog2 +
		  (1|NomeGame),family=Gamma(link="sqrt"), data=dados2)

mSl3<- glmer(ValorDificuldade ~ taxaLog + taxaLog2 +
		  (taxaLog|NomeGame),family=Gamma(link="sqrt"), data=dados2)

mSl4<- glmer(ValorDificuldade ~ taxaLog + taxaLog2 +
		  (taxaLog2|NomeGame),family=Gamma(link="sqrt"), data=dados2)

anova(mSl3, mSl2) #mSl3
anova(mSl4, mSl2) #mSl4

mSl5<- glmer(ValorDificuldade ~ taxaLog + taxaLog2 +
		  (taxaLog + taxaLog2|NomeGame),family=Gamma(link="sqrt"), data=dados2)

anova(mSl5, mSl3) #mSl3
anova(mSl5, mSl4) #mSl4

AIC(mSl3, mSl4)

summary(mSl4)
cor.test(predict(mSl4, type="response"), dados2$ValorDificuldade)$estimate^2
AIC(mSl4)
jpeg(filename = "Tplot26.jpg", width = 480, height = 480)
F1(mSl4, main="mSl4", ylim=c(0,100))
dev.off()

cor.test(dados$Performance, dados$taxa)

##############################################################


summary(TmIn1)
summary(TmIn3)
summary(TmIl1)
summary(TmIl3)

summary(TmLn1)
summary(TmLn3)
summary(TmLl1)
summary(TmLl3)

summary(TmSn1)
summary(TmSn4)
summary(mSl1)
summary(mSl4)

predict(TmSn1, type="response")
(predict(TmSn1, type="link"))^2


cbind(ranef(TmIn1)$NomeGame
,ranef(TmIn3)$NomeGame
,ranef(TmIl1)$NomeGame
,ranef(TmIl3)$NomeGame)

cbind(ranef(TmLn1)$NomeGame
,ranef(TmLn3)$NomeGame
,ranef(TmLl1)$NomeGame
,ranef(TmLl3)$NomeGame)

cbind(ranef(TmSn1)$NomeGame
,ranef(TmSn4)$NomeGame
,ranef(mSl1)$NomeGame
,ranef(mSl4)$NomeGame)






 



