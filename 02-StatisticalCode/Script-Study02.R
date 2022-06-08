

ValorDificuldade 
LevelID
Performance

cor.test(dados2$ValorDificuldade, dados2$LevelID)
cor.test(dados2$ValorDificuldade, dados2$Performance)

head(dados2)

dados2$LevelID2<- ifelse(dados2$LevelID<4, 1, ifelse(dados2$LevelID<7, 2, 
ifelse(dados2$LevelID<10, 3, ifelse(dados2$LevelID<13, 4, ifelse(dados2$LevelID<16, 5, 
ifelse(dados2$LevelID<19, 6, ifelse(dados2$LevelID<22, 7, ifelse(dados2$LevelID<25, 8, 
ifelse(dados2$LevelID<28, 9, ifelse(dados2$LevelID<31, 10, ifelse(dados2$LevelID<34, 11, 
ifelse(dados2$LevelID<37, 12, ifelse(dados2$LevelID<40, 13, ifelse(dados2$LevelID<43, 14, 
ifelse(dados2$LevelID<46, 15, 16)))))))))))))))

m1<- glmer(ValorDificuldade ~ taxa +
		  (taxa|NomeGame) + (1|LevelID),family=Gamma(link="identity"), data=dados2)
summary(m1)
ranef(m1)

cor.test(predict(m1, type="response"), dados2$ValorDificuldade)$estimate^2

#################

TmIn1<- glmer(ValorDificuldade ~ taxa +
		  (taxa|NomeGame) + (1|LevelID),family=Gamma(link="identity"), data=dados2)
summary(TmIn1)
cor.test(predict(TmIn1, type="response"), dados2$ValorDificuldade)$estimate^2
AIC(TmIn1)

TmIn3<- glmer(ValorDificuldade ~ taxa + taxa2 +
		  (taxa|NomeGame) + (1|LevelID),family=Gamma(link="identity"), data=dados2)
summary(TmIn3)
cor.test(predict(TmIn3, type="response"), dados2$ValorDificuldade)$estimate^2
AIC(TmIn3)

TmIl1<- glmer(ValorDificuldade ~ taxaLog +
		  (taxaLog|NomeGame) + (1|LevelID),family=Gamma(link="identity"), data=dados2)
summary(TmIl1)
cor.test(predict(TmIl1, type="response"), dados2$ValorDificuldade)$estimate^2
AIC(TmIl1)

TmIl3<- glmer(ValorDificuldade ~ taxaLog + taxaLog2 +
		  (taxaLog|NomeGame) + (1|LevelID),family=Gamma(link="identity"), data=dados2)
summary(TmIl3)
cor.test(predict(TmIl3, type="response"), dados2$ValorDificuldade)$estimate^2
AIC(TmIl3)

TmLn1<- glmer(ValorDificuldade ~ taxa +
		  (1|NomeGame) + (1|LevelID),family=Gamma(link="log"), data=dados2)
summary(TmLn1)
cor.test(predict(TmLn1, type="response"), dados2$ValorDificuldade)$estimate^2
AIC(TmLn1)

TmLn3<- glmer(ValorDificuldade ~ taxa + taxa2 +
		  (1|NomeGame) + (1|LevelID),family=Gamma(link="log"), data=dados2)
summary(TmLn3)

cor.test(predict(TmLn3, type="response"), dados2$ValorDificuldade)$estimate^2
AIC(TmLn3)

TmLl1<- glmer(ValorDificuldade ~ taxaLog +
		  (1|NomeGame) + (1|LevelID2),family=Gamma(link="log"), data=dados2)
summary(TmLl1)
ranef(TmLl1)
cor.test(predict(TmLl1, type="response"), dados2$ValorDificuldade)$estimate^2
AIC(TmLl1)

TmLl3<- glmer(ValorDificuldade ~ taxaLog + taxaLog2 +
		  (taxaLog|NomeGame) + (1|LevelID),family=Gamma(link="log"), data=dados2)
summary(TmLl3)
cor.test(predict(TmLl3, type="response"), dados2$ValorDificuldade)$estimate^2
AIC(TmLl3)

TmSn1<- glmer(ValorDificuldade ~ taxa +
		  (taxa|NomeGame) + (1|LevelID),family=Gamma(link="sqrt"), data=dados2)
summary(TmSn1)
cor.test(predict(TmSn1, type="response"), dados2$ValorDificuldade)$estimate^2
AIC(TmSn1)

TmSn3<- glmer(ValorDificuldade ~ taxa + taxa2 +
		  (taxa|NomeGame) + (1|LevelID),family=Gamma(link="sqrt"), data=dados2)
summary(TmSn3)
cor.test(predict(TmSn3, type="response"), dados2$ValorDificuldade)$estimate^2
AIC(TmSn3)

mSl1<- glmer(ValorDificuldade ~ taxaLog +
		  (taxaLog|NomeGame) + (1|LevelID),family=Gamma(link="sqrt"), data=dados2)
summary(mSl1)
cor.test(predict(mSl1, type="response"), dados2$ValorDificuldade)$estimate^2
AIC(mSl1)

mSl3<- glmer(ValorDificuldade ~ taxaLog + taxaLog2 +
		  (taxaLog|NomeGame) + (1|LevelID),family=Gamma(link="sqrt"), data=dados2)
summary(mSl3)
cor.test(predict(mSl3, type="response"), dados2$ValorDificuldade)$estimate^2
AIC(mSl3)


rbind(summary(TmIn1)$coefficients
,summary(TmIn3)$coefficients
,summary(TmIl1)$coefficients
,summary(TmIl3)$coefficients
,summary(TmLn1)$coefficients
,summary(TmLn3)$coefficients
,summary(TmLl1)$coefficients
,summary(TmLl3)$coefficients
,summary(TmSn1)$coefficients
,summary(TmSn3)$coefficients
,summary(mSl1)$coefficients
,summary(mSl3)$coefficients)

rbind(cor.test(predict(TmIn1, type="response"), dados2$ValorDificuldade)$estimate^2
,cor.test(predict(TmIn3, type="response"), dados2$ValorDificuldade)$estimate^2
,cor.test(predict(TmIl1, type="response"), dados2$ValorDificuldade)$estimate^2
,cor.test(predict(TmIl3, type="response"), dados2$ValorDificuldade)$estimate^2
,cor.test(predict(TmLn1, type="response"), dados2$ValorDificuldade)$estimate^2
,cor.test(predict(TmLn3, type="response"), dados2$ValorDificuldade)$estimate^2
,cor.test(predict(TmLl1, type="response"), dados2$ValorDificuldade)$estimate^2
,cor.test(predict(TmLl3, type="response"), dados2$ValorDificuldade)$estimate^2
,cor.test(predict(TmSn1, type="response"), dados2$ValorDificuldade)$estimate^2
,cor.test(predict(TmSn3, type="response"), dados2$ValorDificuldade)$estimate^2
,cor.test(predict(mSl1, type="response"), dados2$ValorDificuldade)$estimate^2
,cor.test(predict(mSl3, type="response"), dados2$ValorDificuldade)$estimate^2)

rbind(AIC(TmIn1)
,AIC(TmIn3)
,AIC(TmIl1)
,AIC(TmIl3)
,AIC(TmLn1)
,AIC(TmLn3)
,AIC(TmLl1)
,AIC(TmLl3)
,AIC(TmSn1)
,AIC(TmSn3)
,AIC(mSl1)
,AIC(mSl3))

summary(TmIn1)$varcor
summary(TmIn3)$varcor
summary(TmIl1)$varcor
summary(TmIl3)$varcor
summary(TmLn1)$varcor
summary(TmLn3)$varcor
summary(TmLl1)$varcor
summary(TmLl3)$varcor
summary(TmSn1)$varcor
summary(TmSn3)$varcor
summary(mSl1)$varcor
summary(mSl3)$varcor


cbind(ranef(TmIn1)$NomeGame
,ranef(TmIn3)$NomeGame
,ranef(TmIl1)$NomeGame
,ranef(TmIl3)$NomeGame)

cbind(ranef(TmLn1)$NomeGame
,ranef(TmLn3)$NomeGame
,ranef(TmLl1)$NomeGame
,ranef(TmLl3)$NomeGame)

cbind(ranef(TmSn1)$NomeGame
,ranef(TmSn3)$NomeGame
,ranef(mSl1)$NomeGame
,ranef(mSl3)$NomeGame)


cbind(ranef(TmIn1)$LevelID
,ranef(TmIn3)$LevelID
,ranef(TmIl1)$LevelID
,ranef(TmIl3)$LevelID)

cbind(ranef(TmLn1)$LevelID
,ranef(TmLn3)$LevelID
,ranef(TmLl1)$LevelID
,ranef(TmLl3)$LevelID)

cbind(ranef(TmSn1)$LevelID
,ranef(TmSn3)$LevelID
,ranef(mSl1)$LevelID
,ranef(mSl3)$LevelID)



dados2$pred<- predict(TmLl1, type="response")
dt<- subset(dados2, dados2$LevelID==2)

dx1<- subset(dt, dt$NomeGame=="arteGalacticaData")
dx2<- subset(dt, dt$NomeGame=="decifrandoCodigosData")
dx3<- subset(dt, dt$NomeGame=="desafioDosOpostosData")
dx4<- subset(dt, dt$NomeGame=="exploradorData")
dx5<- subset(dt, dt$NomeGame=="laboratorioData")
dx6<- subset(dt, dt$NomeGame=="pulandoAsteroidesData")
dx7<- subset(dt, dt$NomeGame=="tunelAceleradorData")

plot(dx1$pred ~ dx1$taxa, type="l", ylim=c(0,150), xlim=c(0,1))
lines(dx2$pred ~ dx2$taxa, type="l")
lines(dx3$pred ~ dx3$taxa, type="l")
lines(dx4$pred ~ dx4$taxa, type="l")
lines(dx5$pred ~ dx5$taxa, type="l")
lines(dx6$pred ~ dx6$taxa, type="l")
lines(dx7$pred ~ dx7$taxa, type="l")


level<- c(rep(unique(dados2$LevelID2)[1], 707), rep(unique(dados2$LevelID2)[2], 707),
rep(unique(dados2$LevelID2)[3], 707), rep(unique(dados2$LevelID2)[4], 707),
rep(unique(dados2$LevelID2)[5], 707), rep(unique(dados2$LevelID2)[6], 707),
rep(unique(dados2$LevelID2)[7], 707), rep(unique(dados2$LevelID2)[8], 707),
rep(unique(dados2$LevelID2)[9], 707), rep(unique(dados2$LevelID2)[10], 707),
rep(unique(dados2$LevelID2)[11], 707), rep(unique(dados2$LevelID2)[12], 707),
rep(unique(dados2$LevelID2)[13], 707), rep(unique(dados2$LevelID2)[14], 707),
rep(unique(dados2$LevelID2)[15], 707), rep(unique(dados2$LevelID2)[16], 707))

NomeGame1<- rep(rep(unique(dados$NomeGame), rep(101,7)), 48)
Taxaaux<- rep(rep((seq(0, 1, 0.01)),7), 48)
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

Newdata<- data.frame(level, NomeGame1, Taxaaux, TaxaauxInv, TaxaauxLog,
Taxaaux2, TaxaauxInv2, TaxaauxLog2, TaxaauxMa, TaxaauxMe,
TaxaauxInvMa, TaxaauxInvMe, TaxaauxLogMa, TaxaauxLogMe)

colnames(Newdata)<- c("LevelID2", "NomeGame", "taxa", "taxaInv", "taxaLog", 
"taxa2", "taxaInv2", "taxaLog2", "taxaMa", "taxaMe",
"taxaInvMa", "taxaInvMe", "taxaLogMa", "taxaLogMe")

setwd("Y:/00 - Consultoria/TC553 - João Batista Mossmann/Gráficos")

modelo=TmLl1
Newdata$pred<- predict(modelo, newdata=Newdata, type="response")
dlevel<- subset(Newdata, Newdata$LevelID2==8)

Newdata1<- subset(dlevel, dlevel$NomeGame=="arteGalacticaData")
Newdata2<- subset(dlevel, dlevel$NomeGame=="decifrandoCodigosData")
Newdata3<- subset(dlevel, dlevel$NomeGame=="desafioDosOpostosData")
Newdata4<- subset(dlevel, dlevel$NomeGame=="exploradorData")
Newdata5<- subset(dlevel, dlevel$NomeGame=="laboratorioData")
Newdata6<- subset(dlevel, dlevel$NomeGame=="pulandoAsteroidesData")
Newdata7<- subset(dlevel, dlevel$NomeGame=="tunelAceleradorData")

svg(filename = "g1.svg", width = 9, height = 7)
plot(Newdata1$pred ~ Newdata1$taxa, main="", type="l", ylim=c(0,200), ylab="Dificuldade", xlab="Performance", lwd=2, col="#d9f0a3")
lines(Newdata2$pred ~ Newdata2$taxa, type="l", col="#addd8e", lwd=2)
lines(Newdata3$pred ~ Newdata3$taxa, type="l", col="#7fcdbb", lwd=2)
lines(Newdata4$pred ~ Newdata4$taxa, type="l", col="#41ab5d", lwd=2)
lines(Newdata5$pred ~ Newdata5$taxa, type="l", col="#238443", lwd=2)
lines(Newdata6$pred ~ Newdata6$taxa, type="l", col="#253494", lwd=2)
lines(Newdata7$pred ~ Newdata7$taxa, type="l", col="#1d91c0", lwd=2)

legend("topleft", c("Arte Galáctica", "Decifrando Códigos", "Desafio dos Opostos"
,"Explorador", "Laboratório", "Pulando Asteroides", "Túnel Acelerador"), lty=1,
col=c("#d9f0a3","#addd8e", "#7fcdbb", "#41ab5d", "#238443", "#253494", "#1d91c0"),
lwd=2, bty="n", cex=0.8)
dev.off()

### Arte Galáctica ###

dj1<- subset(Newdata, Newdata$NomeGame=="arteGalacticaData")

dl1<- subset(dj1, dj1$LevelID2==1)
dl2<- subset(dj1, dj1$LevelID2==2)
dl3<- subset(dj1, dj1$LevelID2==3)
dl4<- subset(dj1, dj1$LevelID2==4)
dl5<- subset(dj1, dj1$LevelID2==5)
dl6<- subset(dj1, dj1$LevelID2==6)
dl7<- subset(dj1, dj1$LevelID2==7)
dl8<- subset(dj1, dj1$LevelID2==8)
dl9<- subset(dj1, dj1$LevelID2==9)
dl10<- subset(dj1, dj1$LevelID2==10)
dl11<- subset(dj1, dj1$LevelID2==11)
dl12<- subset(dj1, dj1$LevelID2==12)
dl13<- subset(dj1, dj1$LevelID2==13)
dl14<- subset(dj1, dj1$LevelID2==14)
dl15<- subset(dj1, dj1$LevelID2==15)
dl16<- subset(dj1, dj1$LevelID2==16)

svg(filename = "g2.svg", width = 9, height = 7)
plot(dl1$pred ~ dl1$taxa, type="l", main="Arte Galáctica", ylim=c(0,220), ylab="Dificuldade", xlab="Performance", lwd=2, col="gray90")
lines(dl2$pred ~ dl2$taxa, type="l", col="gray84", lwd=2)
lines(dl3$pred ~ dl3$taxa, type="l", col="gray78", lwd=2)
lines(dl4$pred ~ dl4$taxa, type="l", col="gray72", lwd=2)
lines(dl5$pred ~ dl5$taxa, type="l", col="gray66", lwd=2)
lines(dl6$pred ~ dl6$taxa, type="l", col="gray60", lwd=2)
lines(dl7$pred ~ dl7$taxa, type="l", col="gray54", lwd=2)
lines(dl8$pred ~ dl8$taxa, type="l", col="gray48", lwd=2)
lines(dl9$pred ~ dl9$taxa, type="l", col="gray42", lwd=2)
lines(dl10$pred ~ dl10$taxa, type="l", col="gray36", lwd=2)
lines(dl11$pred ~ dl11$taxa, type="l", col="gray30", lwd=2)
lines(dl12$pred ~ dl12$taxa, type="l", col="gray24", lwd=2)
lines(dl13$pred ~ dl13$taxa, type="l", col="gray18", lwd=2)
lines(dl14$pred ~ dl14$taxa, type="l", col="gray12", lwd=2)
lines(dl15$pred ~ dl15$taxa, type="l", col="gray6", lwd=2)
lines(dl16$pred ~ dl16$taxa, type="l", col="gray0", lwd=2)

legend("bottomleft", c("1, 2, 3", "4, 5, 6", "7, 8, 9", "10, 11, 12", "13, 14, 15", "16, 17, 18", "19, 20, 21", "22, 23, 24", "25, 26, 27", "28, 29, 30", "31, 32, 33", "34, 35, 36", "37, 38, 39", "40, 41, 42", "43, 44, 45", "46, 47, 48"), lty=1,
ncol=5, col=c("gray90", "gray84", "gray78", "gray72", "gray66", "gray60", "gray54", "gray48", "gray42", "gray36", "gray30", "gray24", "gray18", "gray12", "gray6", "gray0"),
lwd=2, bty="n", cex=0.8)
dev.off()

### Decifrando Códigos ###

dj2<- subset(Newdata, Newdata$NomeGame=="decifrandoCodigosData")

dl11<- subset(dj2, dj2$LevelID2==1)
dl12<- subset(dj2, dj2$LevelID2==2)
dl13<- subset(dj2, dj2$LevelID2==3)
dl14<- subset(dj2, dj2$LevelID2==4)
dl15<- subset(dj2, dj2$LevelID2==5)
dl16<- subset(dj2, dj2$LevelID2==6)
dl17<- subset(dj2, dj2$LevelID2==7)
dl18<- subset(dj2, dj2$LevelID2==8)
dl19<- subset(dj2, dj2$LevelID2==9)
dl110<- subset(dj2, dj2$LevelID2==10)
dl111<- subset(dj2, dj2$LevelID2==11)
dl112<- subset(dj2, dj2$LevelID2==12)
dl113<- subset(dj2, dj2$LevelID2==13)
dl114<- subset(dj2, dj2$LevelID2==14)
dl115<- subset(dj2, dj2$LevelID2==15)
dl116<- subset(dj2, dj2$LevelID2==16)

svg(filename = "g3.svg", width = 9, height = 7)
plot(dl11$pred ~ dl11$taxa, type="l", main="Decifrando Códigos", ylim=c(0,110), ylab="Dificuldade", xlab="Performance", lwd=2, col="gray90")
lines(dl12$pred ~ dl12$taxa, type="l", col="gray84", lwd=2)
lines(dl13$pred ~ dl13$taxa, type="l", col="gray78", lwd=2)
lines(dl14$pred ~ dl14$taxa, type="l", col="gray72", lwd=2)
lines(dl15$pred ~ dl15$taxa, type="l", col="gray66", lwd=2)
lines(dl16$pred ~ dl16$taxa, type="l", col="gray60", lwd=2)
lines(dl17$pred ~ dl17$taxa, type="l", col="gray54", lwd=2)
lines(dl18$pred ~ dl18$taxa, type="l", col="gray48", lwd=2)
lines(dl19$pred ~ dl19$taxa, type="l", col="gray42", lwd=2)
lines(dl110$pred ~ dl110$taxa, type="l", col="gray36", lwd=2)
lines(dl111$pred ~ dl111$taxa, type="l", col="gray30", lwd=2)
lines(dl112$pred ~ dl112$taxa, type="l", col="gray24", lwd=2)
lines(dl113$pred ~ dl113$taxa, type="l", col="gray18", lwd=2)
lines(dl114$pred ~ dl114$taxa, type="l", col="gray12", lwd=2)
lines(dl115$pred ~ dl115$taxa, type="l", col="gray6", lwd=2)
lines(dl116$pred ~ dl116$taxa, type="l", col="gray0", lwd=2)

legend("bottomleft", c("1, 2, 3", "4, 5, 6", "7, 8, 9", "10, 11, 12", "13, 14, 15", "16, 17, 18", "19, 20, 21", "22, 23, 24", "25, 26, 27", "28, 29, 30", "31, 32, 33", "34, 35, 36", "37, 38, 39", "40, 41, 42", "43, 44, 45", "46, 47, 48"), lty=1,
ncol=5, col=c("gray90", "gray84", "gray78", "gray72", "gray66", "gray60", "gray54", "gray48", "gray42", "gray36", "gray30", "gray24", "gray18", "gray12", "gray6", "gray0"),
lwd=2, bty="n", cex=0.8)
dev.off()

### Desafio dos Opostos ###

dj3<- subset(Newdata, Newdata$NomeGame=="desafioDosOpostosData")

dl21<- subset(dj3, dj3$LevelID2==1)
dl22<- subset(dj3, dj3$LevelID2==2)
dl23<- subset(dj3, dj3$LevelID2==3)
dl24<- subset(dj3, dj3$LevelID2==4)
dl25<- subset(dj3, dj3$LevelID2==5)
dl26<- subset(dj3, dj3$LevelID2==6)
dl27<- subset(dj3, dj3$LevelID2==7)
dl28<- subset(dj3, dj3$LevelID2==8)
dl29<- subset(dj3, dj3$LevelID2==9)
dl210<- subset(dj3, dj3$LevelID2==10)
dl211<- subset(dj3, dj3$LevelID2==11)
dl212<- subset(dj3, dj3$LevelID2==12)
dl213<- subset(dj3, dj3$LevelID2==13)
dl214<- subset(dj3, dj3$LevelID2==14)
dl215<- subset(dj3, dj3$LevelID2==15)
dl216<- subset(dj3, dj3$LevelID2==16)

svg(filename = "g4.svg", width = 9, height = 7)
plot(dl21$pred ~ dl21$taxa, type="l", main="Desafio dos Opostos", ylim=c(0,50), ylab="Dificuldade", xlab="Performance", lwd=2, col="gray90")
lines(dl22$pred ~ dl22$taxa, type="l", col="gray84", lwd=2)
lines(dl23$pred ~ dl23$taxa, type="l", col="gray78", lwd=2)
lines(dl24$pred ~ dl24$taxa, type="l", col="gray72", lwd=2)
lines(dl25$pred ~ dl25$taxa, type="l", col="gray66", lwd=2)
lines(dl26$pred ~ dl26$taxa, type="l", col="gray60", lwd=2)
lines(dl27$pred ~ dl27$taxa, type="l", col="gray54", lwd=2)
lines(dl28$pred ~ dl28$taxa, type="l", col="gray48", lwd=2)
lines(dl29$pred ~ dl29$taxa, type="l", col="gray42", lwd=2)
lines(dl210$pred ~ dl210$taxa, type="l", col="gray36", lwd=2)
lines(dl211$pred ~ dl211$taxa, type="l", col="gray30", lwd=2)
lines(dl212$pred ~ dl212$taxa, type="l", col="gray24", lwd=2)
lines(dl213$pred ~ dl213$taxa, type="l", col="gray18", lwd=2)
lines(dl214$pred ~ dl214$taxa, type="l", col="gray12", lwd=2)
lines(dl215$pred ~ dl215$taxa, type="l", col="gray6", lwd=2)
lines(dl216$pred ~ dl216$taxa, type="l", col="gray0", lwd=2)

legend("bottomleft", c("1, 2, 3", "4, 5, 6", "7, 8, 9", "10, 11, 12", "13, 14, 15", "16, 17, 18", "19, 20, 21", "22, 23, 24", "25, 26, 27", "28, 29, 30", "31, 32, 33", "34, 35, 36", "37, 38, 39", "40, 41, 42", "43, 44, 45", "46, 47, 48"), lty=1,
ncol=5, col=c("gray90", "gray84", "gray78", "gray72", "gray66", "gray60", "gray54", "gray48", "gray42", "gray36", "gray30", "gray24", "gray18", "gray12", "gray6", "gray0"),
lwd=2, bty="n", cex=0.8)
dev.off()

### Explorador ###

dj4<- subset(Newdata, Newdata$NomeGame=="exploradorData")

dl31<- subset(dj4, dj4$LevelID2==1)
dl32<- subset(dj4, dj4$LevelID2==2)
dl33<- subset(dj4, dj4$LevelID2==3)
dl34<- subset(dj4, dj4$LevelID2==4)
dl35<- subset(dj4, dj4$LevelID2==5)
dl36<- subset(dj4, dj4$LevelID2==6)
dl37<- subset(dj4, dj4$LevelID2==7)
dl38<- subset(dj4, dj4$LevelID2==8)
dl39<- subset(dj4, dj4$LevelID2==9)
dl310<- subset(dj4, dj4$LevelID2==10)
dl311<- subset(dj4, dj4$LevelID2==11)
dl312<- subset(dj4, dj4$LevelID2==12)
dl313<- subset(dj4, dj4$LevelID2==13)
dl314<- subset(dj4, dj4$LevelID2==14)
dl315<- subset(dj4, dj4$LevelID2==15)
dl316<- subset(dj4, dj4$LevelID2==16)

svg(filename = "g5.svg", width = 9, height = 7)
plot(dl31$pred ~ dl31$taxa, type="l", main="Explorador", ylim=c(0,90), ylab="Dificuldade", xlab="Performance", lwd=2, col="gray90")
lines(dl32$pred ~ dl32$taxa, type="l", col="gray84", lwd=2)
lines(dl33$pred ~ dl33$taxa, type="l", col="gray78", lwd=2)
lines(dl34$pred ~ dl34$taxa, type="l", col="gray72", lwd=2)
lines(dl35$pred ~ dl35$taxa, type="l", col="gray66", lwd=2)
lines(dl36$pred ~ dl36$taxa, type="l", col="gray60", lwd=2)
lines(dl37$pred ~ dl37$taxa, type="l", col="gray54", lwd=2)
lines(dl38$pred ~ dl38$taxa, type="l", col="gray48", lwd=2)
lines(dl39$pred ~ dl39$taxa, type="l", col="gray42", lwd=2)
lines(dl310$pred ~ dl310$taxa, type="l", col="gray36", lwd=2)
lines(dl311$pred ~ dl311$taxa, type="l", col="gray30", lwd=2)
lines(dl312$pred ~ dl312$taxa, type="l", col="gray24", lwd=2)
lines(dl313$pred ~ dl313$taxa, type="l", col="gray18", lwd=2)
lines(dl314$pred ~ dl314$taxa, type="l", col="gray12", lwd=2)
lines(dl315$pred ~ dl315$taxa, type="l", col="gray6", lwd=2)
lines(dl316$pred ~ dl316$taxa, type="l", col="gray0", lwd=2)

legend("bottomleft", c("1, 2, 3", "4, 5, 6", "7, 8, 9", "10, 11, 12", "13, 14, 15", "16, 17, 18", "19, 20, 21", "22, 23, 24", "25, 26, 27", "28, 29, 30", "31, 32, 33", "34, 35, 36", "37, 38, 39", "40, 41, 42", "43, 44, 45", "46, 47, 48"), lty=1,
ncol=5, col=c("gray90", "gray84", "gray78", "gray72", "gray66", "gray60", "gray54", "gray48", "gray42", "gray36", "gray30", "gray24", "gray18", "gray12", "gray6", "gray0"),
lwd=2, bty="n", cex=0.8)
dev.off()

### Laboratório ###

dj5<- subset(Newdata, Newdata$NomeGame=="laboratorioData")

dl41<- subset(dj5, dj5$LevelID2==1)
dl42<- subset(dj5, dj5$LevelID2==2)
dl43<- subset(dj5, dj5$LevelID2==3)
dl44<- subset(dj5, dj5$LevelID2==4)
dl45<- subset(dj5, dj5$LevelID2==5)
dl46<- subset(dj5, dj5$LevelID2==6)
dl47<- subset(dj5, dj5$LevelID2==7)
dl48<- subset(dj5, dj5$LevelID2==8)
dl49<- subset(dj5, dj5$LevelID2==9)
dl410<- subset(dj5, dj5$LevelID2==10)
dl411<- subset(dj5, dj5$LevelID2==11)
dl412<- subset(dj5, dj5$LevelID2==12)
dl413<- subset(dj5, dj5$LevelID2==13)
dl414<- subset(dj5, dj5$LevelID2==14)
dl415<- subset(dj5, dj5$LevelID2==15)
dl416<- subset(dj5, dj5$LevelID2==16)

svg(filename = "g6.svg", width = 9, height = 7)
plot(dl41$pred ~ dl41$taxa, type="l", main="Laboratório", ylim=c(0,60), ylab="Dificuldade", xlab="Performance", lwd=2, col="gray90")
lines(dl42$pred ~ dl42$taxa, type="l", col="gray84", lwd=2)
lines(dl43$pred ~ dl43$taxa, type="l", col="gray78", lwd=2)
lines(dl44$pred ~ dl44$taxa, type="l", col="gray72", lwd=2)
lines(dl45$pred ~ dl45$taxa, type="l", col="gray66", lwd=2)
lines(dl46$pred ~ dl46$taxa, type="l", col="gray60", lwd=2)
lines(dl47$pred ~ dl47$taxa, type="l", col="gray54", lwd=2)
lines(dl48$pred ~ dl48$taxa, type="l", col="gray48", lwd=2)
lines(dl49$pred ~ dl49$taxa, type="l", col="gray42", lwd=2)
lines(dl410$pred ~ dl410$taxa, type="l", col="gray36", lwd=2)
lines(dl411$pred ~ dl411$taxa, type="l", col="gray30", lwd=2)
lines(dl412$pred ~ dl412$taxa, type="l", col="gray24", lwd=2)
lines(dl413$pred ~ dl413$taxa, type="l", col="gray18", lwd=2)
lines(dl414$pred ~ dl414$taxa, type="l", col="gray12", lwd=2)
lines(dl415$pred ~ dl415$taxa, type="l", col="gray6", lwd=2)
lines(dl416$pred ~ dl416$taxa, type="l", col="gray0", lwd=2)

legend("bottomleft", c("1, 2, 3", "4, 5, 6", "7, 8, 9", "10, 11, 12", "13, 14, 15", "16, 17, 18", "19, 20, 21", "22, 23, 24", "25, 26, 27", "28, 29, 30", "31, 32, 33", "34, 35, 36", "37, 38, 39", "40, 41, 42", "43, 44, 45", "46, 47, 48"), lty=1,
ncol=5, col=c("gray90", "gray84", "gray78", "gray72", "gray66", "gray60", "gray54", "gray48", "gray42", "gray36", "gray30", "gray24", "gray18", "gray12", "gray6", "gray0"),
lwd=2, bty="n", cex=0.8)
dev.off()

### Pulando Asteroides ###

dj6<- subset(Newdata, Newdata$NomeGame=="pulandoAsteroidesData")

dl51<- subset(dj6, dj6$LevelID2==1)
dl52<- subset(dj6, dj6$LevelID2==2)
dl53<- subset(dj6, dj6$LevelID2==3)
dl54<- subset(dj6, dj6$LevelID2==4)
dl55<- subset(dj6, dj6$LevelID2==5)
dl56<- subset(dj6, dj6$LevelID2==6)
dl57<- subset(dj6, dj6$LevelID2==7)
dl58<- subset(dj6, dj6$LevelID2==8)
dl59<- subset(dj6, dj6$LevelID2==9)
dl510<- subset(dj6, dj6$LevelID2==10)
dl511<- subset(dj6, dj6$LevelID2==11)
dl512<- subset(dj6, dj6$LevelID2==12)
dl513<- subset(dj6, dj6$LevelID2==13)
dl514<- subset(dj6, dj6$LevelID2==14)
dl515<- subset(dj6, dj6$LevelID2==15)
dl516<- subset(dj6, dj6$LevelID2==16)

svg(filename = "g7.svg", width = 9, height = 7)
plot(dl51$pred ~ dl51$taxa, type="l", main="Pulando Asteroides", ylim=c(0,110), ylab="Dificuldade", xlab="Performance", lwd=2, col="gray90")
lines(dl52$pred ~ dl52$taxa, type="l", col="gray84", lwd=2)
lines(dl53$pred ~ dl53$taxa, type="l", col="gray78", lwd=2)
lines(dl54$pred ~ dl54$taxa, type="l", col="gray72", lwd=2)
lines(dl55$pred ~ dl55$taxa, type="l", col="gray66", lwd=2)
lines(dl56$pred ~ dl56$taxa, type="l", col="gray60", lwd=2)
lines(dl57$pred ~ dl57$taxa, type="l", col="gray54", lwd=2)
lines(dl58$pred ~ dl58$taxa, type="l", col="gray48", lwd=2)
lines(dl59$pred ~ dl59$taxa, type="l", col="gray42", lwd=2)
lines(dl510$pred ~ dl510$taxa, type="l", col="gray36", lwd=2)
lines(dl511$pred ~ dl511$taxa, type="l", col="gray30", lwd=2)
lines(dl512$pred ~ dl512$taxa, type="l", col="gray24", lwd=2)
lines(dl513$pred ~ dl513$taxa, type="l", col="gray18", lwd=2)
lines(dl514$pred ~ dl514$taxa, type="l", col="gray12", lwd=2)
lines(dl515$pred ~ dl515$taxa, type="l", col="gray6", lwd=2)
lines(dl516$pred ~ dl516$taxa, type="l", col="gray0", lwd=2)

legend("bottomleft", c("1, 2, 3", "4, 5, 6", "7, 8, 9", "10, 11, 12", "13, 14, 15", "16, 17, 18", "19, 20, 21", "22, 23, 24", "25, 26, 27", "28, 29, 30", "31, 32, 33", "34, 35, 36", "37, 38, 39", "40, 41, 42", "43, 44, 45", "46, 47, 48"), lty=1,
ncol=5, col=c("gray90", "gray84", "gray78", "gray72", "gray66", "gray60", "gray54", "gray48", "gray42", "gray36", "gray30", "gray24", "gray18", "gray12", "gray6", "gray0"),
lwd=2, bty="n", cex=0.8)
dev.off()

### Túnel Acelerador ###

dj7<- subset(Newdata, Newdata$NomeGame=="tunelAceleradorData")

dl61<- subset(dj7, dj7$LevelID2==1)
dl62<- subset(dj7, dj7$LevelID2==2)
dl63<- subset(dj7, dj7$LevelID2==3)
dl64<- subset(dj7, dj7$LevelID2==4)
dl65<- subset(dj7, dj7$LevelID2==5)
dl66<- subset(dj7, dj7$LevelID2==6)
dl67<- subset(dj7, dj7$LevelID2==7)
dl68<- subset(dj7, dj7$LevelID2==8)
dl69<- subset(dj7, dj7$LevelID2==9)
dl610<- subset(dj7, dj7$LevelID2==10)
dl611<- subset(dj7, dj7$LevelID2==11)
dl612<- subset(dj7, dj7$LevelID2==12)
dl613<- subset(dj7, dj7$LevelID2==13)
dl614<- subset(dj7, dj7$LevelID2==14)
dl615<- subset(dj7, dj7$LevelID2==15)
dl616<- subset(dj7, dj7$LevelID2==16)

svg(filename = "g8.svg", width = 9, height = 7)
plot(dl61$pred ~ dl61$taxa, type="l", main="Túnel Acelerador", ylim=c(0,80), ylab="Dificuldade", xlab="Performance", lwd=2, col="gray90")
lines(dl62$pred ~ dl62$taxa, type="l", col="gray84", lwd=2)
lines(dl63$pred ~ dl63$taxa, type="l", col="gray78", lwd=2)
lines(dl64$pred ~ dl64$taxa, type="l", col="gray72", lwd=2)
lines(dl65$pred ~ dl65$taxa, type="l", col="gray66", lwd=2)
lines(dl66$pred ~ dl66$taxa, type="l", col="gray60", lwd=2)
lines(dl67$pred ~ dl67$taxa, type="l", col="gray54", lwd=2)
lines(dl68$pred ~ dl68$taxa, type="l", col="gray48", lwd=2)
lines(dl69$pred ~ dl69$taxa, type="l", col="gray42", lwd=2)
lines(dl610$pred ~ dl610$taxa, type="l", col="gray36", lwd=2)
lines(dl611$pred ~ dl611$taxa, type="l", col="gray30", lwd=2)
lines(dl612$pred ~ dl612$taxa, type="l", col="gray24", lwd=2)
lines(dl613$pred ~ dl613$taxa, type="l", col="gray18", lwd=2)
lines(dl614$pred ~ dl614$taxa, type="l", col="gray12", lwd=2)
lines(dl615$pred ~ dl615$taxa, type="l", col="gray6", lwd=2)
lines(dl616$pred ~ dl616$taxa, type="l", col="gray0", lwd=2)

legend("bottomleft", c("1, 2, 3", "4, 5, 6", "7, 8, 9", "10, 11, 12", "13, 14, 15", "16, 17, 18", "19, 20, 21", "22, 23, 24", "25, 26, 27", "28, 29, 30", "31, 32, 33", "34, 35, 36", "37, 38, 39", "40, 41, 42", "43, 44, 45", "46, 47, 48"), lty=1,
ncol=5, col=c("gray90", "gray84", "gray78", "gray72", "gray66", "gray60", "gray54", "gray48", "gray42", "gray36", "gray30", "gray24", "gray18", "gray12", "gray6", "gray0"),
lwd=2, bty="n", cex=0.8)
dev.off()



