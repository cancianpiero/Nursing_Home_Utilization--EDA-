setwd("C:/Users/USER/Desktop/DATA ANALYTICS")
WNH<-read.csv(file="WiscNursingHome.csv", header = TRUE)
head(WNH)
str(WNH)
WNH <- WNH[,-1]
WNH <- na.omit(WNH)


WNH$CRYEAR <- factor(WNH$CRYEAR)
WNH$URBAN <- factor(WNH$URBAN)
levels(WNH$URBAN) <- c("rural","urban")
WNH$PRO <- factor(WNH$PRO)
levels(WNH$PRO) <- c("non-profit","profit")
WNH$TAXEXEMPT <- factor(WNH$TAXEXEMPT)
levels(WNH$TAXEXEMPT) <- c("non-tax-exempt", "tax-exempt")
WNH$SELFFUNDINS <- factor(WNH$SELFFUNDINS)
levels(WNH$SELFFUNDINS) <- c("non-self-funded", "self-funded")
WNH$MCERT <- factor(WNH$MCERT)
levels(WNH$MCERT) <- c("non-Medicare certified", "Medicare certified")
WNH$ORGSTR <- factor(WNH$ORGSTR)
levels(WNH$ORGSTR) <- c("profit", "tax-exempt", "governmental unit")
#ospedali privati migliori di quelli governo, costano di piu pero

par(mfrow=c(2,3)) 
barplot(table(WNH$URBAN), main="URBAN", ylim=c(0,400), col=2)
barplot(table(WNH$PRO), main="PRO",ylim=c(0,400), col=3)
barplot(table(WNH$TAXEXEMPT), main="TAXEXEMPT",  ylim=c(0,500), col=4)
barplot(table(WNH$SELFFUNDINS), main="SELFFUNDINS",  ylim=c(0,500), col=5)
barplot(table(WNH$MCERT), main="MCERT", ylim=c(0,700), col=6)
barplot(table(WNH$ORGSTR), main="ORGSTR",  ylim=c(0,400), col=7)
par(mfrow=c(1,1))

table(WNH$CRYEAR)
prop.table(table(WNH$CRYEAR))

table(WNH$URBAN)
prop.table(table(WNH$URBAN))

table(WNH$PRO)
prop.table(table(WNH$PRO))

table(WNH$TAXEXEMPT)
prop.table(table(WNH$TAXEXEMPT))

table(WNH$SELFFUNDINS)   #frequenze assolute
prop.table(table(WNH$SELFFUNDINS))            #tabella confrequenze relative
#table(WNH$SELFFUNDINS) / length(WNH$SELFFUNDINS)      #frequenze relative

table(WNH$MCERT)
prop.table(table(WNH$MCERT))

table(WNH$ORGSTR)
prop.table(table(WNH$ORGSTR))



tab1 <- table(WNH$ORGSTR,WNH$URBAN) 
tab1
ptab1 <- prop.table(tab1,2) #frequenze relative condizionate in relazione alla variabile indipendente
ptab1
barplot(ptab1,legend=T,beside=TRUE, main="URBAN-ORGSTR", ylim=c(0,1), col=(2:4))

tab2 <- table(WNH$PRO,WNH$URBAN)  #indipendenti
tab2
ptab2 <- prop.table(tab2,2) 
ptab2
barplot(ptab2,legend=T,beside=TRUE, main="URBAN-PRO", ylim=c(0,1), col=(3:4))

tab3 <- table(WNH$PRO,WNH$MCERT) 
tab3
ptab3 <- prop.table(tab3,2)
ptab3
barplot(ptab3,legend=T,beside=TRUE, main="MCERT-PRO", ylim=c(0,1), col=(3:4))

tab4 <- table(WNH$SELFFUNDINS,WNH$MCERT) 
tab4
ptab4 <- prop.table(tab4,2)
ptab4
barplot(ptab4,legend=T,beside=TRUE, main="SELFFUNDINS-MCERT", ylim=c(0,1), col=(3:4))




barplot(table(WNH$MSA), main="Metropolitan Statistical Area Code", ylim=c(0,350), col=2:15)


par(mfrow=c(2,3))
boxplot(WNH$TPY, main="Boxplot TPY", col=2)
boxplot(WNH$NUMBED, main="Boxplot NUMBED", col=3)
boxplot(WNH$SQRFOOT, main="Boxplot SQRFOOT", col=4)
hist(WNH$TPY, main="Hist TPY", xlim=c(0,500), ylim=c(0,400), col=2) 
#sull'asse i valori relativi alle frequenze assolute
hist(WNH$NUMBED, main="Hist NUMBED", xlim=c(0,500), ylim=c(0,400), col=3)
hist(WNH$SQRFOOT, main="Hist SQRFOOT", xlim=c(0,300), ylim=c(0,300), col=4)
par(mfrow=c(1,1))


library(moments)#misuro indice di asimmetria:  scarti dalla media elevati al cubo sum((xi ??? M)^3 / n ) / sd^3
skewness(WNH$TPY)   #valori positivi nel caso di asimmetria positiva (coda destra lunga)
skewness(WNH$NUMBED)
skewness(WNH$SQRFOOT)

apply(WNH[, c("TPY","NUMBED", "SQRFOOT")], 2, summary, na.rm=TRUE)  


par(mfrow=c(1,3))
boxplot(log(WNH$TPY), main="Boxplot LOG TPY", col=2, outline=F)
boxplot(log(WNH$SQRFOOT), main="Boxplot LOG NUMBED", col=3, outline=F)
boxplot(log(WNH$NUMBED), main="Boxplot LOG SQRFOOT", col=4, outline=F)
par(mfrow=c(1,1))

#controllo gaussianità delle variabili quantitative 
par(mfrow=c(1,3))
qqnorm(log(WNH$TPY), main="qqnorm TPY")
qqline(log(WNH$TPY),col=2,lwd=2)
qqnorm(log(WNH$NUMBED), main="qqnorm NUMBED")
qqline(log(WNH$NUMBED),col=2,lwd=2)
qqnorm(log(WNH$SQRFOOT), main="qqnorm SQRFOOT")
qqline(log(WNH$SQRFOOT),col=2,lwd=2)
par(mfrow=c(1,1))





#############################################################

#guardo distribuzioni

ggplot(data = WNH, aes(x=URBAN, y=log(NUMBED), fill=URBAN)) + geom_boxplot()
ggplot(data = WNH, aes(x=URBAN, y=log(SQRFOOT), fill=URBAN)) + geom_boxplot()
ggplot(data = WNH, aes(x=PRO, y=log(SQRFOOT), fill=PRO)) + geom_boxplot()
ggplot(data = WNH, aes(x=TAXEXEMPT, y=log(SQRFOOT), fill=TAXEXEMPT)) + geom_boxplot()
ggplot(data = WNH, aes(x=ORGSTR, y=log(SQRFOOT), fill=ORGSTR)) + geom_boxplot()






#regressione         TPY = Y

testanova<-aov(TPY ~ ORGSTR, data=WNH) 
summary(testanova)

cov(WNH$NUMBED, WNH$TPY)
cor(WNH$NUMBED, WNH$TPY)
plot(WNH$NUMBED, WNH$TPY)
qqplot(WNH$NUMBED, WNH$TPY)
abline(0,1,col=2,lwd=2)



cov(WNH$NUMBED,WNH$SQRFOOT)
cor(WNH$NUMBED,WNH$SQRFOOT)
qqplot(WNH$SQRFOOT, WNH$NUMBED)
plot(WNH$NUMBED, WNH$SQRFOOT)
abline(0,1, col=2, lwd=2)

mod <- lm(TPY ~ SQRFOOT , data=WNH)
summary(mod)


mod1 <- lm(TPY ~ NUMBED, data=WNH)
summary(mod1)
plot(mod1)
res <- mod1$residuals
plot(WNH$TAXEXEMPT,res)
plot(WNH$PRO,res)
plot(WNH$MCERT,res)
plot(WNH$CRYEAR,res)
plot(WNH$SQRFOOT,res)
plot(WNH$MSA,res)
plot(WNH$URBAN,res)
plot(WNH$SELFFUNDINS,res)
plot(WNH$ORGSTR,res)

#analisis dei residui
plot(res)    
abline(0,0,col=2)
abline(2,0,col=3)
abline(-2,0,col=3)

##############################################################àà

modP <- lm(TPY ~ NUMBED + CRYEAR, data=WNH)

new_obs = data.frame(NUMBED=120, CRYEAR="2000", SQRFOOT=52, MSA=0)
predict(modP, newdata = new_obs)

new_obs = data.frame(NUMBED=120, CRYEAR="2001", SQRFOOT=52, MSA=0)
predict(modP, newdata = new_obs)


#cluster
setwd("C:/Users/USER/Desktop/DATA ANALYTICS")

library(cluster)
library(GGally)


nurse <- as.data.frame(scale(WNHcl[,-1]))
nursediss <- daisy(nurse)


pam.out1 <-pam(WNHcl[,c(5,3)], 3, metric="euclidean", stand=TRUE)    
pam.out1
plot(WNHcl[,c(5,3)], col =(pam.out1$cluster+1), main="PAM (K=2)", pch=19)

pam.out2 <-pam(WNHcl[,c(5,4)], 3, metric="euclidean", stand=TRUE)    
pam.out2
plot(WNHcl[,c(5,4)], col =(pam.out2$cluster+1), main="PAM (K=2)", pch=19)

pam.out3 <-pam(WNHcl[,c(3,4)], 3, metric="euclidean", stand=TRUE)    
pam.out3
plot(WNHcl[,c(3,4)], col =(pam.out3$cluster+1), main="PAM (K=2)", pch=19)

plot(WNH$SQRFOOT, WNH$NUMBED)#, col=(as.numeric(WNH$URBAN=="urban")+1))


plot(WNHcl$SQRFOOT, WNHcl$TPY)
plot(WNHcl$NUMBED, WNHcl$TPY)


k<-kmeans(WNH[,c(4,3)], 3, nstart=10) 
plot(WNH$SQRFOOT, WNH$NUMBED, col =(k$cluster+1),main="K-Mean (K=3)", xlab ="", ylab="", pch=19)





library(factoextra)
set.seed(123)
fviz_nbclust(nurse[,c(2,3,4)], kmeans, method = "wss")



crit<-0
for(i in 2:10) {
  set.seed(7)
  irisgroup<-kmeans(WNHcl[,c(3,4,5)], i, nstart=10)
  crit[i-1]<-irisgroup$tot.withinss
}
plot(2:10, crit)  #grafico del gomito

