# Data Cleaning -----------------------------------------------------------
library(readr)
set.seed(123)
datalibrary(readr)
data <- read_csv("Documents/Documents/Data Science/SDS/Tardella/Final Project/Jacopo Nudo/l1.csv")
colnames(data)
data=data[,c(2,4,5,6,7,24,25,26)]
s=sample(c(rep(1,380*0.7),rep(0,380*0.3)),380,replace = T)
train=data[s==1,]
test=data[s==0,]

# Exploratory Data Analysis----
library(ggplot2)
ggplot(data=data, aes(x=data$FTHG, group=data$HomeTeam, fill=data$HomeTeam)) +
  geom_density(adjust=1.5) +
  facet_wrap(~data$HomeTeam)

# Plain Dataset -----------------------------------------------------------
data_plain=train[,c(4,2,3)]
colnames(data_plain)=c("Goal","Attack","Defence")
data_plain$HomeEffect=rep(TRUE,263)
data_plain2=train[,c(5,3,2)]
colnames(data_plain2)=c("Goal","Attack","Defence")
data_plain2$HomeEffect=rep(FALSE,263)
data_plain=rbind(data_plain,data_plain2)
# Bayesian Poisson Model -----------------------------------------------------------
library(rjags)
library(R2jags)
data_jags= list(Goal=data_plain$Goal, Attack=as.integer(as.factor(data_plain$Attack)),Defence=as.integer(as.factor(data_plain$Defence)),HomeEffect=as.integer(as.factor(data_plain$HomeEffect))-1)
wanted=c("a","b","c","d")
model.file="/Users/jacoponudo/Documents/Documents/Data Science/SDS/Tardella/Final Project/Jacopo Nudo/Poisson_jags.txt"
J1 <- jags(data_jags, NULL, wanted,model.file=model.file ,n.chains=1, n.iter=1000,n.burnin = 100)
parameters= c("a","b[1]","b[2]","b[3]","b[4]","b[5]","b[6]","b[7]","b[8]","b[9]","b[10]","b[11]","b[12]","b[13]","b[14]","b[15]","b[16]","b[17]","b[18]","b[19]","b[20]","c[1]","c[2]","c[3]","c[4]","c[5]","c[6]","c[7]","c[8]","c[9]","c[10]","c[11]","c[12]","c[13]","c[14]","c[15]","c[16]","c[17]","c[18]","c[19]","c[20]","d")
SQUADRE=sort(unique(data$Attack))
SQUADRE_ATT=sprintf("%s_ATT", SQUADRE)
SQUADRE_DEF=sprintf("%s_DEF", SQUADRE)
LABELS=c("Intercept",SQUADRE_ATT,SQUADRE_DEF,"Home_Effect")
i=1
par(mfrow=c(2,7))
for (p in parameters[1:14]){
  plot(density(J1$BUGSoutput$sims.array[,1,p]),main=LABELS[i],xlim=c(-2,2))
  abline(v=0,col="tomato",lty=2)
  i=i+1
}
for (p in parameters[15:28]){
  plot(density(J1$BUGSoutput$sims.array[,1,p]),main=LABELS[i],xlim=c(-2,2))
  abline(v=0,col="tomato",lty=2)
  i=i+1
}
for (p in parameters[29:42]){
  plot(density(J1$BUGSoutput$sims.array[,1,p]),main=LABELS[i],xlim=c(-2,2))
  abline(v=0,col="tomato",lty=2)
  i=i+1
}q



plot(J1)
# Bayesian Poisson Model with specific prior-----
model_with_prior="model{
    for(i in 1:length(Goal)) {
    log(m[i])=a+b[Attack[i]]+c[Defence[i]]+d*HomeEffect[i]
    Goal[i] ~ dpois(m[i])
      }
    a~dnorm(0.56, 1/((0.18)**2))
    b[1]=0
    b[2]~dnorm (-0.555, 1/((0.189)**2))
    b[3]~dnorm (-0.736, 1/((0.206)**2))
    b[4]~dnorm (0,200**(-2))
    b[5]~dnorm (-0.620, 1/((0.181)**2))
    b[6]~dnorm (-0.681, 1/((0.169)**2))
    b[7]~dnorm (-0.020, 1/((0.166)**2))
    b[8]~dnorm (-0.181, 1/((0.150)**2))
    b[9]~dnorm (-0.376, 1/((0.165)**2))
    b[10]~dnorm (-0.228, 1/((0.151)**2))
    b[11]~dnorm (-0.055, 1/((0.172)**2))
    b[12]~dnorm (-0.286, 1/((0.138)**2))
    b[13]~dnorm (0,200**(-2))
    b[14]~dnorm (-0.566, 1/((0.156)**2))
    b[15]~dnorm (-0.347, 1/(( 0.183)**2))
    b[16]~dnorm (-0.578, 1/((0.178)**2))
    b[17]~dnorm (-0.601, 1/((0.187)**2))
    b[18]~dnorm (-0.768, 1/((0.191)**2))
    b[19]~dnorm (0,200**(-2))
    b[20]~dnorm (-0.771, 1/((0.173)**2))
    c[1]=0
    c[2]~dnorm (0.339, 1/((0.183)**2))
    c[3]~dnorm (0.202, 1/((0.167)**2))
    c[4]~dnorm (0,200**(-2))
    c[5]~dnorm (0.207, 1/((0.196)**2))
    c[6]~dnorm (0.228, 1/((0.198)**2))
    c[7]~dnorm (-0.164, 1/((0.184)**2))
    c[8]~dnorm (-0.212, 1/((0.191)**2))
    c[9]~dnorm (0.140, 1/((0.170)**2))
    c[10]~dnorm (-0.555, 1/((0.189)**2))
    c[11]~dnorm (-0.071, 1/((0.264)**2))
    c[12]~dnorm (0.173, 1/((0.190)**2))
    c[13]~dnorm (0, 200**(-2))
    c[14]~dnorm (0.143, 1/((0.176)**2))
    c[15]~dnorm (0.194, 1/((0.162)**2))
    c[16]~dnorm (0.462, 1/((0.162)**2))
    c[17]~dnorm (0.363, 1/((0.181)**2))
    c[18]~dnorm (0.175, 1/((0.138)**2))
    c[19]~dnorm (0, 200^(-2))
    c[20]~dnorm (-0.009, 1/((0.242)**2))
    d~dnorm(0.118, 1/((0.055)**2))
}"
writeLines(model_with_prior, con="Poisson_jags2.txt")
J2 <- jags(data_jags, NULL, wanted, "Poisson_jags2.txt",n.chains=2, n.iter=1000,n.burnin = 100)

hist(J1$BUGSoutput$sims.list$b[,3])
plot(J1$BUGSoutput$sims.list$b[,3],type="l")
# Frequentist Poisson Model -----------------------------------------------------------
model_goal<-glm(data_plain$Goal ~ as.factor(data_plain$Attack)+as.factor(data_plain$Defence)+data_plain$HomeEffect, family = poisson)

library(coefplot)
coefplot(model_goal)
summary(model_goal)$deviance

# Test ----
data_plain=test[,c(4,2,3)]
colnames(data_plain)=c("Goal","Attack","Defence")
data_plain$HomeEffect=rep(TRUE,108)
data_plain2=test[,c(5,3,2)]
colnames(data_plain2)=c("Goal","Attack","Defence")
data_plain2$HomeEffect=rep(FALSE,108)
data_plain=rbind(data_plain,data_plain2)
Y_hat_f1=predict(model_goal,data_plain,type="response")
predict(J1,data_plain,type="response")
Y=data_plain$Goal
sum((Y-Y_hat_f1)**2)
plot(density(J1$BUGSoutput$sims.list$d),ylim=c(0,10))
lines(density(J2$BUGSoutput$sims.list$d))
str(J1$BUGSoutput$sims.list)
plot(density(J1$BUGSoutput$sims.list$b[,3]),ylim=c(0,3))
lines(density(J2$BUGSoutput$sims.list$b[,3]),col="red")
