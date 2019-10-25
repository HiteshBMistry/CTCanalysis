require(survival)
require(ggplot2)
require(prodlim)
require(rms)
# load in data
dat<-read.csv("dat.csv",header=T)
summary(dat$CTC)

# cohort survival stats
km<-survfit(Surv(Time,Death)~1,data=dat)
print(km)

# plot of distribution of CTCs
ggplot(dat, aes(x="",y=CTC+1))+ylab('CTC')+ylab(" No. of CTCs")+xlab("")+
  geom_boxplot(aes(colour="Brown"), outlier.alpha =  0) +
  geom_jitter(height=0,size=3,aes(colour="Brown"))+
  theme_bw(base_size=15)+ theme(legend.position="none")+
  scale_y_log10(breaks=c(1,10,100,1000),labels=c(0,9,99,999))
# No. patients with low numbers of CTCs
sum(dat$CTC==0)# 38
sum(dat$CTC==1)# 15

# split the CTC data into groups with ~equal frequency
quantile(dat$CTC,c(0.125,0.25,0.375,0.5,0.625,0.75,0.875))
dat$GRP<-NA
dat$GRP[dat$CTC==0]<-"0"
dat$GRP[dat$CTC==1]<-"1"
dat$GRP[dat$CTC==2]<-"2"
dat$GRP[dat$CTC>=3 & dat$CTC<6]<-"[3,6)"
dat$GRP[dat$CTC>=6 & dat$CTC<11]<-"[6,11)"
dat$GRP[dat$CTC>=11 & dat$CTC<25]<-"[11,25)"
dat$GRP[dat$CTC>=25 & dat$CTC<75]<-"[25,75)"
dat$GRP[dat$CTC>=75]<-"[75+"
dat$GRP<-factor(dat$GRP,levels=c("0","1","2","[3,6)","[6,11)",
                                 "[11,25)","[25,75)","[75+"))
# visualise the data
km<-prodlim(Surv(Time,Death)~GRP,data=dat)
plot(km,confint=F,legend=F,percent=F)

m1<-(coxph(Surv(Time,Death)~GRP,data=dat))
summary(m1)
# let's plot the relationship
x<-c(0,0.99,1,1.99,2,2.99,3,5.99,6,10.99,11,24.99,25,74.99,75,max(dat$CTC))
y<-c(0,0,m1$coefficients[1],m1$coefficients[1],
     m1$coefficients[2],m1$coefficients[2],
     m1$coefficients[3],m1$coefficients[3],
     m1$coefficients[4],m1$coefficients[4],
     m1$coefficients[5],m1$coefficients[5],
     m1$coefficients[6],m1$coefficients[6],
     m1$coefficients[7],m1$coefficients[7])
plot(x+1,y,type="l",log="x",xlab="CTC",xaxt="n",
     ylab="log(Hazard Ratio)")
xtick<-c(1, 6,11,34, 101,334,1001)
axis(side=1, at=xtick, labels = c(0,5,10,33,100,333,1000))

x<-seq(0,1800,by=1)
lines(x,2.4/(1+(10/x)^1.5),col=2)
m1<-(coxph(Surv(Time,Death)~c(1/(1+(10/CTC)^1.5)),data=dat))
m2<-(coxph(Surv(Time,Death)~CTC<5,data=dat))

# compare likelihoods
-2*m1$loglik[2]
-2*m2$loglik[2]
# it's clear the models do give different fits
summary(m1)
summary(m2)
# some of you may like looking at concordance values - it's clear there is a difference
# Magic Number gives 0.66, whereas the sigmoid model 0.74

# some of you may want to use th cph function in the rms package to look at more statistics
x1<-as.numeric(dat$CTC<5)
dd<-datadist(x1)
options(datadist='dd')
y<-dat$Death
t<-dat$Time
m3 <- cph(Surv(t,y) ~ x1, x=TRUE, y=TRUE, surv=TRUE) 

x1<-1/(1+(dat$CTC/10)^1.5)
dd<-datadist(x1)
options(datadist='dd')
y<-dat$Death
t<-dat$Time
m4 <- cph(Surv(t,y) ~ x1, x=TRUE, y=TRUE, surv=TRUE)

m3
m4
# you can see the clear difference between the two models across all discrmination indexes

