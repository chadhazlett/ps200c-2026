#---------------------------------------
# Code for Annan and Blattman Piece on Effects of Abduction, with Matching
#---------------------------------------
library(foreign)
library(Matching)
library(ebal)
#library(mediation)

setwd("~/Dropbox/chad_transfer/UCLA/PS200D/slides/04SOO")

dat=read.csv("BlattmanAnnan.csv")
View(dat)

#------------------------------------------------------------------
#MATCHING
#------------------------------------------------------------------
#Check the covariate balance in the unmatched dataset. 

#First, before reporting actual balance statistics let's take a look at
#what seems to predict treatment status:
summary(lm(abd~age+fthr_ed+mthr_ed+hh_size96+orphan96+
             C_ach+C_akw+C_ata+C_kma+C_oro+C_pad+C_paj+C_pal,data=dat))

#Second, we could check balance "by hand", e.g.
#Balance re: age
t.test(dat$age~dat$abd, var.equal=FALSE)
ks.test(x=dat$age[dat$abd==1], y=dat$age[dat$abd==0])
plot(ecdf(dat$age[dat$abd==1]),verticals=TRUE,col=2,cex=0,main="eCDFs of Age by Abduction Status")
par(new=TRUE)
plot(ecdf(dat$age[dat$abd==0]),verticals=TRUE,col=1,cex=0,main="")
legend("topleft",lty=c(1,1), c("Treated","Control"),col=c(2,1))

#Confirm with density plots
dev.off()
plot(density(dat$age[dat$abd==1]),ylim=c(0,0.1),col=2,cex=0,main="PDF of Age by Abduction Status")
lines(density(dat$age[dat$abd==0]),col=1,cex=0,main="")
legend("topright",lty=c(1,1),c("Treated","Control"),col=c(2,1))

#Often you'll want to speed up the process, so use MatchBalance
#With just a few covars first, to make it easy to see:
mb = MatchBalance(abd~age+fthr_ed+mthr_ed+hh_size96,data=dat)

#With everything
mb = MatchBalance(abd~age+fthr_ed+mthr_ed+hh_size96+orphan96+
                    C_ach+C_akw+C_ata+C_kma+C_oro+C_pad+C_paj+C_pal,data=dat)

varnames=c("age","fthr_ed","mthr_ed","hh_size96","orphan96",
           "C_ach","C_akw","C_ata","C_kma","C_oro","C_pad","C_paj","C_pal")

#A useful function: baltest.collect (from ebal)
btest=baltest.collect(mb,var.names=varnames,after=F)

#Display output, rounding to two digits
round(btest,2)
round(btest[,c("mean.Tr","mean.Co","T pval","KS pval")],2)


###-------------------------------------------------------------
### Matching
###------------------------------------------------------------

#Select covariates to match on in order to identify 
#the ATT, according to our identification assumptions. 
#Together with others you may want to get balance on. 
#Use one match per treated unit.

varnames=c("age","fthr_ed","mthr_ed","hh_size96","orphan96",
           "C_ach","C_akw","C_ata","C_kma","C_oro","C_pad","C_paj","C_pal")
#varnames_short=c("age","C_ach","C_akw","C_ata","C_kma","C_oro","C_pad","C_paj","C_pal")

exactmatch=c(FALSE,TRUE,TRUE,FALSE,TRUE, replicate(8,"TRUE"))  #describes which vars to get exact matches on	
X=dat[,varnames] #put together covariate data

#Do the matching:
?Match

matchout=Match(Y=dat$educ, Tr=dat$abd, X=X, M=1,exact=exactmatch,estimand="ATT")
#with bias adjustment
matchout=Match(BiasAdjust=TRUE,Y=dat$educ, Tr=dat$abd, X=X, M=1, exact=exactmatch,estimand="ATT")

#See what is in matchout object:
ls(matchout)
summary(matchout)
matchout$est
matchout$se

length(matchout$index.treated)
length(matchout$index.control)
length(unique(matchout$index.control))

#compare to "naive" estimate:
summary(lm(dat$educ~dat$abd))

#---------------------------------------
# Balance after Matching:
#-----------------------------------------
# (Note, you can check balance on things that weren't matched on)
mb.out = MatchBalance(match.out=matchout,abd~age+fthr_ed+mthr_ed+hh_size96+orphan96+
                        C_ach+C_akw+C_ata+C_kma+C_oro+C_pad+C_paj+C_pal,data=dat)

btest_after=baltest.collect(mb.out,var.names=varnames,after=TRUE)

#Display output, rounding to two digits
round(btest_after[,c("mean.Tr","mean.Co","T pval","KS pval")],2)

#See balance before and after next to each other
balancecompare=cbind(round(btest[,c("mean.Tr","mean.Co","T pval","KS pval")],2),#
                     round(btest_after[,c("mean.Tr","mean.Co","T pval","KS pval")],2))

balancecompare

###-----------------------------------------
### Balance by weighting
###----------------------------------------
library(ebal)
#remove one location dummy
X.ebal=X[,-13]
varnames.ebal=varnames[-13]
eb.out = ebalance(dat$abd, X=X.ebal)

#That only gives weights for controls; I'd like an N-dim vector with all weights:
ebalw=replicate(length(dat$abd),1)
ebalw[dat$abd==0]=eb.out$w

bout.ebal=MatchBalance(dat$abd~as.matrix(X),weights=ebalw)
bal.ebal=baltest.collect(matchbal.out=bout.ebal,var.names=colnames(X),after=F)

balancecompare.ebal=cbind(round(btest[,c("mean.Tr","mean.Co","T pval","KS pval")],2),#
                     round(bal.ebal[,c("mean.Tr","mean.Co","T pval","KS pval")],2))

balancecompare.ebal

#Let's plot the standardized imbalance:

d=rbind(as.matrix(btest[,4]),as.matrix(bal.ebal[,4]))
#d=rbind(as.matrix(bal.kbal[,4]),as.matrix(bal.match[,4]))
d.vnames=rownames(d)
rownames(d)=NULL
colnames(d)="sdiff.pooled"
d=data.frame(d)
d$vname <- d.vnames
d$vname <- factor(d$vname,levels = unique(d$vname)[ncol(X):1], labels = unique(d$vname)[ncol(X):1])
d$gr <- rep(c("orig","ebal"),each=ncol(X))

library(lattice)
library(RColorBrewer)
Cex <- 1.2
Cex2 <- 1

mypal<-brewer.pal(3,"Set2")

#pdf("ebal_blattman.pdf", heigh=4, width=6)
print(
  
  xyplot(vname~sdiff.pooled/100,data=d,groups=gr,
         #xlim=c(min(d$sdiff.pooled/100),max(d$sdiff.pooled/100)),
         xlim=c(-2,2),
         panel = function(x,y,...)
         {
           panel.abline(v=0, lwd = 1 , lty="solid")
           panel.abline(v=c(-2,-1,1,2), lwd = 2 , lty="dotted")
           panel.abline(h=c(1:nrow(d)), lwd = 1 , lty="dashed", col="gray95")
           #panel.grid(v=0,h=-1)
           panel.xyplot(x,y,...)
         }
         ,par.settings = list(superpose.symbol = list(pch = c(15,18,19),col=mypal,cex=1.2))
         ,xlab=list("standardized difference in means",cex=Cex),ylab="",auto.key=T,scales=list(y=list(cex=Cex),x=list(cex=Cex2,at=c(-1,-.5,0,.5,1),labels=c("-1","-.5","0",".5","1")))
  )
)

dev.off()

###---------------------------------------
### Propensiy Scores
###--------------------------------------
pi.out = glm(abd~age+fthr_ed+mthr_ed+hh_size96+orphan96+
                        C_ach+C_akw+C_ata+C_kma+C_oro+C_pad+C_paj+C_pal,data=dat, family="binomial"(link=logit))

#How do they look for treated and control?
plot(density(pi.out$fit[dat$abd==1]), lwd=2, main="Distribution of pscores")
lines(density(pi.out$fit[dat$abd==0]), lwd=2, lty=2)
legend("topleft", legend=c("treated","controls"), lty=c(1,2), lwd=2)

matchout.pi=Match(Y=dat$educ, Tr=dat$abd, X=pi.out$fit, M=1,estimand="ATT")
summary(matchout.pi)

#Check balance
mb.out.pi = MatchBalance(match.out=matchout.pi,abd~age+fthr_ed+mthr_ed+hh_size96+orphan96+
                        C_ach+C_akw+C_ata+C_kma+C_oro+C_pad+C_paj+C_pal,data=dat)

btest_after_pi=baltest.collect(mb.out.pi,var.names=varnames,after=TRUE)

#See balance before and after next to each other
balancecompare.pi=cbind(round(btest[,c("mean.Tr","mean.Co","T pval","KS pval")],2),
                     round(btest_after[,c("mean.Tr","mean.Co","T pval","KS pval")],2),
                     round(btest_after_pi[,c("mean.Tr","mean.Co","T pval","KS pval")],2))

balancecompare.pi

###-----------------------------------------------
### Now with stabilized IPW weights:
###----------------------------------------------
ps=pi.out$fit
D=dat$abd
PrD=mean(D)
IPW= (D*PrD+(1-D)*(1-PrD))/(D*ps+(1-D)*(1-ps))
plot(density(IPW))

#See how distribution of pscores has changed:

par(mfrow=c(1,2))
plot(density(pi.out$fit[D==1]), lwd=2, main="Distribution of pscores")
lines(density(pi.out$fit[D==0]), lwd=2, lty=2)
legend("topleft", legend=c("treated","controls"), lty=c(1,2), lwd=2)

plot(density(pi.out$fit[D==1], weight=IPW[D==1]/sum(IPW[D==1])), lwd=2, main="Distribution of pscores: Weighted")
lines(density(pi.out$fit[D==0], weight=IPW[D==0]/sum(IPW[D==0])),lwd=2, lty=2)
legend("topleft", legend=c("treated","controls"), lty=c(1,2), lwd=2)

dev.off()

#Check omnibus balance:
omnibus.bal = lm(abd~age+fthr_ed+mthr_ed+hh_size96+orphan96+
                   C_ach+C_akw+C_ata+C_kma+C_oro+C_pad+C_paj+C_pal, data=dat) 
summary(omnibus.bal)

omnibus.bal.ipw = lm(abd~age+fthr_ed+mthr_ed+hh_size96+orphan96+
                   C_ach+C_akw+C_ata+C_kma+C_oro+C_pad+C_paj+C_pal, 
                   weight=IPW, data=dat) 
summary(omnibus.bal.ipw)  #yay!


#Now use weights in regression:
lm.out.naive = lm(educ~abd, data=dat) 
summary(lm.out.naive)

lm.out.ipw = lm(educ~abd, weight=IPW, data=dat) 
summary(lm.out.ipw)

#Adding covariates should not change estimate on "abd":
lm.out.ipw2 = lm(educ~abd+age+fthr_ed+mthr_ed+hh_size96+orphan96+
                  C_ach+C_akw+C_ata+C_kma+C_oro+C_pad+C_paj+C_pal, weight=IPW, data=dat) 
summary(lm.out.ipw2)


