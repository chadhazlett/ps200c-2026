

setwd("~/Dropbox/mit/quant2/2012/slides/04RE")
library(foreign)
library(xtable)

d <- read.dta("gerber.dta")

labels <- c("Control","Hawthorne","Civic Duty","Neighbors","Self") 
d$treatment <- factor(d$treatment,levels=0:4,labels=labels)

d <- read.dta("gerber.dta")
covars <- c("hh_size","g2002","g2000","p2004","p2002","p2000","sex","yob")
print(aggregate(d[,covars],by=list(d$treatment),mean),digits=3)

print(aggregate(d[,covars],by=list(d$treatment),sd),digits=3)

print(aggregate(d[,c("yob")],by=list(d$treatment),quantile),digits=3)

d$treatment <- as.numeric(d$treatment)

form <- as.formula(paste("treatment","~",paste(covars,collapse="+")))
form
summary(lm(form,data=d))

d <- read.dta("gerber.dta")
covars <- c("hh_size","g2002","g2000","p2004","p2002","p2000","sex","yob")
print(xtable(aggregate(d[,covars],by=list(d$treatment),mean)),digits=3)

print(aggregate(d[,covars],by=list(d$treatment),sd),digits=3)

print(aggregate(d[,c("yob")],by=list(d$treatment),quantile),digits=3)

d$treatment <- as.numeric(d$treatment)

form <- as.formula(paste("treatment","~",paste(covars,collapse="+")))
form
summary(lm(form,data=d))
