
# parameter
Y    <- 90
N    <- 26
SD <- 40
P    <- .5
N; SD

# equal variance, no covariates
ppow <- function(SD=1,N,Sig=.05,Po=.8,P=.5,Y=Y){
DF   <- N-2
M    <- qt(1-Sig/2,DF) +  qt(Po,DF)
MDE  <- M*sqrt(SD^2/(N*P*(1-P)))
MDES <- M*sqrt(1/(N*P*(1-P)))
out  <- cbind(MDE,MDES,N,SD,Sig,Po,mean(Y),median(Y),(MDE/mean(Y))*100)
out1 <- matrix(NA,1,length(out))
out1 <- out
colnames(out)[7:9] <- c("mean Y","median Y","MDE/Mean")
return(round(out,2))
}
                    out
                    
out <- matrix(NA,5,9)
colnames(out) <- colnames(ppow(N=N,SD=SD,Y=Y))
out[1,] <- ppow(N=1000,SD=500,Y=2500)
out[2,] <- ppow(N=500 ,SD=500,Y=2500)
out[3,] <- ppow(N=100 ,SD=500,Y=2500)
out[4,] <- ppow(N=50 ,SD=500,Y=2500)
out[5,] <- ppow(N=25 ,SD=500,Y=2500)



ppow.cov <- function(SD=1,N,Sig=.05,Po=.8,P=.5,Y=Yt,R=R){
DF   <- N-2-1
M    <- qt(1-Sig/2,DF) +  qt(Po,DF)
MDES  <- M*sqrt((1-R)/(N*P*(1-P)))
MDE   <- MDES*SD
out  <- cbind(MDE,MDES,N,SD,Sig,Po,mean(Y),median(Y),(MDE/mean(Y))*100,R)
out1 <- matrix(NA,1,length(out))
out1 <- out
colnames(out)[7:10] <- c("mean Y","median Y","MDE/Mean","R")
return(round(out,2))
}

R = .9
out1 <- matrix(NA,8,10)
colnames(out1) <- colnames(ppow.cov(N=N,SD=SD,Y=Y,R=R))
out1[1,] <- ppow.cov(N=N,SD=SD,Y=Y,R=R)
out1[2,] <- ppow.cov(N=N,SD=SD,Y=Y,R=R-.1)
out1[3,] <- ppow.cov(N=N,SD=SD,Y=Y,R=R-.2)
out1[4,] <- ppow.cov(N=N,SD=SD,Y=Y,R=R-.3)
out1[5,] <- ppow.cov(N=N-4,SD=SD,Y=Y,R=R)
out1[6,] <- ppow.cov(N=N-4,SD=SD,Y=Y,R=R-.1)
out1[7,] <- ppow.cov(N=N-4,SD=SD,Y=Y,R=R-.2)
out1[8,] <- ppow.cov(N=N-4,SD=SD,Y=Y,R=R-.3)
print(out1[,-8])



pdf("pow1.pdf")
RR <-  seq(0,.95,.01)
plot(RR,ppow.cov(N=20,SD=SD,Y=Y,R=RR)[,"MDE"],ylim=c(0,60),ylab="MDE",xlab="R_b",type="l",col="red")
lines(RR,ppow.cov(N=50,SD=SD,Y=Y,R=RR)[,"MDE"],col="green")
lines(RR,ppow.cov(N=100,SD=SD,Y=Y,R=RR)[,"MDE"],col="blue")
legend("topright",legend=c("N=20","N=50","N=100"),lty=1,col=c("red","green","blue"))
dev.off()
