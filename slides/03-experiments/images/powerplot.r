
x <- seq(-3.5,8,.01)
h0 <- dt(x,df=30)
h1 <- h0


pdf("powernew.pdf")
plot(x,h0,xlim=c(-3.5,8),type="l",col="red",
lwd=2,xlab="effect size (delta)",ylab="")
lines(x+2.042+.80,h1,lwd=2,col="blue",lty="dashed")


    x = seq(.8,4,by=.01)
    y = dnorm(x)
    x = c(x,.8, .8)-2.84
    y = c(y, 0, dnorm(2.04))
polygon(-x,y, density=30, angle=45,col="blue")

    x = seq(-.8,4,by=.01)
    y = dnorm(x)
    x = c(x,-.8, -.8)+2.84
    y = c(y, 0, dnorm(-.8))
    polygon(x,y, density=30, angle=-45,col="green")


arrows(x1=1.4,x0=6,y0=.05,y1=.05)
text(7,.05,"Pr(Type 2)",col="blue")

arrows(x1=3.5,x0=5,y0=.075,y1=.075)
text(6.5,.075,"Power: 1-Pr(Type 2)",col="green")


    # shade rejection regions
    x = seq(2.042,4,by=.01)
    y = dnorm(x)
    x = c(x,2.04, 2.04)
    y = c(y, 0, dnorm(2.04))
    polygon(x,y, density=30, angle=-45,col="red")




abline(h=0,col="black")
abline(v=2.042,col="red")
abline(v=0,col="red")
abline(v=2.042+.80,col="blue")

legend("topright",legend=c(
"t-Distribution: H0: Effect=0","t-Distriubution: H1: Effect=delta",
"t_(alpha/2)=2.04","t_(1-psi)=.80"),
col=c("red","blue","white","white"),lty=c("solid","dashed"),cex=.8
)


arrows(x1=0,x0=2.04,y0=.3,y1=.3,length=.1,lwd=3)
arrows(x1=2.04,x0=0,y0=.3,y1=.3,length=.1,lwd=3)
text(pos=1,1,.3,"t_{a/2}",col="black",cex=1.2)

arrows(x1=2.04,x0=2.8,y0=.3,y1=.3,length=.1,lwd=3)
arrows(x1=2.8,x0=2.04,y0=.3,y1=.3,length=.1,lwd=3)
text(pos=3,2.5,.31,"t_{1-psi}",col="black",cex=1.2)

dev.off()




