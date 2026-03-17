N=1000
P=2
Xpre=matrix(rnorm(n = N*P),nrow=N,ncol=P)
rotmat=cbind(c(1,.8),c(.8,1))
Xrot=Xpre%*%chol(rotmat)
cor(Xrot)

covmat=cov(Xrot)
Xunrot=Xrot%*%solve(chol(covmat))

cola=rgb(0,0,100,120, alpha=50, maxColorValue=255)
colb=rgb(100,0,100,120, alpha=50, maxColorValue=255)

plot(Xrot, col=cola, pch=16, cex=1.2, xlim=c(-5,5),ylim=c(-5,5))
chol(covmat)
solve(chol(covmat))
plot(Xunrot, col=colb, pch=16, cex=1.2, xlim=c(-5,5),ylim=c(-5,5))
