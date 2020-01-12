Moc <- function(a,delta,proba){
  return((1-pnorm(qnorm(1-a/2)-sqrt(proba/2)*delta))+pnorm(qnorm(a/2)-sqrt(proba/2)*delta))
}

X=seq(-5,5,by=0.01)
Y=Moc(0.05,X,10)

plot(X,Y,  type="l", lwd=1, lty=1,col=c("black"),
     xlab = "delta", ylab="Moc testu")