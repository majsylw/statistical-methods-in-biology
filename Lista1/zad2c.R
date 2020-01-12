library("ggplot2")

n=100#licznosc proby - moze byc mala
symNum=1000#liczba powtorzen eksperymentu
Xe=matrix(rexp(n*symNum,1), ncol=symNum)
Ye=matrix(rexp(n*symNum,1), ncol=symNum)
Xme=apply(Xe,2,mean)
Yme=apply(Ye,2,mean)
Ze=(Xme-Yme)*sqrt(n/2)

Norm=rnorm(symNum)
#licznik=1:symNum
#dane=data.frame(licznik,Ze,Norm)
#dane2=melt(dane,id="licznik")
#ggplot(dane2,aes(x=value))+geom_density(aes(group=variable,colour=variable)))
qqnorm(Ze)
hist(Ze)
ks.test(Ze,"pnorm")

#moc testu

critical <- function(x){
  wynik=0
  if(x<(-1.96) || x>1.96){
      wynik=1
  }
return(wynik)
}

MocEmp <- function(shift){
  n=100
  symNum=1000
  Xe=matrix(rexp(n*symNum,1),ncol=symNum)
  Ye=matrix(rexp(n*symNum,1),ncol=symNum)
  Xme=apply(Xe,2,mean)
  Yme=apply(Ye,2,mean)
  Ze=(Xme-Yme+rep(shift,symNum))*sqrt(n/2)
  return (mean(apply(as.matrix(Ze),1,critical)))
}

#MocEmp(0.1)
#shift=5

X=seq(-5,5,by=0.01)
Y=apply(as.matrix(X),1,MocEmp)
plot(X,Y)

Moc <- function(a,delta,proba){
  return((1-pnorm(qnorm(1-a/2)-sqrt(proba/2)*delta))+pnorm(qnorm(a/2)-sqrt(proba/2)*delta))
}

X=seq(-5,5,by=0.01)
Z=Moc(0.05,X,100)

plot(X,Y,col="red", ylab = "Moc testu", xlab="Parametr przesuniecia")
lines(X,Z,col="black",type="l", ylab = "Moc testu", xlab="Parametr przesuniecia")