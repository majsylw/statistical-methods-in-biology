library("ggplot2")

critical <- function(x){
  wynik=0
  if(x<(-1.96) || x>1.96){
    wynik=1
  }
  return(wynik)
}

MocNormil <- function(delta){
  n = 100
  symNum = 500
  Xe=matrix(rnorm(n*symNum),ncol=symNum)
  Ye=matrix(rnorm(n*symNum)+delta,ncol=symNum)
  Xme=apply(Xe,2,mean)
  Yme=apply(Ye,2,mean)
  Ze=abs(Xme-Yme)*sqrt(n/2)
  return (mean(apply(as.matrix(Ze),1,critical)))
  
}



MocNorm <- function(delta){
  n = 50
  symNum=500
  Xe=matrix(rnorm(n*symNum),ncol=symNum)
  Ye=matrix(rnorm(n*symNum)+delta,ncol=symNum)
  Xme=apply(Xe,2,mean)
  Yme=apply(Ye,2,mean)
  Ze=abs(Xme-Yme)*sqrt(n/2)
  return (mean(apply(as.matrix(Ze),1,critical)))

}

X=seq(-5,5,by=0.01)
Y=apply(as.matrix(X),1,MocNorm)

wek = seq(-0.5,0.4,by=0.1)
Z=Moc(0.05,wek,100)
bladteo2 = sqrt(Z*(1-Z)/100)
Zw=Moc(0.05,wek,50)
bladteo = sqrt(Z*(1-Z)/50)
spr2 = apply(as.matrix(wek),1,MocNormil)
blademp2 = sqrt(spr2*(1-spr2)/100)
spr= apply(as.matrix(wek),1,MocNorm)#50
blademp = sqrt(spr*(1-spr)/50)


plot(wek,spr2, type="p", col="black", ylab = "Moc testu",
     xlab="Parametr przesuniecia")
lines(wek,Zw,col="green",type="l")
arrows(wek, Zw-bladteo, wek, Zw+bladteo, 
        length=0.05, col="green", angle=90, code=3)
arrows(wek, spr2-blademp, wek, spr2+blademp, 
        length=0.05, angle=90, code=3)
lines(wek,spr, type="p", col="blue")
lines(wek,Z,col="red",type="l")
arrows(wek, Z-bladteo2, wek, Z+bladteo2, 
        length=0.05, col="red", angle=90, code=3)
arrows(wek, spr-blademp2, wek, spr+blademp2, 
        length=0.05, col="blue", angle=90, code=3)


MocEmp <- function(shift){
  n=100
  symNum=500
  Xe=matrix(rexp(n*symNum,1),ncol=symNum)
  Ye=matrix(rexp(n*symNum,1),ncol=symNum)
  Xme=apply(Xe,2,mean)
  Yme=apply(Ye,2,mean)
  Ze=(Xme-Yme+rep(shift,symNum))*sqrt(n/2)
  return (mean(apply(as.matrix(Ze),1,critical)))
}


W=apply(as.matrix(X),1,MocEmp)

Moc <- function(a,delta,proba){
  return((1-pnorm(qnorm(1-a/2)-sqrt(proba/2)*delta))
         +pnorm(qnorm(a/2)-sqrt(proba/2)*delta))
}

Z=Moc(0.05,X,100)

plot(X,Y,col="red",type="p", ylab = "Moc testu",
     xlab="Parametr przesuniecia")
lines(X,Z,col="black",type="l", ylab = "Moc testu",
      xlab="Parametr przesuniecia")
points(X,W, type="s", col="green", ylab = "Moc testu",
       xlab="Parametr przesuniecia")