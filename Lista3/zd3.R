# Zadanie3
set.seed(2457)
alpha = 0.05
k = 1000 #ilosc powtorzen eksperymentu

zad3 <- function(n1,n2,symNum){
  
  Xe=matrix(rnorm(n1*symNum),n1,symNum)
  Ye=matrix(rnorm(n2*symNum),n2,symNum) - 1
  #Xe=matrix(rlogis(n1*symNum),n1,symNum)
  #Ye=matrix(rlogis(n2*symNum),n2,symNum) - 1
  #Xe=matrix(rcauchy(n1*symNum),n1,symNum)
  #Ye=matrix(rcauchy(n2*symNum),n2,symNum) - 1
  
  #delta = sapply(1:symNum, function(i){wilcox.test(Xe[,i], Ye[,i], exact = TRUE, conf.int = TRUE)$estimate})
  prze = sapply(1:symNum, function(i){wilcox.test(Xe[,i], Ye[,i], exact = TRUE, conf.int = TRUE)$conf.int})
  zlicz = 0
  for (i in 1:symNum){
    if (1 > prze[1,i] && 1 < prze[2,i])
      zlicz = zlicz + 1
  }
  
  return (zlicz/symNum)
  #return (apply(prze,1,mean))
  #return (mean(delta))
}


#i) wyniki
I = zad3(5,10,k)
I

#ii) wyniki
II = zad3(10,20,k)
II

#iii) wyniki
III = zad3(20,40,k)
III

bladI = qnorm(1-alpha/2)*sqrt(I*(1-I)/k)
bladII = qnorm(1-alpha/2)*sqrt(II*(1-II)/k)
bladIII = qnorm(1-alpha/2)*sqrt(III*(1-III)/k)

plot(c(1),I,ylim=c(0.9,1.0), xlim=c(0,4), ylab='Power', col='green', xlab='n', xaxt='n')
legend("bottomright", c("n1=5, n2=10", "n1=10, n2=20",'n1=20, n2=40'), col = c('black','blue','red'), text.col = "black", lty = 1, cex=0.8)
arrows(c(3), III-bladIII, c(3), III+bladIII, length=0.05, col="red", angle=90, code=3)
lines(c(2),II,col = 'blue', type='p')
lines(c(3),III,col = 'red', type='p')
arrows(c(2), II-bladII, c(2), II+bladII,length=0.05, col="blue", angle=90, code=3)
arrows(c(1), I-bladI, c(1), I+bladI, length=0.05, col='green', angle=90, code=3)
axis(1, at=1:3,labels = c('','',''))
#lines(c(1,2,3),rep(1,3),col = 'black', type='p')

los <- function(n1,n2,symNum){
  
  #Xe=rnorm(n1)
  #Ye=rnorm(n2) - 1
  #Xe=rlogis(n1)
  #Ye=rlogis(n2) - 1
  Xe=rcauchy(n1)
  Ye=rcauchy(n2) - 1
  
  delta = wilcox.test(Xe, Ye, exact = TRUE, conf.int = TRUE)$estimate
  prze = wilcox.test(Xe, Ye, exact = TRUE, conf.int = TRUE)$conf.int
  dlu = prze[2]-prze[1]
  
  return (c(prze[1],prze[2],dlu, delta))
}

#i) wyniki
Il = los(5,10,k)
Il

#ii) wyniki
IIl = los(10,20,k)
IIl

#iii) wyniki
IIIl = los(20,40,k)
IIIl

plot(c(1),7,ylim=c(-13,4.5), xlim=c(0,4), ylab='Distance', col='green', xlab='n', xaxt='n')
legend("bottomright", c("n1=5, n2=10", "n1=10, n2=20",'n1=20, n2=40'), col = c('green','blue','red'), text.col = "black", lty = 1, cex=0.7)
arrows(c(3), IIIl[1], c(3), IIIl[2], length=0.05, col="red", angle=90, code=3)
arrows(c(2), IIl[1], c(2), IIl[2],length=0.05, col="blue", angle=90, code=3)
arrows(c(1), Il[1], c(1), Il[2], length=0.05, col='green', angle=90, code=3)
axis(1, at=1:3,labels = c('','',''))
lines(c(1,2,3),rep(1,3),col = 'black', type='p')
