# Zadanie2
set.seed(10) # sianie nasionka

n1 = 5
n2 = 10
s1 = 1
s2 = 1
u1 = 0
u2 = 1.1
alpha = 0.05

MocWelcha <- function(a,n1,n2,u1,u2,s1,s2){
  symNum = 1000 #ilosc powtorzen eksperymentu

  Xe=matrix(rnorm(n1*symNum,u1,s1),n1,symNum)
  Ye=matrix(rnorm(n2*symNum,u2,s2),n2,symNum)
  Xme=apply(Xe,2,mean)
  Xs=c(1:symNum)
  for (i in 1:symNum){
    Xs[i] = 1/(n1-1)*sum((Xe[,i]-Xme[i])^2)}
  Yme=apply(Ye,2,mean)
  Ys=c(1:symNum)
  for (k in 1:symNum){
    Ys[k] = 1/(n2-1)*sum((Ye[,k]-Yme[k])^2)}
  Ze=(Xme-Yme)/sqrt((Xs)/n1+Ys/n2)
  v = (Xs/n1 + Ys/n2)^2/((Xs^2/n1^2)/(n1-1)+(Ys^2/n2^2)/(n2-1))
  
  wynik=0
  for(j in 1:symNum){
    if(Ze[j] < (-qt((1-a/2),v[j])) || Ze[j] > qt((1-a/2),v[j])){
      wynik = wynik + 1
    }
  }
  return (wynik/symNum)
}

MocTeo <- function(a,n1,n2,u1,u2,s1,s2){
  
  delta = (u1-u2)/sqrt(s1^2/n1+s2^2/n2)
  
  return(1-pt(qt(1-a/2,n1+n2-2),n1+n2-2,delta)+pt(-qt(1-a/2,n1+n2-2),n1+n2-2,delta))
   
}

MocWelcha(alpha, n1, n2, u1, u2, s1, s2)
MocTeo(alpha, n1, n2, u1, u2, s1, s2)
