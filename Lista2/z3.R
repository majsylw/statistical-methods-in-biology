# Zadanie3
set.seed(10) # sianie nasionka

Student <- function(a,n1,n2){
  symNum = 10000 #ilosc powtorzen eksperymentu
  
  Xe=matrix(rnorm(n1*symNum),n1,symNum)
  Ye=matrix(rnorm(n2*symNum),n2,symNum)
  Xme=apply(Xe,2,mean)
  Xs=c(1:symNum)
  for (i in 1:symNum){
    Xs[i] = 1/(n1-1)*sum((Xe[,i]-Xme[i])^2)}
  Yme=apply(Ye,2,mean)
  Ys=c(1:symNum)
  for (k in 1:symNum){
    Ys[k] = 1/(n2-1)*sum((Ye[,k]-Yme[k])^2)}
  Ze=abs(Xme-Yme)/sqrt((Xs)/n1+Ys/n2)
  
  wynik=0
  for(j in 1:symNum){
    if(Ze[j] < (-qnorm(1-a/2)) || Ze[j] > qnorm(1-a/2)){
      wynik = wynik + 1
    }
  }
  return (wynik/symNum)
}

przedzial<- function(p,N,alfa){
  return(c(p - qnorm(1-alfa/2)*sqrt(p*(1-p)/N),p + qnorm(1-alfa/2)*sqrt(p*(1-p)/N)))
}

przedzial(Student(0.05, 5, 10),10000,0.05)
przedzial(Student(0.05, 10, 20),10000,0.05)
przedzial(Student(0.05, 20, 40),10000,0.05)
przedzial(Student(0.05, 40, 80),10000,0.05)

przedzial(Student(0.1, 5, 10),10000,0.05)
przedzial(Student(0.1, 10, 20),10000,0.05)
przedzial(Student(0.1, 20, 40),10000,0.05)
przedzial(Student(0.1, 40, 80),10000,0.05)