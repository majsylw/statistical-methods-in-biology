# Zadanie1
set.seed(100)
alpha = 0.05
k = 1000 #ilosc powtorzen eksperymentu

zad1 <- function(alfa,n1,n2,symNum){

  Xe=matrix(rnorm(n1*symNum),n1,symNum)
  Ye=matrix(rnorm(n2*symNum),n2,symNum)
  #Xe=matrix(rexp(n1*symNum,1),n1,symNum)
  #Ye=matrix(rexp(n2*symNum,1),n2,symNum)
  
  # a) test Wilcoxona
  pwarA = sapply(1:symNum, function(i){wilcox.test(Xe[,i], Ye[,i], exact = TRUE)$p.value})
  WilcoxA = sum(pwarA<alfa)/symNum
  
  # b1) przybli¿enie rozk³adem normalnym bez poprawki
  pwarnbpB = sapply(1:symNum, function(i){wilcox.test(Xe[,i], Ye[,i], exact = FALSE, correct = FALSE)$p.value})
  WilcoxnbpB = sum(pwarnbpB<alfa)/symNum
  
  # b2) przybli¿enie rozk³adem normalnym z poprawk¹
  pwarnzpB = sapply(1:symNum, function(i){wilcox.test(Xe[,i], Ye[,i], exact = FALSE, correct = TRUE)$p.value})
  WilcoxnzpB = sum(pwarnzpB<alfa)/symNum
  
  # c) test Studenta dla rang
  Z = rbind(Xe,Ye)
  Z = apply(Z,2,rank)
  X = Z[1:n1,]
  Y = Z[n1+1:n2,]
  pwarC = sapply(1:symNum, function(i){t.test(X[,i], Y[,i], var.equal = TRUE)$p.value})
  Student = sum(pwarC<alfa)/symNum
  #Xme=apply(X,2,mean)
  #Yme=apply(Y,2,mean)
  #Xs = apply(X,2,var)*n1/(n1-1)
  #Ys = apply(X,2,var)*n2/(n2-1)
  #Ze = (Xme-Yme)/sqrt(Xs/n1+Ys/n2)
  #pwarC = 2*(1-pnorm(abs(Ze)))
  #Student = sum(pwarC<alfa)/symNum
  
  wynik = c(WilcoxA, WilcoxnbpB, WilcoxnzpB, Student)
  return (wynik)
}

przedzial<- function(p,N,alfa){
  return(c(p - qnorm(1-alfa/2)*sqrt(p*(1-p)/N),p + qnorm(1-alfa/2)*sqrt(p*(1-p)/N)))
}

#i) wyniki
I = zad1(alpha,5,10,k)
I[1]
przedzial(I[1],k,alpha)
I[2]
przedzial(I[2],k,alpha)
I[3]
przedzial(I[3],k,alpha)
I[4]
przedzial(I[4],k,alpha)

#ii) wyniki
II = zad1(alpha,10,20,k)
II[1]
przedzial(II[1],k,alpha)
II[2]
przedzial(II[2],k,alpha)
II[3]
przedzial(II[3],k,alpha)
II[4]
przedzial(II[4],k,alpha)

#iii) wyniki
III = zad1(alpha,20,40,k)
III[1]
przedzial(III[1],k,alpha)
III[2]
przedzial(III[2],k,alpha)
III[3]
przedzial(III[3],k,alpha)
III[4]
przedzial(III[4],k,alpha)

bladI = qnorm(1-alpha/2)*sqrt(I*(1-I)/k)
bladII = qnorm(1-alpha/2)*sqrt(II*(1-II)/k)
bladIII = qnorm(1-alpha/2)*sqrt(III*(1-III)/k)

plot(c(1:4),I,ylim=c(0.02,0.1), xlim=c(0.5,4.5), ylab='Type I error', col = 'green', xlab='n', xaxt='n')
legend("topleft", c("n1=5, n2=10", "n1=10, n2=20",'n1=20, n2=40','alpha = 0.05'), col = c('green','blue','red','black'), text.col = "black", lty = 1, cex=0.6)
arrows(c(1.2:4.2), III-bladIII, c(1.2:4.2), III+bladIII, length=0.05, col="red", angle=90, code=3)
lines(c(1.1:4.1),II,col = 'blue', type='p')
lines(c(1.2:4.2),III,col = 'red', type='p')
arrows(c(1.1:4.1), II-bladII, c(1.1:4.1), II+bladII,length=0.05, col="blue", angle=90, code=3)
arrows(c(1:4), I-bladI, c(1:4), I+bladI, length=0.05, col="green", angle=90, code=3)
axis(1, at=1.1:4.1,labels = c('Wilcoxon','Normal','Normal correction','Student'))
lines(c(1:4,1.1:4.1,1.2:4.2),rep(0.05,12),col = 'black', type='p')