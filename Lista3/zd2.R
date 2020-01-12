# Zadanie2
set.seed(100)
alpha = 0.05
k = 1000 #ilosc powtorzen eksperymentu

zad2 <- function(alfa,n1,n2,symNum){
  
  Xe=matrix(rnorm(n1*symNum),n1,symNum)
  Ye=matrix(rnorm(n2*symNum),n2,symNum) - 1
  #Xe=matrix(rlogis(n1*symNum),n1,symNum)
  #Ye=matrix(rlogis(n2*symNum),n2,symNum) - 1
  #Xe=matrix(rcauchy(n1*symNum),n1,symNum)
  #Ye=matrix(rcauchy(n2*symNum),n2,symNum) - 1
  
  # test Wilcoxona
  pwarA = sapply(1:symNum, function(i){wilcox.test(Xe[,i], Ye[,i], exact = TRUE)$p.value})
  Wilcox = sum(pwarA<alfa)/symNum
  
  # test Studenta
  pwarC = sapply(1:symNum, function(i){t.test(Xe[,i], Ye[,i], var.equal = TRUE)$p.value})
  Student = sum(pwarC<alfa)/symNum
  
  wynik = c(Wilcox, Student)
  return (wynik)
}

przedzial<- function(p,N,alfa){
  return(c(p - qnorm(1-alfa/2)*sqrt(p*(1-p)/N),p + qnorm(1-alfa/2)*sqrt(p*(1-p)/N)))
}

#i) wyniki
I = zad2(alpha,5,10,k)
I[1]
przedzial(I[1],k,alpha)
I[2]
przedzial(I[2],k,alpha)


#ii) wyniki
II = zad2(alpha,10,20,k)
II[1]
przedzial(II[1],k,alpha)
II[2]
przedzial(II[2],k,alpha)

#iii) wyniki
III = zad2(alpha,20,40,k)
III[1]
przedzial(III[1],k,alpha)
III[2]
przedzial(III[2],k,alpha)

bladI = qnorm(1-alpha/2)*sqrt(I*(1-I)/k)
bladII = qnorm(1-alpha/2)*sqrt(II*(1-II)/k)
bladIII = qnorm(1-alpha/2)*sqrt(III*(1-III)/k)

plot(c(1:2),I,ylim=c(0.0,0.65), xlim=c(0.5,2.5), ylab='Power', xlab='n', xaxt='n')
legend("topleft", c("n1=5, n2=10", "n1=10, n2=20",'n1=20, n2=40'), col = c('black','blue','red'), text.col = "black", lty = 1, cex=0.7)
arrows(c(1.2:2.2), III-bladIII, c(1.2:2.2), III+bladIII, length=0.05, col="red", angle=90, code=3)
lines(c(1.1:2.1),II,col = 'blue', type='p')
lines(c(1.2:2.2),III,col = 'red', type='p')
arrows(c(1.1:2.1), II-bladII, c(1.1:2.1), II+bladII,length=0.05, col="blue", angle=90, code=3)
arrows(c(1:2), I-bladI, c(1:2), I+bladI, length=0.05, col="black", angle=90, code=3)
axis(1, at=1.1:2.1,labels = c('Wilcoxon','Student'))


plot(c(1:2),norm[3,],ylim=c(0.0,0.95), xlim=c(0.5,2.5), ylab='Power', xlab='n', xaxt='n')
legend("topleft", c("normal", "logistic",'Cauchy'), col = c('black','blue','red'), text.col = "black", lty = 1, cex=0.7)
arrows(c(1.2:2.2), cauchy[3,]-cauchybl[3,], c(1.2:2.2), cauchy[3,]+cauchybl[3,], length=0.05, col="red", angle=90, code=3)
lines(c(1.1:2.1),logi[3,],col = 'blue', type='p')
lines(c(1.2:2.2),cauchy[3,],col = 'red', type='p')
arrows(c(1.1:2.1), logi[3,]-logibl[3,], c(1.1:2.1), logi[3,]+logibl[3,],length=0.05, col="blue", angle=90, code=3)
arrows(c(1:2), norm[3,]-normbl[3,], c(1:2), norm[3,]+normbl[3,], length=0.05, col="black", angle=90, code=3)
axis(1, at=1.1:2.1,labels = c('Wilcoxon','Student'))