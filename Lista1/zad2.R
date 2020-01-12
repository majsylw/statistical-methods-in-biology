library("ggplot2")
number <- function(prob){
  
  return (((19.6)^2)*(1-prob)/prob)
}

number(0.1)
pvalue=seq(0.01,1, by=0.01)
numberTossing=number(X)

dane=data.frame(pvalue,numberTossing)


#plot=plot(dane, axes(x=pvalue,y=numberTossing))+ geom_line()
#plot

moneta <- function(prob, len){
  return (sample(c(1,0), len, replace=TRUE, prob=c(prob,1-prob)))
}

mean(moneta(0.5,100))
number(0.5)
mean(moneta(0.5,385))

trunc(number(0.5))+1
alfa=0.05

difer <- function(x,scala){
  if(x<0.1*scala){
    result=0
  }else{
    result=1
  }
  return (result)
}

test <- function(alfa){
  repNum=trunc(number(alfa)+1)
  SymNum=1000
  mactest=matrix(moneta(alfa,repNum*SymNum), ncol=SymNum)
  temp=apply(mactest,2,mean)
  temp=abs(temp-alfa)-0.1*alfa
  return (length(temp[temp>0])/length(temp))
}

test(0.1)
number(0.1)

n=50#licznosc proby - moze byc mala
symNum=1000#liczba powtorzen eksperymentu
Xe=matrix(rexp(n*symNum,1), ncol=symNum)
Ye=matrix(rexp(n*symNum,1), ncol=symNum)
Xme=apply(Xe,2,mean)
Yme=apply(Ye,2,mean)
Ze=(Xme-Yme)*sqrt(n/2)
qqnorm(Ze)
hist(Ze)
ks.test(Ze,"pnorm")