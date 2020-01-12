number <- function(prob){
  
  return (((19.6)^2)*(1-prob)/prob)
}

pvalue=seq(0.01,1, by=0.01)
numberTossing=number(pvalue)

#dane=data.frame(pvalue,numberTossing)

plot(pvalue,numberTossing,  type="l", lwd=1, lty=1,col=c("black"),
     ylab = "Liczba prób", xlab="Prawdopodobieñstwo")
#plot=plot(dane, aes(x=pvalue,y=numberTossing))+ geom_line()
#plot

moneta <- function(prob, len){
  return (sample(c(1,0), len, replace=TRUE, prob=c(prob,1-prob)))
}

test <- function(alfa){
  repNum=trunc(number(alfa)+1)
  SymNum=10000
  mactest=matrix(moneta(alfa,repNum*SymNum), ncol=SymNum)
  temp=apply(mactest,2,mean)
  temp=abs(temp-alfa)-0.1*alfa
  return (length(temp[temp<=0])/length(temp))
}

test(0.1)
number(0.1)


