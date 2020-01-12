# a)
set.seed(1222)
# Generacja genotypow na jednym chromosomie z krzyzowki wstecznej
n<-500 # liczba osobnikow - wiersze
L<-200 # dlugosc chromosomu w cM - liczba markerow, kolumny;
d<-1 # odleglosc miedzy sasiednimi markerami w cM
r<-0.5*(1-exp(-0.02*d)) # p-stwo rekombinacji miedzy sasiednimi markerami
P<-rbinom(n,1,0.5) # genotypy w pierwszym markerze
R<-rbinom(n*(L-1),1,r)
R<-matrix(R,nrow=n,ncol=(L-1)) # macierz rekombinacji miedzy sasiednimi markerami
Y<-cbind2(P,R)
Y<-apply(Y,1,'cumsum')
Y<-t(Y)
Y<-Y%%2 # finalna macierz genotypow

# b)
set.seed(251)
beta = c(0.2,0.35,0.5)
wiersze = cbind(Y[,100],Y[,100],Y[,100])
wiersze[,1] = wiersze[,1] * rnorm(500) + (1 - wiersze[,1]) * rnorm(500,beta[1])
wiersze[,2] = wiersze[,2] * rnorm(500) + (1 - wiersze[,2]) * rnorm(500,beta[2])
wiersze[,3] = wiersze[,3] * rnorm(500) + (1 - wiersze[,3]) * rnorm(500,beta[3])

# c)
Stat02 = sapply(1:L, function(i){t.test(wiersze[,1]~Y[,i], var.equal = TRUE)$statistic})
Stat035 = sapply(1:L, function(i){t.test(wiersze[,2]~Y[,i], var.equal = TRUE)$statistic})
Stat05 = sapply(1:L, function(i){t.test(wiersze[,3]~Y[,i], var.equal = TRUE)$statistic})
plot(abs(Stat05)^2, xlab = 'd [cM]', ylab = '|t(i)|', col = 'red', type = 'l')
lines(abs(Stat02)^2, col = 'green', type='l')
lines(abs(Stat035)^2, col = 'blue', type='l')

d02 = which.max(abs(Stat02))
d035 = which.max(abs(Stat035))
d05 = which.max(abs(Stat05))

delta02 = which(Stat02^2 > (Stat02[d02]^2 - 6.6))
delta035 = which(Stat035^2 > (Stat035[d035]^2 - 6.6))
delta05 = which(Stat05^2 > (Stat05[d05]^2 - 6.6))

lines(rep(Stat05[d05]^2 - 6.6,200), type="l", pch=22, lty=2, col="red")
lines(rep(Stat035[d035]^2 - 6.6,200), type="l", pch=22, lty=2, col="blue")
lines(rep(Stat02[d02]^2 - 6.6,200), type="l", pch=22, lty=2, col="green")
points(c(86,132),c(Stat02[86]^2,Stat02[132]^2), cex=1, pch=18, col='green')
points(c(86,119),c(Stat035[86]^2,Stat035[119]^2), cex=1, pch=18, col='blue')
points(c(91,101),c(Stat05[91]^2,Stat05[101]^2), cex=1, pch=18, col='red')
legend("topright", c('B = 0.2', 'B = 0.35', 'B = 0.5'), col = c('green','blue','red'), text.col = "black", lty = 1, cex=0.6)


# d)
efg02 = mean(wiersze[which(Y[,d02] == 0), 1]) - mean(wiersze[which(Y[,d02] == 1), 1])
efg035 = mean(wiersze[which(Y[,d035] == 0), 2]) - mean(wiersze[which(Y[,d035] == 1), 2])
efg05 = mean(wiersze[which(Y[,d05] == 0), 3]) - mean(wiersze[which(Y[,d05] == 1), 3])

dlugosci2 = c(132-86,119-86,101-91)
dlugosci = c(max(delta02)-min(delta02), max(delta035)-min(delta035),max(delta05)-min(delta05))

plot((beta*sqrt(500))^(-2), dlugosci, col = 'red', type = 'o', xlab=expression(xi^-2), ylab=expression(abs(Delta)), ylim = c(10,170))
lines((beta*sqrt(500))^(-2), dlugosci2, col = 'blue', type = 'o')
