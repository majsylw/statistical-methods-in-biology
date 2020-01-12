# -------------------------------------------
# Autor: M. Bogdan
# 500 elementowa proba
set.seed(12)
# Generacja genotypow na jednym chromosomie z krzyzowki wstecznej
n <- 500 # liczba osobnikow - wiersze
L <- 200 # dlugosc chromosomu w cM - liczba markerow, kolumny;
d <- 1 # odleglosc miedzy sasiednimi markerami w cM
r <- 0.5*(1-exp(-0.02*d)) # p-stwo rekombinacji miedzy sasiednimi markerami
P <- rbinom(n,1,0.5) # genotypy w pierwszym markerze
R <- rbinom(n*(L-1),1,r)
R <- matrix(R,nrow=n,ncol=(L-1)) # macierz rekombinacji miedzy sasiednimi markerami
X <- cbind2(P,R)
X <- apply(X,1,'cumsum')
X <- t(X)
X <- X%%2 
set.seed(1222)
# Generacja genotypow na jednym chromosomie z krzyzowki wstecznej
n <- 500 # liczba osobnikow - wiersze
L <- 200 # dlugosc chromosomu w cM - liczba markerow, kolumny;
d <- 1 # odleglosc miedzy sasiednimi markerami w cM
r <- 0.5*(1-exp(-0.02*d)) # p-stwo rekombinacji miedzy sasiednimi markerami
P <- rbinom(n,1,0.5) # genotypy w pierwszym markerze
R <- rbinom(n*(L-1),1,r)
R <- matrix(R,nrow=n,ncol=(L-1)) # macierz rekombinacji miedzy sasiednimi markerami
Y <- cbind2(P,R)
Y <- apply(Y,1,'cumsum')
Y <- t(Y)
Y <- Y%%2
M = X + Y  # finalna macierz genotypow
# -------------------------------------------------
# a)
set.seed(251)
beta11 = 0.50
beta12 = 0.25
beta21 = 0.50
beta22 = 0.50
wiersz = M[,100]
ce = rnorm(500)
for(ind in 1:500){
  if(wiersz[ind] == 1){
    wiersz[ind] = ce[ind] + beta12
    ce[ind] = ce[ind] + beta22
  }
  if(wiersz[ind] == 0){
    wiersz[ind] = ce[ind] + beta11
    ce[ind] = ce[ind] + beta21
  }
}
# b)
Stat01 = sapply(1:L, function(i){anova(lm(wiersz~M[,i]))$'F value'[1]})
pwar01 = sapply(1:L, function(i){anova(lm(wiersz~M[,i]))$'Pr(>F)'[1]})
d01 = which.max(abs(Stat01))  # 100
Stat02 = sapply(1:L, function(i){anova(lm(ce~M[,i]))$'F value'[1]})
pwar02 = sapply(1:L, function(i){anova(lm(ce~M[,i]))$'Pr(>F)'[1]})
d02 = which.max(abs(Stat02))  # 96
# plot(abs(Stat01), xlim=c(0,200), ylim=c(0,150), xlab = 'd [cM]', ylab = '|t(i)|', type = 'l', col = 'green')
# lines(abs(Stat02), xlim=c(0,200), xlab = 'd [cM]', ylab = '|t(i)|', col = 'blue', type = 'l')
# legend("topright", c('B1 = 0.50, B2 = 0.25', 'B1 = 0.50, B2 = 0.50'), col = c('green','blue'), text.col = "black", lty = 1, cex=0.6)
# lines(rep(100,301), seq(0,150, by = 0.5), type="l", pch=22, lty=2, col="red")