# a)
set.seed(25134)
beta11 = 0.40
beta12 = 0.30
beta13 = 0.20
beta21 = 0.40
beta22 = 0.40
beta23 = 0.20
wiersz = M[,100]
ce = M[,100]
for(ind in 1:n){
  if(wiersz[ind] == 1){
    wiersz[ind] = rbinom(1,1,beta12)
    ce[ind] = rbinom(1,1,beta22)
  }
  if(wiersz[ind] == 0){
    wiersz[ind] = rbinom(1,1,beta11)
    ce[ind] = rbinom(1,1,beta21)
  }
  if(wiersz[ind] == 2){
    wiersz[ind] = rbinom(1,1,beta13)
    ce[ind] = rbinom(1,1,beta23)
  }
}

# b)
Stat1 = rep(0,L)
Stat2 = rep(0,L)
pwar1 = rep(0,L)
pwar2 = rep(0,L)
for (i in 1:L){
  Stat1[i] = chisq.test(wiersz,M[,i], correct = FALSE)$statistic
  Stat2[i] = chisq.test(ce,M[,i], correct = FALSE)$statistic
  
  pwar1[i] = chisq.test(wiersz,M[,i], correct = FALSE)$p.value
  pwar2[i] = chisq.test(ce,M[,i], correct = FALSE)$p.value
}
d1 = which.max(abs(Stat1)) # 99
d2 = which.max(abs(Stat2)) # 96

plot(abs(Stat2), xlim=c(0,200), ylim = c(0,100), xlab = 'd [cM]', ylab = '|t(i)|', type = 'l', col='green')
# lines(abs(Stat2), xlim=c(0,200), xlab = 'd [cM]', ylab = '|t(i)|', col = 'blue', type = 'l')
# legend("topright", c('B1 = 0.4, B2 = 0.3, B3 = 0.2', 'B1 = 0.4, B2 = 0.4, B3 = 0.2'), col = c('green','blue'), text.col = "black", lty = 1, cex=0.6)
# points(d1,Stat1[d1], cex=1, pch=18, col='green')
# points(d2,Stat2[d2], cex=1, pch=18, col='blue')
# lines(rep(100,301), seq(0,150, by = 0.5), type="l", pch=22, lty=2, col="red")


# c)
doswiadczenie3 <- function(b1, b2, b3, sumNum, genotypy, nr_markera){
  set.seed(65632)
  osobniki = length(genotypy[,1])  # 500
  L = length(genotypy[1,])  # 200 = dlugosc chromosomu w cM
  wiersze = cbind(genotypy[,nr_markera],genotypy[,nr_markera])
  for (ind in 3:sumNum){
    wiersze = cbind(wiersze,genotypy[,nr_markera])
  }
  
  for (i in 1:sumNum){
    for(ind in 1:osobniki){
      if(wiersze[ind,i] == 1){
        wiersze[ind,i] = rbinom(1,1,b2)
      }
      if(wiersze[ind,i] == 0){
        wiersze[ind,i] = rbinom(1,1,b1)
      }
      if(wiersze[ind,i] == 2){
        wiersze[ind,i] = rbinom(1,1,b3)
      }
    }
  }
  Stat = matrix(0,L,sumNum)
  for(j in 1:sumNum){
    for (i in 1:L){
      Stat[i,j] = chisq.test(wiersze[,j],M[,i], correct = FALSE)$statistic
    }}
  # lines(abs(Stat[,100]), col = 2, type='l')
  # lines(abs(Stat[,200]), col = j, type='l')
  # lines(abs(Stat[,1000]), col = 3, type='l')
  
  d = apply(abs(Stat), 2, which.max)
  # plot(d, xlab = 'd [cM]', ylab = expression(hat(delta)), type = 'p')
  
  return (d)
  # return (c(sd(d), mean(d) - nr_markera))
}

w1 = doswiadczenie3(0.4, 0.3, 0.2, 1000,M,100)
w2 = doswiadczenie3(0.4, 0.4, 0.2, 1000,M,100)


# > w1
# [1]  3.136871 -0.391000
# > w2
# [1]  0.9932863 -0.2130000
