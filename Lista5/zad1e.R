# plot(1000, 100, xlim=c(0,200), ylim=c(0,7), xlab = 'd [cM]', ylab = '|t(i)|', col = 'red', type = 'l')

doswiadczenie <- function(b, sumNum, genotypy, nr_markera){
  set.seed(6532)
  osobniki = length(genotypy[,1])  # 500
  L = length(genotypy[1,])  # 200 = dlugosc chromosomu w cM
  cecha = matrix(rnorm(osobniki * sumNum), osobniki, sumNum)
  wiersze = cbind(genotypy[,nr_markera],genotypy[,nr_markera])
  for (ind in 3:sumNum){
    wiersze = cbind(wiersze,genotypy[,nr_markera])
  }
  
  for (i in 1:sumNum){
    wiersze[,i] = wiersze[,i] * rnorm(500) + (1 - wiersze[,i]) * rnorm(500,b)
  }
  
  Stat = matrix(0,L,sumNum)
  for(j in 1:sumNum){
    cecha = wiersze[,j]
    for (i in 1:L){
      gr1 = (genotypy[,i] == 0) * cecha
      gr1 = gr1[gr1 != 0]
      gr2 = (genotypy[,i] == 1) * cecha
      gr2 = gr2[gr2 != 0]
      Stat[i,j] = t.test(gr1,gr2, var.equal = TRUE)$statistic
    }
  }
  # lines(abs(Stat[,5]), col = 5, type='l')
  # lines(abs(Stat[,100]), col = 2, type='l')
  # lines(abs(Stat[,200]), col = j, type='l')
  # lines(abs(Stat[,1000]), col = 3, type='l')
  
  d = apply(abs(Stat), 2, which.max)
  efg = rep(0,sumNum)
  for (indeks in 1:sumNum) {
    efg[indeks] = mean(wiersze[which(genotypy[,d[indeks]] == 0), indeks]) 
    - mean(wiersze[which(genotypy[,d[indeks]] == 1), indeks])
  }
  
  prawd = 0.0
  for (inde in 1:sumNum){
    s = Stat[,inde]
    delta = which(s^2 > (s[d[inde]]^2 - 6.6))
    if (nr_markera %in% delta){
      prawd = prawd + 1
    }
  }
  prawd = prawd/sumNum
  
  return (c(sd(d), mean(d) - nr_markera, sd(efg), mean(efg) - b, prawd))
}

w1 = doswiadczenie(0.2, 1000,Y,100)
w2 = doswiadczenie(0.35, 1000,Y,100)
w3 = doswiadczenie(0.5, 1000,Y,100)

bladI = qnorm(1-0.05/2)*sqrt(w1[5]*(1-w1[5])/200)
bladII = qnorm(1-0.05/2)*sqrt(w2[5]*(1-w2[5])/200)
bladIII = qnorm(1-0.05/2)*sqrt(w3[5]*(1-w3[5])/200)

#> w1
#[1]  39.06462189   0.01900000   0.08368217  0.01550971   0.90500000
#> w2
#[1]  16.53864150  -1.30800000   0.06450578  0.01286878   0.93500000
#> w3
#[1]   5.91772046  -0.40200000   0.062589016  0.007980207   0.94700000