ni <- function(t){
  wn = (2/t)*(pnorm(t/2)-0.5)/((t/2)*pnorm(t/2)+dnorm(t/2))
  return (wn)
}

FBS <- function(C,L,delta,tc){
  ff = 1 - exp(-2*C*(1-pnorm(tc))-0.04*L*tc*dnorm(tc)*ni(tc*sqrt(0.04*delta)))
  return (ff)
}

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
  
  maxwar = apply(Stat, 2, max)
  pwar = rep(0, length(maxwar))
  for (ind in 1:sumNum){
    pwar[ind] = FBS(1,L,1,abs(maxwar[ind]))
  }
  numery = which(pwar < 0.05)  # kolumny Stat, dla ktorych odrzucamy H0
  moc = length(numery)/sumNum
  sumNum = length(numery)
  
  
  nStat = Stat[,numery[1]]
  nwiersze = wiersze[,numery[1]]
  for (indy in numery[2:(length(numery))]){
    nStat = cbind(nStat,Stat[,indy])
    nwiersze = cbind(nwiersze,wiersze[,indy])
  }
  Stat = nStat
  rm(nStat)
  wiersze = nwiersze
  rm(nwiersze)
  
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
  
  return (c(moc, sd(d), mean(d) - nr_markera, sd(efg), mean(efg) - b, prawd))
}

w1 = doswiadczenie(0.2, 1000,Y,100)
w2 = doswiadczenie(0.35, 1000,Y,100)
w3 = doswiadczenie(0.5, 1000,Y,100)