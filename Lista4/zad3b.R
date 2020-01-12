set.seed(10)

kwantyl=function(alpha, dane){
  dane1=sort(dane) 
  return (dane1[floor((1-alpha)*length(dane))])
}

macierz_genotypow = cbind(X,Y,Z) # 3 zlaczone chromosomy
k = 500
powt = 1000
alfa = 0.05

osobniki = length(macierz_genotypow[,1]) # liczba osobnikow = 500
markery = length(macierz_genotypow[1,]) # liczba markerow = 450
cechy = matrix(rnorm(osobniki*k),osobniki,k)

zliczA = 0 # 428
zliczB = 0 # 4
zliczC = 0 # 56
zliczD = 0
for (indeks in 1:k) {
  cecha = cechy[,indeks] 
  pwar = rep(0,markery)
  stat = rep(0,markery)
  ST = matrix(0, powt, markery)
  for (i in 1:markery){
    gr1 = (macierz_genotypow[,i] == 0) * cecha
    gr1 = gr1[gr1 != 0]
    gr2 = (macierz_genotypow[,i] == 1) * cecha
    gr2 = gr2[gr2 != 0]
    pwar[i] = t.test(gr1, gr2, var.equal = TRUE)$p.value
    stat[i] = t.test(gr1, gr2, var.equal = TRUE)$statistic
    #pwar[i] = t.test(cecha~macierz_genotypow[,i], var.equal = TRUE)$p.value
    #stat[i] = t.test(cecha~macierz_genotypow[,i], var.equal = TRUE)$statistic
  #pwar = sapply(1:markery, function(kk){t.test(cecha~macierz_genotypow[,kk], var.equal = TRUE)$p.value})
  #stat = sapply(1:markery, function(kk){t.test(cecha~macierz_genotypow[,kk], var.equal = TRUE)$statistic})

    ST[1,i] = stat[i]
    ks[1,i] = abs(mean(gr2)-mean(gr1))
    cem = matrix(sample(cecha), powt-1, osobniki)
    #ST[2:powt,i] = sapply(1:(powt-1), function(jj){abs(t.test((ce[jj,])~(macierz_genotypow[,i]), var.equal = TRUE)$statistic)})
    
    for (jj in 2:powt){
      ce = cem[(jj-1),]
      gru1 = (macierz_genotypow[,i] == 0) * ce
      gru1 = gru1[gru1 != 0]
      gru2 = (macierz_genotypow[,i] == 1) * ce
      gru2 = gru2[gru2 != 0]
      ST[jj,i] = abs(t.test(gru1, gru2, var.equal = TRUE)$statistic)
      ks[jj,i] = abs(mean(gr2)-mean(gr1))
    }
  }
  maxD = apply(ST, 2, max)
  critical_value2 = kwantyl(alfa, maxD)
  if (length(pwar[(pwar<alfa) == TRUE]) != 0){
    zliczA = zliczA + 1
  } 
  if (length(pwar[(pwar<alfa/markery) == TRUE]) != 0){
    zliczB = zliczB + 1
  }
  if (length(stat[(abs(stat)>critical_value) == TRUE]) != 0){
    zliczC = zliczC + 1
  }
  if (length(stat[(abs(stat)>critical_value2) == TRUE]) != 0){
    zliczD = zliczD + 1
  }
}

pA = zliczA/k # 0.856
pB = zliczB/k # 0.008
pC = zliczC/k # 0.112
pD = zliczD/k # 0.008 dla 1000 powtorzen

# wyniki
pA
pB
pC
pD