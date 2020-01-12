set.seed(10)
macierz_genotypow = cbind(X,Y,Z) # 3 zlaczone chromosomy
k = 500
alfa = 0.05
powt = 100

osobniki = length(macierz_genotypow[,1]) # liczba osobnikow = 500
markery = length(X[1,]) # liczba markerow = 150
cechy = matrix(rnorm(osobniki*k),osobniki,k)

zliczA = 0
zliczB = 0
zliczC = 0
zliczD = 0
ksiZ = matrix(0, markery, k)
for (indeks in 1:k) {
  cecha = cechy[,indeks] 
  pwar = rep(0,markery)
  stat = rep(0,markery)
  ST = matrix(0, powt, markery)
  for (i in 1:markery){
    gr1 = (Z[,i] == 0) * cecha
    gr1 = gr1[gr1 != 0]
    gr2 = (Z[,i] == 1) * cecha
    gr2 = gr2[gr2 != 0]
    pwar[i] = t.test(gr1, gr2, var.equal = TRUE)$p.value
    stat[i] = t.test(gr1, gr2, var.equal = TRUE)$statistic
    ksiZ[i,indeks] = abs(mean(gr2) - mean(gr1))
    
    
    #ST[1,i] = stat[i]
    #cem = matrix(sample(cecha), powt-1, osobniki)
    #for (jj in 2:powt){
      #ce = cem[(jj-1),]
      #gru1 = (X[,i] == 0) * ce
      #gru1 = gru1[gru1 != 0]
      #gru2 = (X[,i] == 1) * ce
      #gru2 = gru2[gru2 != 0]
      #ST[jj,i] = abs(t.test(gru1, gru2, var.equal = TRUE)$statistic)
    #}
  }
  #maxD = apply(ST, 2, max)
  #critical_value2 = kwantyl(alfa, maxD)
  if (length(pwar[(pwar<alfa) == TRUE]) != 0){
    zliczA = zliczA + 1
  } 
  if (length(pwar[(pwar<alfa/markery) == TRUE]) != 0){
    zliczB = zliczB + 1
  }
  if (length(stat[(abs(stat)>critical_value3) == TRUE]) != 0){
    zliczC = zliczC + 1
  }
  #if (length(stat[(abs(stat)>critical_value2) == TRUE]) != 0){
  #  zliczD = zliczD + 1
  #}
}

pA = zliczA/k # 0.856
pB = zliczB/k # 0.008
pC = zliczC/k # 0.112
pD = zliczD/k # 

# Y
# wyniki
#  > pA
#[1] 0.506
#> pB
#1] 0.008
#> pC
#[1] 0.044
#> pD
#[1] 0.008  dla 100 permutacji

# X
#> pA
#[1] 0.46
#> pB
#[1] 0.012
#> pC
#[1] 0.044
#> pD
#[1] 0.02  dla 100 permutacji

# Z
# wyniki
#  > pA
#[1] 0.472
#> pB
#[1] 0.008
#> pC
#[1] 0.048
#> pD
#[1] 0.012 dla 100 permutacji

# wyniki
pA
pB
pC
pD