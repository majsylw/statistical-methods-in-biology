set.seed(123)
macierz_genotypow = cbind(X,Y,Z) # 3 zlaczone chromosomy
k = 500
alfa = 0.05

osobniki = length(macierz_genotypow[,1]) # liczba osobnikow = 500
markery = length(macierz_genotypow[1,]) # liczba markerow = 450
cechy = matrix(rnorm(osobniki*markery),osobniki,markery)

zliczA = 0
zliczB = 0
for (indeks in 1:k) {
  cecha = cechy[indeks,]
  for (i in 1:markery){
    gr1 = (macierz_genotypow[,i] == 0) * cecha
    gr1 = gr1[gr1 != 0]
    gr2 = (macierz_genotypow[,i] == 1) * cecha
    gr2 = gr2[gr2 != 0]
    pwar[i] = t.test(gr1,gr2, var.equal = TRUE)$p.value
  }
  if (length((pwar<alfa)[(pwar<alfa) == TRUE]) != 0){
    zliczA = zliczA + 1
  }
  if (length((pwar<alfa/markery)[(pwar<alfa/markery) == TRUE]) != 0){
    zliczB = zliczB + 1
  }
}

pA = zliczA/k
pB = zliczB/k

