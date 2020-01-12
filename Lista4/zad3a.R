macierz_genotypow = cbind(X,Y,Z)
k = 500
alfa = 0.05

markery = length(macierz_genotypow[1,]) # liczba markerow = 450

zlicz = 0
for (indeks in 1:k) {
  cecha = rnorm(500)
  for (i in 1:markery){
    gr1 = (macierz_genotypow[,i] == 0) * cecha
    gr1 = gr1[gr1 != 0]
    gr2 = (macierz_genotypow[,i] == 1) * cecha
    gr2 = gr2[gr2 != 0]
    pwar[i] = t.test(gr1,gr2, var.equal = TRUE)$p.value
  }
  if (length((pwar<alfa)[(pwar<alfa) == TRUE]) != 0){
    zlicz = zlicz + 1
  }
}

zlicz/500

