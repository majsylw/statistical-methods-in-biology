set.seed(11)
powt = 1000

wektor = rnorm(500) # wektor z cecha
ab = macierz_genotypow[,10]
statTS = rep(0, powt)
statTS[1] = t.test(wektor~ab, var.equal = TRUE)$statistic

for (inta in 2:powt){
  
}

