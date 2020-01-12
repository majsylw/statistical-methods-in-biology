set.seed(500)

odleglosc = c(0:149)
cecha = rnorm(500)
StatTX = rep(0,150) # wartosci statystyki Studenta dla chromosomu X

for (i in 1:150){
  gr1 = (X[,i] == 0) * cecha
  gr1 = gr1[gr1 != 0]
  gr2 = (X[,i] == 1) * cecha
  gr2 = gr2[gr2 != 0]

  StatTX[i] = t.test(gr1,gr2, var.equal = TRUE)$statistic
}

# wspolny wykres
plot(odleglosc,StatTX, ylim = c(-2,2), type = 'l', xlab = 'd [cM]', ylab = 'T')
lines(odleglosc,StatTY, type = 'l', xlab = 'd [cM]', ylab = 'T', col = 'red') # chromosom Y
lines(odleglosc,StatTZ, type = 'l', xlab = 'd [cM]', ylab = 'T', col = 'blue') # chromosom Z
legend("topright", c('Chromosom X', 'Chromosom Y', 'Chromosom Z'), col = c('black','red','blue'), text.col = "black", lty = 1, cex=0.6)
