# zadanie 2
beta1 = 0.25
beta2 = 0.5
gamma5 = 0.5
gamma1 = 1.0

set.seed(112)
cecha2_5 = beta1 * Y[,80] + beta2 * Y[,120] + gamma5*Y[,80] * Y[,120] + rnorm(500)
cecha2_1 = beta1 * Y[,80] + beta2 * Y[,120] + gamma1*Y[,80] * Y[,120] + rnorm(500)

szukanie_pary2 <- function(ce, genotypy, markery){
  Stat1 = 0.0
  Stat2 = 0.0
  para = rep(0,2)
  macierz = matrix(0, 3, 199*100)
  sym = 1
  for(i in 1:(markery-1)){
    for(j in (i+1):markery){
      Stat2 = summary(lm(ce~I(genotypy[,i]+genotypy[,j]+genotypy[,i]*genotypy[,j])))$fstatistic[1]
      macierz[1,sym] = i
      macierz[2,sym] = j
      macierz[3,sym] = Stat2
      if (!is.null(Stat2)){
        if(Stat2 > Stat1){
          Stat1 = Stat2
          para = c(i,j)  
        }
      }
      sym = sym + 1
    }
  }
  return (macierz)
  #return (para)
}

m1=szukanie_pary2(cecha2_1, Y, L) # c(80, 118)
m2=szukanie_pary2(cecha2_5, Y, L) # c(104, 119)

m3=szukanie_pary(cecha2_1, Y, L) # c(84, 114)
m4=szukanie_pary(cecha2_5, Y, L) # c(34, 118)

# a)
SymNumer = 1000
wyniki2a5 = matrix(0, 2, SymNumer)
for(index in 1:SymNumer){
  cecha2_5 = beta1 * Y[,80] + beta2 * Y[,120] + gamma5*Y[,80]*Y[,120] + rnorm(500)
  wyniki2a5[,index] = szukanie_pary2(cecha2_5, Y, L)
}

wyniki2a1 = matrix(0, 2, SymNumer)
for(index in 1:SymNumer){
  cecha2_1 = beta1 * Y[,80] + beta2 * Y[,120] + gamma1*Y[,80]*Y[,120] + rnorm(500)
  wyniki2a1[,index] = szukanie_pary2(cecha2_1, Y, L)
}

# b)
wyniki2b5 = matrix(0, 2, SymNumer)
for(index in 1:SymNumer){
  cecha2_5 = beta1 * Y[,80] + beta2 * Y[,120] + gamma5*Y[,80]*Y[,120] + rnorm(500)
  wyniki2b5[,index] = szukanie_pary(cecha2_5, Y, L)
}

wyniki2b1 = matrix(0, 2, SymNumer)
for(index in 1:SymNumer){
  cecha2_1 = beta1 * Y[,80] + beta2 * Y[,120] + gamma1*Y[,80]*Y[,120] + rnorm(500)
  wyniki2b1[,index] = szukanie_pary(cecha2_1, Y, L)
}



