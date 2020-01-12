set.seed(121680)
# Generacja genotypow na jednym chromosomie z krzyzowki wstecznej
n<-500 # liczba osobnikow - wiersze
L<-200 # dlugosc chromosomu w cM - liczba markerow, kolumny;
d<-1 # odleglosc miedzy sasiednimi markerami w cM
r<-0.5*(1-exp(-0.02*d)) # p-stwo rekombinacji miedzy sasiednimi markerami
P<-rbinom(n,1,0.5) # genotypy w pierwszym markerze
R<-rbinom(n*(L-1),1,r)
R<-matrix(R,nrow=n,ncol=(L-1)) # macierz rekombinacji miedzy sasiednimi markerami
Y<-cbind2(P,R)
Y<-apply(Y,1,'cumsum')
Y<-t(Y)
Y<-Y%%2 # finalna macierz genotypow

# zadanie 1
# a)
Y = Y - 0.5
beta1 = 0.25
beta2 = 0.5
set.seed(12)
cecha = beta1 * Y[,80] + beta2 * Y[,120] + rnorm(500)

# b)
szukanie_pary <- function(ce, genotypy, markery){
  Stat1 = 0.0
  Stat2 = 0.0
  para = rep(0,2)
  macierz = matrix(0, 3, 199*100)
  sym = 1
  for(i in 1:(markery-1)){
    for(j in (i+1):markery){
      Stat2 = summary(lm(ce~genotypy[,c(i,j)]))$fstatistic[1]
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
}

mm = szukanie_pary(cecha, Y, L) # c(75,120)

# c
SymNumer = 1000
wyniki = matrix(0, 2, SymNumer)
for(index in 1:SymNumer){
  cecha = beta1 * Y[,80] + beta2 * Y[,120] + rnorm(500)
  wyniki[,index] = szukanie_pary(cecha, Y, L)
}
# 80cM
sd(wyniki[1,]) # odchylenie standardowe
mean(wyniki[1,]) - 80 # obciazenie
mean((wyniki[1,] - 80)^2) # blad sredniokwadratowy

# 120cM
sd(wyniki[2,]) # odchylenie standardowe
mean(wyniki[2,]) - 120 # obciazenie
mean((wyniki[2,] - 120)^2) # blad sredniokwadratowy

