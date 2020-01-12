liczba_prob=10000
liczba_symulacji=10000
daneZ=rep(0, liczba_symulacji)
for( i in 1: liczba_symulacji){
  dane= rnorm(liczba_prob)
  daneX= rnorm(liczba_prob)
  daneY= rnorm(liczba_prob)
  daneZ[i]=abs(mean(daneX)-mean(daneY))*sqrt(liczba_prob/2)
}
hist(daneZ)
plot(density(daneZ))

kwantyl=function(alfa, dane){
  dane1=sort(dane) 
  return (dane1[floor((1-alfa)*length(dane))])
}
A0.1 = kwantyl(0.1, daneZ)
A0.05 = kwantyl(0.05, daneZ)
