# set.seed(124) -> Z
 set.seed(122) -> Y
#set.seed(121) # -> X
# Generacja genotypow na jednym chromosomie z krzyzowki wstecznej
n<-500 # liczba osobnikow - wiersze
L<-150 # dlugosc chromosomu w cM - liczba markerow, kolumny;
d<-1 # odleglosc miedzy sasiednimi markerami w cM
r<-0.5*(1-exp(-0.02*d)) # p-stwo rekombinacji miedzy sasiednimi markerami
P<-rbinom(n,1,0.5) # genotypy w pierwszym markerze
R<-rbinom(n*(L-1),1,r)
R<-matrix(R,nrow=n,ncol=(L-1)) # macierz rekombinacji miedzy sasiednimi markerami
Y<-cbind2(P,R)
Y<-apply(Y,1,'cumsum')
Y<-t(Y)
Y<-Y%%2 # finalna macierz genotypow