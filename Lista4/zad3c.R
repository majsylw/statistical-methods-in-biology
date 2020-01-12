ni <- function(t){
  wn = (2/t)*(pnorm(t/2)-0.5)/((t/2)*pnorm(t/2)+dnorm(t/2))
  return (wn)
}

FBS <- function(C,L,delta,tc){
  ff = 1 - exp(-2*C*(1-pnorm(tc))-0.04*L*tc*dnorm(tc)*ni(tc*sqrt(0.04*delta)))
  return (ff)
}

tt = seq(0.001, 4.001, by = 0.0001)
fun = rep(0, length(tt))
for (index in 1:length(tt)){
  fun[index] = FBS(1, 200, 1, tt[index])
}

plot(tt,fun, type = 'l', xlab = expression(t[c]), ylab = expression(alpha))


szukane = 0.05
pozycja = 1
for (kk in 1:length(fun))
{
  if(abs(fun[kk] - szukane) < abs(fun[pozycja] - szukane)){
    pozycja = kk
  }
}
critical_value = tt[pozycja] # tt[30726] = 3.0735
critical_value
lines(critical_value, fun[pozycja], type ='p', col='red')
