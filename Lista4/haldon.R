haldon <- function(d){
  ff = 0.5*(1 - exp(-0.02*d))
  return (ff)
}

tt = seq(0, 150, by = 0.1)
fun = rep(0, length(tt))
for (index in 1:length(tt)){
  fun[index] = haldon(tt[index])
}

plot(tt,fun, type = 'l', ylab = 'r', xlab = 'd[cM]')