additives.modell.varianz <- function(t, sigma, N){
  varianz <- t*sigma^2/N
  return(varianz)
}
additives.modell.varianz(t = 3, sigma = 2, N = 12)


additives.modell.E_St <- function(a = 1, S0, t, E_u, N){
  # E_u: erwartungswert von u
  E_St <- a*S0 + t*E_u/N
  return(E_St)
}
additives.modell.E_St(a = 1, S0 = 60, t = 3, E_u = 12, N = 12)


multilplikatives.modell.E_s <- function(S0, delta_t, N, v, sigma){
  # m: divisor für t. 52 für wöchentlich, 12 für monatlich, 1 für jährlich
  # E[S(t_k )]=S_0*exp⁡(μ⋅t_k )  mit  μ=ν+σ^2/2
  mu <- v/N + (sigma^2/N)/2
  E_s <- S0 * exp(mu*delta_t)
  list.out <- list(mu, E_s)
  names(list.out) <- c("mu", "E_s")
  return(list.out)
}
multilplikatives.modell.E_s(S0 = 1, delta_t = 52/4, N = 52, v = 0.2, sigma = 0.3)
multilplikatives.modell.E_s(S0 = 1, delta_t = 52/2, N = 52, v = 0.2, sigma = 0.3)


wienerprozes <- function(mu, t){
  mu <- 0
  varianz <- t
  list.out <- list(mu, varianz)
  names(list.out) <- c("mu", "varianz")
  return(list.out)
}

wienerprozes(3, 4)


geometrische.brownsche.bewegung <- function(a,b,t){
  exp(a*t + b^2/2*t)
}
geometrische.brownsche.bewegung(a = 0.06, b = 0.20, t = 1)
