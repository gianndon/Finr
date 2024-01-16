delta <- function(S, K, sig, r, T, type = "C"){
  d1 <- (log(S/K) + (r + sig^2/2)*T) / (sig*sqrt(T))
  if(type == "C"){
    return(pnorm(d1))
  } else {
    return(pnorm(d1)-1)
  }
}
round(delta(S = 49, r = 0.08, sig = 0.20, T = 3/12, K = c(40, 50, 60), type = "P"), 2)

delta(S = 0.6, r = 0, sig = 0.10, T = 3/12, K = 0.6, type = "C")

delta(S = 48, r = 0.05, sig = 0.25, T= 3/12, K= c(40,50,60), type = "C")
delta(S = 48, r = 0.05, sig = 0.25, T= 3/12, K= c(40,50,60), type = "P")


Gamma <- function(S, K, sig, r, T){
  d1 <- (log(S/K) + (r + sig^2/2)*T) / (sig*sqrt(T))
  return((1/(sig*S*sqrt(2*pi*T))) * exp(-(d1)^2/2))
}
Gamma(S = 48, r = 0.05, sig = 0.25, T= 3/12, K= c(40,50,60))

round(Gamma(S = 49, r = 0.08, sig = 0.20, T = 3/12, K = c(40, 50, 60)), 3)


Vega <- function(S, K, sig, r, T){
  d1 <- (log(S/K) + (r + sig^2/2)*T) / (sig*sqrt(T))
  return(S * dnorm(d1) * sqrt(T))
}
Vega(S = 48, r = 0.05, sig = 0.25, T= 3/12, K= c(40,50,60))

round(Vega(S = 49, r = 0.08, sig = 0.20, T = 3/12, K = c(40, 50, 60)), 2)

library(greeks)
BS_European_Greeks(initial_price = 49, exercise_price = 40, r = 0.08, 
                   time_to_maturity = 3/12, volatility = 0.20)




# b). 
taylor.approx <- function(S, S0, K, sig, r, T, type = "P"){
  delta <- delta(S0, K, sig, r, T, type = type)
  gamma <- Gamma(S0, K, sig, r, T)
  return(BlackScholes(S0,K,r,T,sig, type = type)$value + delta*(S-S0) + 0.5*gamma * (S-S0)^2)
}


round(taylor.approx(S= c(51, 53, 55), S0 = 48, K = 50, sig = 0.25, r = 0.05, T = 3/12, type = "C"), 2)
round(BlackScholes(S = c(51, 53, 55), K = 50, sig = 0.25, r = 0.05, T= 3/12, type = "C")$value, 2)
BlackScholes(S = 48, K = 50, sig = 0.25, r = 0.05, T= 3/12, type = "C")

1.80 + 0.4348*3 + 0.5*0.0656*3^2
-1.80-3.18 + 11.38 + 0.13
-1.96-1.95 
4.98 * sqrt(1/3)
4.98 * sqrt(1/3)
4.98 * sqrt(1/(3*4))
4.98 * sqrt(1/(3*4*7))
4.98 * sqrt(4)



25/20 * 0.48
30/25
0.2 * 4.98 * sqrt(1/3)
0.2 * 4.98 * sqrt(1/(3*4))
0.2 * 4.98 * sqrt(1/(3*4*7))
0.2 * 4.98
0.2 * 4.98 * sqrt(4)

# Implizite Volatilität

imp.vola <- c(0.54, 0.46, 0.33, 0.40)
round(BlackScholes(S = 60, K = c(40, 45, 60, 80), sig = imp.vola, r = 0.00, T= 1/12, type = "C")$value, 2)


imp.vola.finder <- function(eff.opt.preis, S, K, sig, r, T, Type = "C"){
  # input:
  # effektive Options-Preise
  # S, K, sig, r, T, Type : um die Schätzung mittels Black-Sholes zu machen
  
  
  # Differenz zwischen dem effektivn Options-Preis und der Schätzung minimieren
  imp.vola <- rep(NA, length(eff.opt.preis))
  for(i in 1:length(eff.opt.preis)){
    fun <- function(sig){abs(BlackScholes(S, K[i], r, T, sig, type = Type)$value - eff.opt.preis[i])}
    imp.vola[i] <- (optim(par = sig, fn = fun, method = "Brent", lower = 0, upper= 100)$par)
  }
  return(cbind(K, imp.vola))
}

imp.vola.finder(S = 60, r= 0, T =1/12, 
                sig =0.20, K = c(40, 45, 60, 80), 
                eff.opt.preis = c(0.01, 0.04, 2.28, 20.02), 
                Type = "P")




P_Call <- 98.99495 # mit Faustregel gerechnet
imp.vola <- 0.24 # gegeben
sig <- 0.2 # gegeben
marktpreis <- P_Call * imp.vola / sig 
round(marktpreis)

P_Call = 0.24 * sig * sqrt(T) 
marktpreis <- P_Call * imp.vola / sig 
marktpreis * sig /P_Call = imp.vola
0.012 * sig/(0.24 * sig * sqrt(T)) = imp.vola = 0.012/(0.24*sqrt(3/12))

round(
  c(20.01,15.04, 2.28, 0.02) 
  - BlackScholes(S = 60, K= c(40,45,60,80), r = 0, T=1/12, sig = 0.27)$value,
  2)

round(
  c(0.01, 0.04, 2.28, 20.02) 
  - BlackScholes(S = 60, K= c(40,45,60,80), r = 0, T=1/12, sig = 0.27, type = "P")$value,
  2)

# theta bei S=K und r = 0

1/(2*pi) * S* sig * sqrt(T-t)
S <- 0.6



