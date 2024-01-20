# investor ist bullisch gegenüber einer Aktia eingestellt.
# er glaubt die Aktie wird in Zukunft steigen.
# Call-Option mit Ausübungspreis K1 kaufen
# zweite Call-Option mit Ausübungspreis K2 VERkaufen, diese ist billiger

# Wert C1 = max(0, S-K1)
# Wert C2 = max(0, S-K2)
# Bulischer Spread: C1 - C2 = max(0, S-K1) - max(0, S-K2)

bullischer_spread <- function(S, K1, K2){
  C1 <- apply(matrix(S-K1), MARGIN = 1, function(x) max(0, x))
  C2 <- apply(matrix(S-K2), MARGIN = 1, function(x) max(0, x))
  bs <- C1 - C2
  par(mfrow = c(1,3))
  plot(S, C1, type = 'l', ylim = c(min(c(C1, C2)), max(c(C1, C2))), main = "long"); abline(v= K1)
  plot(S, -C2, type = 'l', ylim = c(min(c(C1, -C2)), max(c(C1, C2))), main = "short"); abline(v = K2)
  plot(S, bs, type = 'l', ylim = c(min(c(C1, -C2)), max(c(C1, C2))), main = "bulischer fuck spread")
  lines(S, C1, lty = 2, col = "red"); abline(v= K1, lwd = 0.5)
  lines(S, -C2, lty = 2, col = "red"); abline(v = K2, lwd = 0.5)
}
bulischer.spread(S = 0:10, K1 = 3, K2 = 5)


bulischer.spread(S= seq(10000, 15000, by = 100), K1= 10500, K2 = 12000)




# Optionen ----
# Auszahlung Calloption: C(T) = max(0, S − K )
# Auszahlung Putoption: P(T) = max(0, K − S)
## Moniness ----
# A call option is in the money (ITM) if the market price is above the strike price.
# A put option is in the money if the market price is below the strike price.

ITM <- function(type = "C", K, S){
  if(type == "C"){
    if(S>K){
      return_value <- "Call is in the Money"
    } else if(S<K){
        return_value <- "Call is out of the Money"
    } else {
      return_value <- "Call is on the money"
      }
  } else if(type == "P"){
    if(S<K){
      return_value <- "Call is in the Money"
    } else if(S>K){
      return_value <- "Call is out of the Money"
    } else {
      return_value <- "Call is on the money"
    }
  } else{print("choose P:Put or C:Call")}
  return(return_value)
}

ITM(type = "C", K = 10500, S = 11000)
ITM(type = "C", K = 12000, S = 11000)


## Parität ----
put.call.parität <- function(C, K, S, r, t){
  # C(T)−P(T)+B(T) = S
  # Auszahlung des Kaufs einer Calloption, dem Verkauf einer Putoption und dem Verleihen der Summe dK
  B <- exp(-r*t) * K
  P <- C + B - S
  return(P)
}

round(put.call.parität(C = 800, S = 11000, K = 10500, t = 3/12, r = 0.04))
round(put.call.parität(C = 3.92, S = 48, K = 45, t = 2/12, r = 0.03),2)

call.put.parität <- function(P, K, S, r, t){
  # C(T)−P(T)+B(T) = S
  # Auszahlung des Kaufs einer Calloption, dem Verkauf einer Putoption und dem Verleihen der Summe dK
  B <- exp(-r*t) * K
  C <- P - B + S
  return(C)
}
call.put.parität(P = 1.96, K = 50, S = 50, t = 6/12, r = 0.1)


call.put.parität(P = 196, K = 10500, S = 11000, t = 3/12, r = 0.04)





