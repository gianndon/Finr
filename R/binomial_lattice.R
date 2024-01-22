binomialLattice <- function(n, S0, K, r, v, sigma = NA, T, optionType, exerciseType, u = NA, d = NA) {
  #' n = Anzahl Perioden
  #' S0 = Heutige Aktienpreis
  #' K = Strike
  #' sigma = Volatilität
  #' T = Periodizität: quartalsweise : T= 4, halbjährlich T = 2
  #' optionType = Call/Put
  #' exerciseType = Eu/Ami für Ami --> "American"
  #' u = Preissprung nach oben
  #' d = Preissprung nach unten
  #' r = risikofreier Zins
  #' v = erwarte Wachstumsrate der log-preise
  
  #' Zeitschritte anwenden
  dt <- 1 / T
  R <- (1 + (r/T))
  #' Berechne Preissprünge oben/unten
  if (is.na(u) || is.na(d)) {
    u <- exp(sigma * sqrt(dt))
    d <- 1 / u
  }
  #' Sollte sigma nicht gegeben sein:
  if (is.na(sigma)) {
    sigma <- log(u)*sqrt(T)
  }
  
  # p ausrechnen:
  p <- 0.5 + 0.5*(v/sigma) * sqrt(dt)
  
  # Risikoneutraler Zins
  q <- (R - d) / (u - d)
  
  # Init. Matrix für stonks
  stockPrices <- matrix(0, nrow = n + 1, ncol = n + 1)
  optionValues <- matrix(0, nrow = n + 1, ncol = n + 1)
  
  # Befüllen von Binomialgitter
  for (i in 0:n) {
    for (j in 0:i) {
      stockPrices[j + 1, i + 1] <- S0 * u^j * d^(i - j)
    }
  }
  
  # Optionswerte bei Fälligkeit
  for (j in 0:n) {
    stockPrice <- stockPrices[j + 1, n + 1]
    if (optionType == "Call") {
      optionValues[j + 1, n + 1] <- max(stockPrice - K, 0)
    } else {
      optionValues[j + 1, n + 1] <- max(K - stockPrice, 0)
    }
  }
  
  # Derivatpreis Rückwärts rechnen
  for (i in (n - 1):0) {
    for (j in 0:i) {
      EUValue <- (1/R) * (((q * optionValues[j + 2, i + 2]) + ((1 - q) * optionValues[j + 1, i + 2])))
      if (exerciseType == "American") {
        stockPrice <- stockPrices[j + 1, i + 1]
        intrinsicValue <- if (optionType == "Call") max(stockPrice - K, 0) else max(K - stockPrice, 0)
        if (intrinsicValue > EUValue) print(c(j+1,i+1))#paste("Vorz. Ausübung Lohnt sich bei Node: ",c(i+1,j+1))
        optionValues[j + 1, i + 1] <- max(EUValue, intrinsicValue)
      } else {
        optionValues[j + 1, i + 1] <- EUValue
      }
    }
  }
  
  
  return(list(R=R, dt=dt, u=u, d=d, p=p, q=q, sigma=sigma, assetlattice = stockPrices, optionValues = optionValues))
}








