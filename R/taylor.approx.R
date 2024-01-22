#' Taylor Approximation
#'
#' @param S: die verschiedenen S-Werte
#' @param S0: initialer S-Wert
#' @param K: Strike
#' @param sig: Volatilität
#' @param r: Zinssatz
#' @param type: Call oder Put
#'
#' @return
#' @export
taylor.approx <- function(S, S0, K, sig, r, T, type = "P"){
BlackScholes <- function(S, K, r, T, sig, type = "C"){
  # S= Stock Price
  # K = Strike
  # r = Risk-Free Interest Rate
  # T = Time to expiration
  # sig = Volatility


  ## 𝑆(𝑡)=𝑆0 e^(𝑤(t)),𝑤(0)=0, E[𝑤(𝑡)]=𝜈𝑡, Var[𝑤(𝑡)]=𝜎^2𝑡
  ## 𝑑1 = 1/(𝜎*sqrt(T-t)) *( 𝑙𝑛(𝑆/K) + r(𝑇−𝑡)+𝜎^2/2(𝑇−𝑡) * N(d2))
  ## 𝑑2 = 1/(𝜎*sqrt(T-t)) * (𝑙𝑛(𝑆/K)+𝑟(𝑇−𝑡)−𝜎^2/2 e^r*(𝑇−𝑡) * N(-d2)

  if(type=="C"){
    d1 <- (log(S/K) + (r + sig^2/2)*T) / (sig*sqrt(T))
    d2 <- d1 - sig*sqrt(T)
    #𝐶(𝑆,𝑡) = 𝑆*𝑁(d1) − 𝐾 e^(-r*T) 𝑁(𝑑2)  || N(d1) = pnorm(d1)
    value <- S*pnorm(d1) - K*exp(-r*T)*pnorm(d2)
  }

  if(type=="P"){
    d1 <- (log(S/K) + (r + sig^2/2)*T) / (sig*sqrt(T))
    d2 <- d1 - sig*sqrt(T)
    #𝑃(𝑆,𝑡) = −𝑆 𝑁(−𝑑1) + 𝐾*𝑒^(-r(T))*𝑁(−𝑑2)
    value <-  (K*exp(-r*T)*pnorm(-d2) - S*pnorm(-d1))
  }

  return_list <- list(d1, d2,value)
  names(return_list) <- c("d1", "d2", "value")
  return(return_list)
}

delta <- function(S, K, sig, r, T, type = "C"){
  d1 <- (log(S/K) + (r + sig^2/2)*T) / (sig*sqrt(T))
  if(type == "C"){
    return(pnorm(d1))
  } else {
    return(pnorm(d1)-1)
  }
}
Gamma <- function(S, K, sig, r, T){
  d1 <- (log(S/K) + (r + sig^2/2)*T) / (sig*sqrt(T))
  return((1/(sig*S*sqrt(2*pi*T))) * exp(-(d1)^2/2))
}


  delta <- delta(S0, K, sig, r, T, type = type)
  gamma <- Gamma(S0, K, sig, r, T)
  return(BlackScholes(S0,K,r,T,sig, type = type)$value + delta*(S-S0) + 0.5*gamma * (S-S0)^2)
}

# taylor.approx(S0=48,S=c(51,53,55), K=50, sig=0.25, r = 0.05, T = 3/12, type = "C")
