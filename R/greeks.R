greeks <- function(S,K,r,T,sig,type){
  #' S = Preis des Underlying
  #' K = Strike
  #' r = Zins
  #' T = Fälligkeit
  #' sig = Vola des Underlying
  #' type = Call/Put
  
  if(type == "Call"){
    d1 <- (log(S/K) + r*T +  (sig^2*T)/2) / (sig*sqrt(T))
    d2 <- d1 - sig*sqrt(T)
    
    delta <- pnorm(d1)
    gamma <- (1/(sig*S*sqrt(2*pi*T)))*exp(-(d1^2/2))
    theta <- -((S*dnorm(d1)*sig)/(2*sqrt(T))) - (r*K*exp(-r*T)*pnorm(d2))
    vega <-  S*sqrt(T)*dnorm(d1)
    val <- S*pnorm(d1) - K*exp(-r*T)*pnorm(d2)
    return(list(delta = delta,gamma=gamma,theta=theta,vega=vega,val=val,d1 = d1,d2 = d2))
  }
  if(type == "Put"){
    d1 <- (log(S/K) + r*T +  (sig^2*T)/2) / (sig*sqrt(T))
    d2 <- d1 - sig*sqrt(T)
    
    delta <- pnorm(d1) -1 
    gamma <- (1/(sig*S*sqrt(2*pi*T)))*exp(-(d1^2/2))
    theta <- -((S*dnorm(d1)*sig)/(2*sqrt(T))) + (r*K*exp(-r*T)*pnorm(-d2))
    vega <-  S*sqrt(T)*dnorm(d1)
    val <- (K*exp(-r*T)*pnorm(-d2) - S*pnorm(-d1))
    return(list(delta = delta,gamma=gamma,theta=theta,vega=vega,val= val,d1 = d1,d2 = d2))
  }
  
}