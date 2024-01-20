# Function ----
#' Black Sholes Formula
#'
#' @param S price of underlying
#' @param K strike price
#' @param t delta t (time)
#' @param r return
#' @param sigma volatility (standard deviation)
#' @param type either call or put [default = 'call']
#' @param short boolean (wheter option is shorted or not) [default = 'FALSE']
#' @param verbose boolean (print out information or not) [default = 'FALSE']
#'
#' @return
#' @export

black_scholes <- function(S, K, t, r, sigma, type="call", short=FALSE, verbose=FALSE){
  "
  S = price of underlying
  K = strike price
  t = delta t
  r = return
  sigma = volatility (standard deviation)
  type = either call or put [default = 'call']
  short = boolean (wheter option is shorted or not) [default = 'FALSE']
  verbose = boolean (print out information or not) [default = 'FALSE']
  "
  # cumulative normal probability distribution
  N <- function(x){return(pnorm(q=x))}
  # compute d1 and d2
  d1 <- (log(S/K)+(r+(sigma^2)/2)*t)/(sigma*sqrt(t))
  d2 <- d1 - sigma*sqrt(t)
  # compute option value based on input type
  if(tolower(type) == "call"){
    # compute value
    res <- S*N(x=d1)-K*exp(-r*t)*N(x=d2)
    # test if short is set to true
    if(short == TRUE){res <- -1*res}
    # print and return value
    if(verbose == TRUE){cat("\nValue of", type, "option:\t", res)}
    return(invisible(res))
  }else if(tolower(type) == "put"){
    # compute value
    res <- -S*N(x=-d1)+K*exp(-r*t)*N(x=-d2)
    # check wheter short is set to true
    if(short == TRUE){res <- -1*res}
    # print and return value
    if(verbose == TRUE){cat("\nValue of", type, "option:\t", res)}
    return(invisible(res))
  }else{
    print("Please type in put or call.", quote=FALSE)
  }
}
