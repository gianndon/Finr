#' Value of a Eurpoean Put- or Call-Option
#'
#' @param r interest rate
#' @param n periods of the binomial lattice
#' @param u probability for upward movement in the binomial lattice
#' @param d probability for downward movement in the binomial lattice
#' @param K strike price
#' @param S0 spot price
#' @param type either call or put
#'
#' @return value of put/call
#' @export
#'
#' @examples
put_call_value <- function(r, n, u, d, K, S0, type=""){
  "Computes the Value of a European Put or Call."

  "
  r = interest rate
  n = periods of the binomial lattice
  q = risk free probability
  v = end values of the call or put (depending on input)
  type = either call or put
  "

  # definitions
  R <- 1+r  # discount factor
  k <- 0:n  # index/iteration variable
  end_values <- rep(0, n)  # empty vector for end values
  q <- (R-d)/(u-d)

  # distinction between call and put
  if(type == "call"){
    for(i in 0:n){
      end_values[i+1] <- max(0, u^(i) * d^(n-i) * S0 - K)
    }
  }else if(type == "put"){
    for(i in 0:n){
      end_values[i+1] <- max(0, K - u^(i) * d^(n-i) * S0)
    }
  }else{
    print("Wrong entry! Either type in call or put.")
  }

  # compute value of option
  res <- (1/R^n)*sum(choose(n, k)*(q^k)*((1-q)^(n-k))*end_values)

  # print and return value of option
  cat("\nValue of", type, "option:", res)
  return(invisible(res))

}
