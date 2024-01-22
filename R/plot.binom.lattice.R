#' plot.binom.lattice
#'
#' nimmt Werte von der Funktion binomiallattice und plottet diese.
#'
#' @param assetlattice:
#' @param n: maxmialer Zeitschritt
#' @param optionValues
#'
#' @return
#' @export
plot.binom.lattice <- function(assetlattice, n, optionValues=NA){
  if(!any(is.na(optionValues))){
    labels <- optionValues
  } else {
    labels <- assetlattice
  }
  assetlattice[assetlattice == 0] <- NA
  plot(nt, assetlattice[1,], ylim=c(min(assetlattice, na.rm=T), max(assetlattice, na.rm=T)+5),
       xlim = c(0, n+0.5))
  nt <- 0:n
  for(x in 2:nt){
    points(nt, assetlattice[x+1,])

    text(x=x,
         y = assetlattice[,x+1],
         label = round(labels[,x+1], 2),
         pos= 3)
  }
}
