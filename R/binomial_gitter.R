deltat <- 0.25
N <- 1 / deltat
u <- exp(0.25 / sqrt(N))
d <- 1/u
exp(-0.25/sqrt(N)) == d
K <- 
r <- 0.1 / 4
R <- 1+r
q <- (R-d)/(u-d)
Pt <- list(c(u,d),c(u*u,u*d,d*d),c(u*u*u, u*u*d, u*d*d, d*d*d),c(u*u*u*u, u*u*u*d, u*u*d*d, u*d*d*d, d*d*d*d))
S0 <- 50
St <- list(S0, c(0,0),list(1,1,1),list(2,2,2,2),list(3,3,3,3,3))
for(i in 1:length(Pt)){
  St[i+1] <- list(unlist(Pt[i])*S0)
  }
St

p <- 0.5 + 0.5*(0.12/0.25) * sqrt(deltat)

t <- 0:4
# plot(1, unlist(St[1]))

S0 * q*q*(1-q)
p^2
(1-p)^2  

p*(1-p)*2

q^2
(1-q)^2
q*(1-q)*2
plot(t, c(unlist(St[1])[1],unlist(St[2])[1], unlist(St[3])[1], unlist(St[4])[1], unlist(St[5])[1]), 
     type = 'b', ylim=c(unlist(St[5])[5], unlist(St[5])[1]),ylab = 'Preis', log = 'y')
points(t ,c(unlist(St[1])[1], unlist(St[2])[2], unlist(St[3])[3], unlist(St[4])[4], unlist(St[5])[5]),type = 'b')
points(3:4, c(unlist(St[4])[1], unlist(St[5])[2]), type = 'b')
points(2:4, c(unlist(St[3])[1], unlist(St[4])[2], unlist(St[5])[2]), type = 'b')
points(1:4, c(unlist(St[2])[1], unlist(St[3])[2], unlist(St[4])[2], unlist(St[5])[3]), type = 'b')
points(1:4, c(unlist(St[2])[2], unlist(St[3])[2], unlist(St[4])[3], unlist(St[5])[3]), type = 'b')
points(2:4, c(unlist(St[3])[3], unlist(St[4])[3], unlist(St[5])[4]), type = 'b')
points(3:4, c(unlist(St[4])[4], unlist(St[5])[4]), type = 'b')
abline(h = K, col = 'red')

# europäischer Put
Pt4 <- ifelse(K-unlist(St[5]) < 0, 0, K-unlist(St[5]))
Pt3 <- c()
for (i in 1:length(Pt4)-1){
  Pt3[i] <- (q * Pt4[(i)] + (1-q)*Pt4[i+1]) / R
}
Pt3

Pt2 <- c()
for (i in 1:length(Pt3)-1){
  Pt2[i] <- (q * Pt3[(i)] + (1-q)*Pt3[i+1]) / R
}
Pt2

Pt1 <- c()
for (i in 1:length(Pt2)-1){
  Pt1[i] <- (q * Pt2[(i)] + (1-q)*Pt2[i+1]) / R
}
Pt1

Pstrich <- (q * Pt1[(1)] + (1-q)*Pt1[2]) /R

round(Pt4,2); round(Pt3,2); round(Pt2,2); round(Pt1,2); round(Pstrich,2)

# nur für amerikanischen PUT!
Wt3 <- ifelse(Pt3 > K- unlist(St[4]), Pt3, K- unlist(St[4])) 
Wt2 <- ifelse(Pt2 > K- unlist(St[3]), Pt2, K- unlist(St[3])) 
Wt1 <-  ifelse(Pt1 > K- unlist(St[2]), Pt1, K- unlist(St[2])) 
Wt0 <- K-S0

Pt4 <- ifelse(K-unlist(St[5]) < 0, 0, K-unlist(St[5]))
col <- ifelse(Pt4 > K- unlist(St[5]), "red", "blue") 
points(rep(4,5), unlist(St[5]), col = col , pch = 16)

Pt3 <- c()
for (i in 1:length(Pt4)-1){
  Pt3[i] <- (q * Pt4[(i)] + (1-q)*Pt4[i+1]) / R
}
Pt3 <- ifelse(Pt3 > K- unlist(St[4]), Pt3, K- unlist(St[4])) 
col <- ifelse(Pt3 > K- unlist(St[4]), "red", "blue") 
points(rep(3,4), unlist(St[4]), col = col , pch = 16)
Pt3


Pt2 <- c()
for (i in 1:length(Pt3)-1){
  Pt2[i] <- (q * Pt3[(i)] + (1-q)*Pt3[i+1]) / R
}
Pt2 <- ifelse(Pt2 > K- unlist(St[3]), Pt2, K- unlist(St[3])) 
col <- ifelse(Pt2 > K- unlist(St[3]), "red", "blue") 
points(rep(2,3), unlist(St[3]), col = col , pch = 16)
Pt2

Pt1 <- c()
for (i in 1:length(Pt2)-1){
  Pt1[i] <- (q * Pt2[(i)] + (1-q)*Pt2[i+1]) / R
}
Pt1 <-  ifelse(Pt1 > K- unlist(St[2]), Pt1, K- unlist(St[2])) 
col <- ifelse(Pt1 > K- unlist(St[2]), "red", "blue") 
points(rep(1,2), unlist(St[2]), col = col , pch = 16)
Pt1

Pstrich <- ((q * Pt1[1]) + ((1-q)*Pt1[2]))/R
col <- ifelse(Pstrich > K- unlist(St[1]), "red", "blue") 
points(0, unlist(St[1]), col = col , pch = 16)

Pstrich

round(Pt4,2); round(Pt3,2); round(Pt2,2); round(Pt1,2); round(Pstrich,2)

