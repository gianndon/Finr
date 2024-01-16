library("FinancialMath")

option.call(S, K, r, t, sd, price = NA, position, plot = TRUE)

swap.rate(rates= c(0.02, 0.02, 0.025), type = "spot_rate")
swap.commodity(prices = c(50,50,50), rates = c(0.02, 0.02, 0.025), type = "spot_rate")


forward(S=100,t=2,r=.03,position="long",div.structure="none", plot = TRUE)
straddle(S=100,K=110,r=.03,t=1,price1=15,price2=10,position="long", plot = TRUE)




## Futures
### Futures sind standardisierte, börsenkotierte Terminkontrakte mit folgenden Parametern:
# •	Basisinstrument sowie deren Einheit (z.B. «Unzen Feingold»)
# •	Multiplikator (z.B. 100 Einheiten)
# •	Fälligkeiten (z.B. ein bestimmter Tag in jedem Monat)
# •	Tick-Grösse der Preisfluktuationen (z.B. 0.1$), Delivery-Spezifikationen, usw. 


# ein Future: 1 *bushel *(27kg/bushel) * 5000 *(1t/1000kg) = 135 t/future
400 / (27*5000/1000) 
# = 3 Futures

### Minimum Varianz Hedge
mvh <- function(corelation){
  # corelation: Korrelation des einen Gutes welches das andere Hedgen soll
  sqrt(1-corelation^2)
}
mvh(0.92)


## Forwards
# «Normal Backwardation» ist, wenn der Terminpreis 𝐹(𝑇) niedriger ist als der
# erwartete zukünftige Kassapreis zum Zeitpunkt 𝑇. Es wird also erwartet, dass
# der Terminpreis steigt. «Contango» ist, wenn der Terminpreis 𝐹(𝑇) höher ist
# als der erwartete zukünftige Kassapreis zum Zeitpunkt 𝑇. Es wird also
# erwartet, dass der Terminpreis sinkt.
Ft <- function(S0, r, t, c=0, R=0, zins = "stetig"){
  # Ft: Terminpreis in t
  # t in Jahren
  # c: stetige proportionale Lagerkosten
  # R: stetige proportionale Zinseinnahmen.
  if(zins == "stetig"){
    F <- S0 * exp((r+c-R)*t)
  } else if(zins =="jährlich"){
    F <- S0 * (1+r)^t
  }
  return(F)
}
Ft(S0 = 2000, r = 0.02, t = 0.5)
Ft(S0 = 30000, r = 0.04, t = 0.5)#, zins = "jährlich")


Ft(S0 = 1.20, r = 0.01 - 0.02,t = 3/12 )
Ft(S0 = 2800, r = 0.015, R = 0.025, t = 1/12)

## Terminzinsen
t <- c(0,6,12,18,24) # in Monaten
f <- c(0.04, 0.05, 0.06, 0.07) # jährliche Zinsen

# kassazins für eine einjährige Laufzeit:
sqrt(1.04) * sqrt(1.05) -1

# Kassazins für zweijährige laufzeit
sqrt(sqrt(1.04) * sqrt(1.05) * sqrt(1.06) * sqrt(1.07)) -1
sqrt(prod(sqrt(1+f))) -1

# Terminzins für eine einjährige Laufzeit in einem Jahr
sqrt(1.06) * sqrt(1.07) -1


1.02^-(1/12) + 1.02^-(1/6) + 1.02^-(1/6) * 1.025^-(1/12)


## Swaps
# Austausch von Zahlungsströmen 
# die Summe des einen  X - die Summe des anderen F(tk) = 0
# Summe(X- F(tk)) abdiskontiert = 0

1.02^-(1/12) + 1.02^-(1/6) + 1.02^-(1/6) * 1.025^-(1/12)



