
Ft <- function(S0, r, t, c=0, R=0, n=1, zins = "stetig"){
  ## Forward ist eine Abmachung. Das underlying zu dem Preis in der Zeit.
  # Ft: Terminpreis in t
  # t in Jahren
  # c: stetige proportionale Lagerkosten
  # R: stetige proportionale Zinseinnahmen.
  # r: Zins
  if(zins == "stetig"){
    Ft_val <- S0 * exp((r+c-R)*t)
  } else if(zins =="nicht-stetig"){
    Ft_val <- S0 * (1+(r/n))^(t*n)
  } else { 
    Ft_val <-  "wrong spelling in Variable 'zins'"}
  return(Ft_val)
}
# veirteljährliclhe Verzinsung:
#Ft(S0 = 2000, r = 0.02, t = 2, n = 4, zins = "nicht-stetig")

# Zinsen
# r: Kassa-Zinssatz = spotrate von heute bis in t
# Ft: Terminpreis = Forwardpreis Wert des Underlying in t
# f: Terminzins= f(1,4): 3 jähriger Zins in einem Jahr
# short rates f(0,1) oder f(1,2) oder f(2,3)

# F1 = S0 * (1+f(0,1)) 
# F2 = S0 * (1+(f0,1)) * (1+f(1,2))

# spot rate: 1/(1+f(0,2))^2 = 1/(1+f(0,1)) * 1/(1+f(1,2)) 



### Futures sind standardisierte, börsenkotierte Terminkontrakte mit folgenden Parametern:
# •	Basisinstrument sowie deren Einheit (z.B. «Unzen Feingold»)
# •	Multiplikator (z.B. 100 Einheiten)
# •	Fälligkeiten (z.B. ein bestimmter Tag in jedem Monat)
# •	Tick-Grösse der Preisfluktuationen (z.B. 0.1$), Delivery-Spezifikationen, usw. 



