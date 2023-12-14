# O: zvetsuji se jezci jatra po pozivani zkvaseneho ovoce
# H0: velikost jater se nemeni
# H1: velikost jater se zvesi
#__________________________________________________________________
# jednovyberovy t-test:
# df = n-1
n = 20
x = seq(from = -4, to = 13, by = 0.01)
y = dt(x = x, df = n-1)
plot(x = x, y = y, type = "l", col = "midnightblue")

# rozdeleni testovaci statistiky za platnosti H1

# minimalni biologicky vyznamna velikost efektu - 10%

# 1) Cohenovo d = velikost efektu na univerzalni skale
d = (27.5 - 25)/3

# 2) decentralizacni parametr
ncp = d * sqrt(n)

# rozdeleni testovaci statistiky za platnosti H1
y2 = dt(x = x, df = n-1, ncp = ncp)
lines(x = x, y = y2, col = "coral2")

# kriticka hodnota - ziskavam kvantilovou funkci q
tcrit = qt(p = 0.95, df = 19)

lines(x = c(tcrit,tcrit), y = c(0, 0.3), col = "green", lwd = 2)

# pravdepodobnost chyby druheho druhu
beta = pt(q = tcrit, df = 19, ncp = ncp)
sila_testu = 1-beta
#__________________________________________________________________
# oboustranna H1: vaha jater se zmeni
# oproti jednostranne se zmeni kriticka hodnota
dev.off()
tcrit2 = qt(p = 0.975, df = 19)

lines(x = c(tcrit2,tcrit2), y = c(0, 0.3), col = "goldenrod", lwd = 2)
lines(x = c(-tcrit2,-tcrit2), y = c(0, 0.3), col = "goldenrod", lwd = 2)

beta2 = pt(q = tcrit2, df = 19, ncp = ncp)-pt(q = -tcrit2, df = 19, ncp = ncp)
sila_testu2 = 1-beta2
#__________________________________________________________________
# POMOCI FUNKCI - package pwr
install.packages("pwr")
library(pwr)
# vypocet sily pro t.test
pwr.t.test(n = n, d = d, sig.level = 0.05, type = "one.sample", alternative = "greater")

# 1) kolik jezku bych potreboval, aby sila testu byla alespon 99%
pwr.t.test(n = NULL, d = d, sig.level = 0.05, power = 0.99, type = "one.sample", alternative = "greater")

# 2) jaky minimalni efekt (v g) bych prokazal s alespon 80% silou
pwr.t.test(n = n, d = NULL, sig.level = 0.05, power = 0.8,  type = "one.sample", alternative = "greater")
# na gramy prepocitam tim, ze vynasobim smerodatnou odchylkou
minimalni_efekt = 0.5769 * 3

# 3) pokud bych mel 30 jezku a velikost efektu d = 1, jakou nejprisnejsi hladinu 
# vyznamnosti si muzu dovolit, aby sila testu byla alespon 90%?
pwr.t.test(n = 30, d = 1, sig.level = NULL, power = 0.9,  type = "one.sample", alternative = "greater")

#__________________________________________________________________
# KORELACNI TEST  
#korelace mnozstvi snedeneh ovocneho kvasu z vahy jater
pwr.r.test(n = NULL, r = 0.3, sig.level = 0.05, power = 0.95)
