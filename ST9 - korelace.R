# KORELACE
#____________________________________________________________
set.seed(32)
# samci: n = 40, N(67mm, 10mm), s presnosti na 0.1mm
samci = round(rnorm(n = 40, mean = 67, sd = 10), 1)
# samice: n = 40, N(101mm, 17mm), s presnosti na 0.1mm
samice = round(rnorm(n = 40, mean = 101, sd = 17), 1)

# dochazi k asortativnimu parovani podle velikosti?
# (asortativni parovani = jsou zavisle)

# H0: k asortativnimu parovani nedochazi (parovani je nahodne, r=0)
# H1: k asortativnimu parovani dochazi (parovani je nenahodne, r!=0)

# graficke reseni
plot(samice~samci, col = "coral3", bty = "l", pch = "♥", cex = 3, 
     main = "Velikost samcu a samic" )
lines(x = rep(mean(samci), 2), y = c(min(samice), max(samice)))
lines(x = c(min(samci), max(samci)), y = rep(mean(samice), 2))

# bodovy odhad korelacniho koeficientu
# test predpokladu
# H0: rozdeleni velikosti samcu je normalni
# H1: rozdeleni velikosti samcu neni normalni
shapiro.test(samci) # p = 0.054 > 0.05 H0 nezamitame
shapiro.test(samice) # p = 0.78 > 0.05 H0 nezamitame

# predpoklady pearsonova testu byly splneny
# (pearsonuv test je silnejsi nez spearmanuv)
r = (sum((samci-mean(samci))*(samice-mean(samice)))/40)/
  (sqrt(sum((samci-mean(samci))**2)/40)*sqrt(sum((samice-mean(samice))**2)/40))

r = cor(samci, samice)

# test
tt = r/sqrt(1-r**2)*sqrt(38)
2*(1-pt(q = tt, df = 38))
# = 0.24 > 0.05, H0 se nezamita

# 95% oboustranny intervalovy odhad pomoci fisherovy Z transformace
z = atanh(r) # fisherova z transformace
z1 = z - qnorm(p = 0.975)/sqrt(38) #leva
z2 = z + qnorm(p = 0.975)/sqrt(38) #prava
tanh(z1) # leva strana
tanh(z2) # prava strana
# interval (-0.13, 0.47)

cor.test(samci, samice) # spocita vse, 
# oboustranna p-value je dvojnasobek jednostranne

#___________________________________________________________________
# Spearmanuv test
cor(rank(samci), rank(samice))
cor.test(samci, samice, method = "spearman")
