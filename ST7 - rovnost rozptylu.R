# testy o rovnosti rozptylu
# n = 40, N(7, 0.07)
set.seed(7)
m1 = rnorm(n = 40, mean = 7, sd = 0.07)

# ________________________________________________
# BODOVY ODHAD
sum((m1-mean(m1))**2)/length(m1) # vyberovy rozptyl
var(m1) # to stejne
s2 = var(m1)

# ________________________________________________
# INTERVALOVY ODHAD
# 95% oboustranny intervalovy odhad rozptylu

    # TEST NORMALITY
    # H0: rozdeleni m1 se nelisi od normalniho
    # H1: rozdeleni se lisi od normalniho
    shapiro.test(m1) # je rozdeleni normalni?
    # -> p-value - 0.12 > 0.05 --> je to normalni rozdeleni
    n = length(m1) # pocet mereni

# leva strana
(n-1)*s2/qchisq(p = 0.975, df = n-1)

# prava strana
(n-1)*s2/qchisq(p = 0.025, df = n-1)

# s pravdepodobnosti 95% lezi skutecny rozptyl
# v intervalu (0.0036, 0.0089) # skutecny rozptyl se nachazi v tomto intervalu! 0.0049
# intervalovy odhad smerodatne odchylky odmocnime interval na druhou

# 95% pravostranny interval
(n-1)*s2/qchisq(p = 0.05, df = n-1)

# Test hypotezy
# je phmetr dostatecne presny? (rozptyl < 0.0085)
# H0: rozptyl je >= 0.0085 (nebude dostatecne presny)
# H1: rozptyl je < 0.0085 (bude dostatecne presny)
    
# na zaklade pravostranneho intervalu zamitame H0, protoze neobsahuje cislo 0.0085
    
# ________________________________________________
# RESENI TESTEM
TT = (n-1)*s2/0.0085
TT<qchisq(p = 0.05, df = n-1)
pchisq(q = TT, df = n-1)
# p = 0.035 < 0.05, H0 se zamita
# pHmetr je dostatecne presny

# ________________________________________________
install.packages("EnvStats")
EnvStats::varTest()
library(EnvStats)
?varTest
varTest(x = m1, alternative = "less", sigma.squared = 0.0085)

# ________________________________________________
# DVOUVYBEROVY TEST ROZPTYLU
# 2. pHmetr n = 100, N(7, .0077)
set.seed(8)
m2 = rnorm(n = 100, mean = 7, sd = .077)

# je prvni pristroj presnejsi nez ten druhy?
# (rozptyl 1 < rozptyl 2)
# H0: r1 >= r2 (pristroj neni presnejsi)
# H1: r1 < r2 (pristroj je presnejsi)

    # TEST PREDPOKLADU F-TESTU
    shapiro.test(m2)
    # H0: rozdeleni m2 se nelisi od normalniho
    # H1: rozdeleni m2 se lisi od normalniho
    # --> predpoklady jsou splneny (0.4564 > 0.05)

TT = var(m1)/var(m2)
TT<qf(p = 0.05, df1 = length(m1) - 1, df2 = length(m2) - 1)
pf(q = TT, df1 = 39, df2 = 99)

# p = 0.19 > 0.05 --> H0 se nezamita, pristroj 1 neni presnejsi
var.test(x = m1, y = m2, alternative = "less")

# LZE TO I TAKHLE
TT = var(m2)/var(m1)
TT>qf(p = 0.95, df1 = length(m2) - 1, df2 = length(m1) - 1)
1-pf(q = TT, df1 = 99, df2 = 39)

# p = 0.19 > 0.05 --> H0 se nezamita, pristroj 1 neni presnejsi
var.test(x = m2, y = m1, alternative = "greater")

# V PRIPADE JINEHO NEZ NORMALNIHO ROZDELENI
fligner.test(x = list(m1, m2)) # u dvouvyberovych testu je df vzdy 1
boxplot(m1, m2) # tlusta cara je median, "fousy" ukazuji minimum a maximum