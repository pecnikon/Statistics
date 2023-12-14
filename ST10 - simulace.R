# simulace chyby prvniho druhu
# 1) H0 a vygenerovat data tak aby H0 platila
  # O: Je stredni velikost ropuch 7cm?
  # H0: mu = 7
x = rnorm(n = 20, mean = 7, sd = 1)
# 2) test a ulozeni p hodnoty
p = t.test(x = x, mu = 7)$p.value

# 3) opakovat 10000x
for(i in 1:10000){
  x = rnorm(n = 20, mean = 7, sd = 1)
  p[i] = t.test(x = x, mu = 7)$p.value
}

# 4) proporce falesne signifikantnich vysledku
length(p[p<0.05])/length(p)

# Simulace chyby druheho druhu
# 1) H0 a vygenerovat data tak, aby H0 neplatila
# H0: mu = 7, s efektem 1 cm (efekt - rozdil strednich hodnot)
x = rnorm(20, 8, 1)
p = NULL
p2 = NULL

for(i in 1:10000){
x = rnorm(n = 40, mean = 8, sd = 1)
p[i] = t.test(x = x, mu = 7)$p.value
p2[i] = wilcox.test(x = x, mu = 7)$p.value
}

# falesne nesignifikantni vysledky
length(p[p>0.05])/length(p)
length(p2[p2>0.05])/length(p2)
#_________________________________________________________
# INTERVALOVY ODHAD STREDNI HODNOTY JINEHO NEZ NORMALNIHO ROZDELENI
# pomoci neparametrickeho bootstrapu
# 1) sber dat
set.seed(7)
x = rpois(n = 50, lambda = 9)

# 2) znahodneni dat
x2 = sample(x = x, size = 50, replace = T)

# 3) bodovy opakovat pomoci cyklu
bod_odhad = NULL
for(i in 1:10000){
x2 = sample(x = x, size = 50, replace = T)
bod_odhad[i] = mean(x2)
}

# 4) empiricke kvantily simulovaneho rozdeleni
quantile(x = bod_odhad, probs = c(0.025, 0.975))






