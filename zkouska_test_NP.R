# 1
1-ppois(q = 6, lambda = 5.5)
# pravdepodobnost ze vsichni zaparkuji je 0.31
#____________________________________________________________________________
# 2
set.seed(105)
predtim = rnorm(n = 30, mean = 4.45, sd = 1.1)
potom = rnorm(n = 30, mean = 4.55, sd = 1.25)

rozdil = predtim - potom
# H0: pocet volnych parkovacich mist se nezvysil (rozdil>=0)
# H1: pocet volnych parkovacich mist se zvysil (rozdil<0)

# predpoklad pouziti paroveho t testu - normalita rozdeleni
  # test normality
  # H0: rozdeleni je normalni
  # H1: rozdeleni neni normalni
  shapiro.test(rozdil) # p = 0.82 > 0.05 -> H0 se nezamita, predpoklady splneny
# parovy t test
t.test(rozdil, alternative = "less")
# p = 0.19 > 0.05 -> H0 se nezamita
#odpoved: pocet volnych parkovacich mist se nezvysil
# df = 29
#____________________________________________________________________________
# 3
library(pwr)
d = mean(predtim)-mean(potom)
pwr.t.test(n = 30, d = d, power = NULL, alternative = "less", type = "paired")
# sila testu = 0.31
#____________________________________________________________________________
# 4
set.seed(10)
poc_mist = rpois(n = 52, lambda = 5.3)
doba = rpois(n = 52, lambda = 6.2)

# H0: veliciny nejsou pozitivne korelovane (r<0)
# H1: veliciny jsou pozitivne korelovane (r>0)

  # predpoklad pouziti Pearsonova kor. testu - normalita rozdeleni
  # H0: rozdeleni je normalni
  # H1: rozdeleni neni normalni
  shapiro.test(poc_mist) # p = 0.15 > 0.05
  shapiro.test(doba) # p = 0.06 > 0.05
  # obe rozdeleni jsou normalni, muzeme pouzit Pearsonuv korelacni test
  
cor(poc_mist, doba) # r = -0.11
cor.test(poc_mist, doba, method = "pearson", alternative = "greater")
# p = 0.79 > 0.05 H0 nemuzeme zamitnout
# odpoved: s delsi dobou nenarusta pocet volnych parkovacich mist

plot(doba~poc_mist, pch = 20, bty = "l", col = "coral3")
