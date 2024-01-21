set.seed(127)
puvodni = rpois(n = 40, lambda = 50)
opravny = rpois(n = 40, lambda = 52)
# H0: na pokyny nema smysl reagovat (rozdil >= 0)
# H1: na pokyny ma smysl reagovat (rozdil < 0)
rozdil = puvodni - opravny

# predpoklad paroveho t-testu: rozdeleni musi byt normalni
  # H0: rozdeleni je normalni
  # H1: rozdeleni neni normalni
  
  shapiro.test(rozdil) # p = 0.0063 < 0.05
  # H0 se zamita, rozdeleni neni normalni --> parovy Wilcoxonuv test

wilcox.test(x = rozdil, alternative = "less")
# p = 0.43 > 0.05
# H0 se nezamita, na pokyny oponentu nema smysl reagovat
# df = 40

boxplot(rozdil, notch = T, col = "deeppink3", lwd = 2)
