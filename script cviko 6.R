# 60 zajicu s hmotnosti z N(mu = 8kg, sd = 1kg)
set.seed(11)
zajici = rnorm(n = 60, mean = 8, sd = 1)

#_______________________________________________________
# bodovy odhad stredni hodnoty hmotnosti zajicu
mean(zajici)
sum(zajici)/length(zajici)

#_______________________________________________________
# intervalovy odhad stredni hodnoty normalniho rozdeleni
# primo priradi pravdepodobnost k tomu co tvrdime
#_______________________________________________________
# 95% oboustranny intervalovy odhad stredni hmotnosti

# leva strana
  mean(zajici)-qt(p = 0.975, df = length(zajici)-1)*(sd(zajici)/sqrt(length(zajici)))

# prava strana
  mean(zajici)+qt(p = 0.975, df = length(zajici)-1)*(sd(zajici)/sqrt(length(zajici)))

# S ve vzorecku je smerodatna odchylka
# -> interval (7.59, 8.02)
#_______________________________________________________
# 95% pravostranny intervalovy odhad
  mean(zajici)+qt(p = 0.95, df = length(zajici)-1)*(sd(zajici)/sqrt(length(zajici)))
# -> interval (-Inf, 7.99)
  
#_______________________________________________________
# reseni funkci
  t.test(x = zajici) # oboustranny interval, satci pouzit tohle
  t.test(x = zajici, alternative = "less") # pravostranny int.
  t.test(x = zajici, alternative = "greater") # levostranny int.
  
#_______________________________________________________
# O: je hmotnost zajicu mensi nez 8kg?
# H0: hmotnost neni meni nez 8kg (je vetsi nebo rovna 8kg)
# H1: hmotnost je mensi nez 8kg
# predpoklady t-testu - normalita rozdeleni:
    # H0: rozdeleni hmotnosti zajicu se nelisi od normalniho
    # H1: rozdeleni hmotnosti zajicu se lisi od normalniho
  
shapiro.test(zajici)
# p = 0.89, H0 se nezamita, rozdeleni je normalni, pouzijeme t-test
TT = (mean(zajici)-8)/sd(zajici)*sqrt(length(zajici))
TT < qt(p = 0.05, df = length(zajici)-1) # pokud je tohle pravda, H0 se zamita
pt(q = TT, df = length(zajici)-1)
# p = 0.04 < 0.05 -> H0 se zamita
t.test(x = zajici, alternative = "less", mu = 8)

# v pripade, ze nesplnime predpoklad, dela se Wilcoxonuv test
wilcox.test(x = zajici, alternative = "less", mu = 8, conf.int = T)
