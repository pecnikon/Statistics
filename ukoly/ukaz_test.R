# 1
navratnost = dbinom(x= 0:70, size = 70, prob = 0.45)
plot(navratnost)

# 2
set.seed(117)
d=(34.65-31.5)/1
install.packages("pwr")
library(pwr)

pwr.t.test(n = 10, 
           d = d,
           sig.level = 0.05, 
           power = NULL, 
           type = "one.sample",
           alternative = "greater")

# alternativa
p = NULL
for(i in 1:10000){
  data = rnorm(n = 10, mean = 34.65, sd = 3)
  test = t.test(x = data, mu = 31.5, alternative = "greater")
  p[i] = test$p.value
}
  
length(p[p<=0.05])/10000
p

# 4
# H0: akltivita neroste s velikosti ocasu (r<=0)
# H1: pozitivni korelace, akltivita roste s velikosti ocasu (r>0)

set.seed(117)
aktivita = sample(x = 1:10, size = 70, replace = T)
ocas = rnorm(n = 70, mean = 113, sd = 6)
sd
  # test normality rozdeleni
  shapiro.test(aktivita) # <0.05
  shapiro.test(ocas) # >0.05
  # zamitame -> spearman
cor.test(aktivita, ocas, method = "spearman", alternative = "greater")
# p = 0.59>0.05, H0 nezamitame
cor(rank(aktivita), rank(ocas))
# r = -0.027
#___________________________________________________________________________
# tohle uz neni z toho ukazkoveho
# 1
1 - pbinom(q = 7, size = 10, prob = 7/10)

set.seed(18)
vel_duse = rnorm(n = 40, mean = 11, sd = 2.7)
pocet_skutku = rpois(n = 40, lambda = 125)
shapiro.test(vel_duse)
shapiro.test(pocet_skutku)
cor(rank(pocet_skutku), rank(vel_duse))
cor.test(pocet_skutku, vel_duse, method = "spearman", alternative = "greater")
