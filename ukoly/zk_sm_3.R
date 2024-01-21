#1
n = round(785*.52, digits = 0)
1-pbinom(q = 1, size = 408, prob = 0.03)

#2
set.seed(320)
# CR = 35,23,22,16,4
# H0: procentualni zisky jsou stejne
# H1: procentualni zisky se lisi

obec = sample(1:5, size = 408, replace = T) 
obec_proc = xtabs(~obec)

chisq.test(obec_proc, p = c(35,23,22,16,4)/100)
# p < .05, H0 se zamita, procentualni zisky pol. stran se lisi v obci a CR
# df = 4

#3
set.seed(325)
# H0: sociolog nema pravdu, korelace neni negativni (r>0)
# H0: sociolog ma pravdu, korelace je negativni (r<0)
poc_ob = rpois(n = 120, lambda = 8000)
volici = rpois(n = 120, lambda = 56)

  # test normality
  shapiro.test(poc_ob) # p < .05
  shapiro.test(volici) # p > .05
  
# -> spearmanuv test
  
cor.test(poc_ob, volici, alternative = "less", method = "spearman")
# p > .05, H0 se nezamita, sociolog nema pravdu
r = -0.138

#4 asi??
library(pwr)
pwr.r.test(n = 120, r = r, alternative = "less")
