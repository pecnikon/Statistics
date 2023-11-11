# 1
barva = rep(c("cerna", "bila", "zrzava", "mourovata"), each = 25000)
srst = sample(c("kratka", "dlouha vlnita", "dlouha rovna"), size = 100000, replace = T)
xtabs(~barva + srst)
chisq.test(xtabs(~barva + srst))
# H0 existuje mezi nimi vztah
# H1 neexistuje mezi nimi vztah
barplot(xtabs(~srst + barva), beside = T)
# hypotezu nemohu zamitnout / p hodnota je vetsi nez 0.05
# pocet stupnu volnosti = 6
#_________________________________________________________________________________
# 2
# H0 = pomery jsou stejne
# H0 = pomery jsou jine
x = sample(c("cervena", "zluta", "zelena", "tyrkysova", "skoricova"), 
               size = 1000, replace = T)
pomery = xtabs(~x)

chisq.test(x = pomery, p = c(42, 1, 9, 15, 33)/100)


ZAMITAM

#_________________________________________________________________________________
# 4
# 40:30:20:15:3
x = sample(1:5, size = 5000, replace = T)
table(x)
barvy = chisq.test(x = table(x), p = c(20, 19, 21, 22, 18)/100)
chisq.test(x = table(x), p = c(80, 60, 40, 30, 6)/216)



