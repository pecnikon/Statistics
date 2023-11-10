# 1. ukol
# a)
1 - pbinom(q = 14, size = 20, prob = 0.7)
# b)
plot(dbinom(x = 0:20, size = 20, prob = 0.7), type = "l")


# 2. ukol
set.seed(42)
# pomer 7:5:3:2
# H0 = virove nemoce se vyskytuji ve stejnem pomeru
# H1 = virove nemoce se vyskytuji v jinem pomeru
nemoce_drubezarna = sample(x = c("pt_chrp", "inf_bronch", "pt_reo_art", "pt_ence"), size = 170, replace = T)
pomer_drub = table(nemoce_drubezarna)
chisq.test(x = pomer_drub, p = c(5, 2, 7, 3)/17)

# predpoklady - kazda z nemoci musi byt zastoupena nejmene 5 vzorky
# abych predesla chybe 2. druhu, zvolim vhodnou velikost vzorku

# pocet stupnu volnosti - 3
# zaver - H0 ZAMITAM, p-value je mensi nez 0.05 a je mala pravdepodobnost, ze
# se dopustim chyby 1. druhu