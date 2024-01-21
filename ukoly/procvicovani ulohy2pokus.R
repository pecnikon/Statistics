# 1
pgeom(q = 6, prob = 0.1)
pnbinom(q = 6, size = 1, prob = 0.1)


?pgeom
sum(dgeom(0:6, prob = 0.1))

# 3
qnorm(p = 0.3, mean = 181, sd = 10)

# 4
ppois(15, lambda = 4) - ppois(q = 9, lambda = 4)

# 5
pk = dbinom(x = 0:15, size= 15, prob = 5/100)
plot(pk, names = 0:15)

rada = seq(from = 0, to = 1, by = 0.01)
plot(qbinom(p = rada, size = 15, prob = 5/100)~rada)

pgeom(q = 6, prob = 5/100)

qgeom(p = 0.7, prob = 5/100) * 10

# 6
qnbinom(p = 0.9, size = 2, prob = 1/50)

qgeom(p = 0.9, prob = 1/50)

pgeom(q = 50, prob = 1/50)

# 7
dpois(x = 8, lambda = 2)

# 8
round(qnorm(.95, mean = 5, sd = 1.5), 1)

1-pnorm(q = 5, mean = 5, sd = 1.5)

pk = rnorm(n = 80, mean = 5, sd = 1.5)
table(pk<5)
xtabs(~pk<5)
46/80

# 9
qgeom(0.8, prob = 0.1)+1
mean(rgeom(1000, 0.1))
mean(rbinom(n = 1000, size = 3, prob = 0.1))
mean(dbinom(x=3, size = 100, prob = 0.1))
#_____________________________________________________________________________
# jeden z probehlych zapoctaku
1-ppois(q = 10, lambda = 5)
navstevnici = dpois(x = 0:50, lambda = 5)
plot(navstevnici, type = "l")

set.seed(305)
# H0 nejsou na sobe zavisle
# H1 jsou na sobe zavisle
kava = sample(x = c("cerna", "mleko", "latte", "cappuccino"), size = 1000, replace = T)
vek = sample(x = c("teen", "dosp", "sen"), size = 1000, replace = T)
xtabs(~kava + vek)
barplot(xtabs(~kava + vek), beside = T)
chisq.test(xtabs(~kava + vek))

# hypotezu nezamitam, p-value je vetsi nez 0.05
# pocet stupnu volnosti je 6
