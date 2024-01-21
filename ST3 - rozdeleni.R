# N(mu=2°C, sigma=3°C)
# zobrazte hustotni funkci


# hustotni funkce (osa x - hodnoty, osa y - hustota)
x=seq(from=2-3*3,
      to=2+3*3,
      by=0.01,)
y=dnorm(x = x, mean = 2, sd = 3)
plot(y~x, type="l")


# distribucni funkce (osa x - hodnoty, osa y - pravdepodobnost)
x=seq(from=2-3*3,
      to=2+3*3,
      by=0.01,)
y = pnorm(q = x, mean = 2, sd = 3)
plot(y~x, type = "l")
pgeom(q = 6, prob = 0.1)

# kvantilova funkce (osa x - pravdepodobnost, osa y - hodnoty)
x = seq(from = 0, 
        to = 1, 
        by = 0.001)
y = qnorm(p = x, mean = 2, sd = 3)
plot(y~x, type = "l")


# vygenerujte 1000 nahodnych hodnot z daneho rozdeleni (1)
nv = rnorm(n = 1000, mean = 2, sd = 3)
hist(nv, breaks = 10) # histogram, zobrazi sloupce intervalu hodnot


# pravdepodobnost ze teplota bude 2 - 5°C (3)
pnorm(q = 5, mean = 2, sd = 3) - pnorm(q = 2, mean = 2, sd = 3)


# teplota vyssi nez 7°C (4)
1 - pnorm(q = 7, mean = 2, sd = 3)


# teplota, ktera nebude prekrocena v 90% vikendu (5)
qnorm(p = 0.9, mean = 2, sd = 3)


# pravdepodobnost ze teploty mensi nez -2°C
pnorm(q = -2, mean = 2, sd = 3) #0.09


# zobrazte rozdeleni veliciny definovane jako:
# pocet vikendu s prasklym pivem v serii 10 po sobe jdoucich vikendu
# Bi(n = 20, p = 0.09)
?dbinom
pk = dbinom(x = 0:10, size = 20, prob = 0.09)
barplot(pk, names = 0:10)

# pocet vikendu pred prvnim prasklym pivem
pk = dgeom(x = 0:50, prob = 0.09)
barplot(pk, names = 0:50, )

# pocet vikendu pred druhym prasklym pivem
# NB(n = 2, p = 0.09)
pk = dnbinom(x = 0:100, size = 2, prob = 0.09)
barplot(pk, names = 0:100) 

# problem s prasklym pivem nastane v prumeru ve 2 bytech v ramci ulice
# jaka je pravdepodobnost ze nastane ve >3 bytech
?rpois
1 - ppois(q = 3, lambda = 2)
