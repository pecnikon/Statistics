# 1)
x = seq(from = 0, to = 2, by = 0.01)
density_values_l = rep(x = 1/6, times = 101)
density_values_r = rep(x = 5/6, times = 100)
density_values = c(density_values_l, density_values_r)
plot(x = x, y = density_values, type = "l", col = "hotpink")

# 2)
results = (2:12)
odds = c(1/36,2/36,3/36,4/36,5/36,6/36,5/36,4/36,3/36,2/36,1/36)
barplot(odds, names = results)

# 3)
drogy = pgeom(q = 7, prob = 0.1)

# 4)
jehovisti = dbinom(x = 0:1281, size = 1281, prob = 1/1001)
barplot(jehovisti, names = 0:1281)

# 5)
qnorm(p = 0.3, mean = 181, sd = 10)

# 6)
ppois(q=15, lambda = 4)-ppois(q = 9, lambda = 4)

# 7)
pk = dgeom(x = 0:15, prob = 5/100)
barplot(pk, names = 0:15)

pgeom(q = 6, prob = 5/100)

qgeom(p = 0.7, prob = 5/100) * 10

# 8)
qnbinom(p = 0.9, size = 2, prob = 1/50)
qgeom(p = 0.9, prob = 1/50)
pgeom(q = 50, prob = 1/50)

# 9)
ppois(q = 8, lambda = 2) - ppois(q = 7, lambda = 2)
dpois(x = 8,lambda = 2) # lepsiiii!!!!

# 10)
round(qnorm(p = .95, mean = 5, sd = 1.5), digits = 1)
pnorm(q = 5, mean = 5, sd = 1.5)
rnorm(n = 80, mean = 5, sd = 1.5)

# 11) # tohle mi vychazi mega podivne
qgeom(p = 0.8, prob = 0.1) + 1 
dgeom(x = 1, prob = 0.1)
jahody = dbinom(x = 0:3, size = 3, prob = 0.1)
barplot(jahody, names = 0:3)
mean(jahody)
