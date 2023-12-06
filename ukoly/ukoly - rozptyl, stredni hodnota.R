# 1)  
set.seed(42)
# H0: oba modely jsou stejne presne
# H1: modely nejsou stejne presne
mereni1 = rnorm(n = 30, mean = 7, sd = 2)
mereni2 = rnorm(n = 30, mean = 7.4, sd = 1.5)

# test normality - H0: je normalni, H1: neni normalni
shapiro.test(mereni1) # 0.35 > 0.05
shapiro.test(mereni2) # 0.06 > 0.05
# H0 nezamitam, rozdeleni je normalni

# rovnost rozptylu - H0: rozptyly jsou stejne H1: rozptyly nejsou stejne
var.test(x = mereni1, y = mereni2)
# p = 0.014 < 0.05 -> H0 zamitam, rozptyly nejsou stejne
boxplot(mereni1, mereni2)
#_______________________________________________________________________

# 2)
set.seed(7)
# H0: oba modely jsou stejne presne
# H1: modely nejsou stejne presne
mereni1 = rnorm(n = 30, mean = 7, sd = 2)
mereni2 = rnorm(n = 30, mean = 7.4, sd = 1.5)

# test normality - H0: je normalni, H1: neni normalni
shapiro.test(mereni1) # 0.30 > 0.05
shapiro.test(mereni2) # 0.99 > 0.05
# H0 nezamitam, rozdeleni je normalni

# rovnost rozptylu - H0: rozptyly jsou stejne H1: rozptyly nejsou stejne
var.test(x = mereni1, y = mereni2)
# p = 0.001 < 0.05 -> H0 zamitam, rozptyly nejsou stejne

# welchuv t-test
t.test(x = mereni1, y = mereni2, var.equal = F)

# p = 0.47 > 0.05 --> H0 se nezamita, oba modely jsou stejne presne
#_______________________________________________________________________

# 3)
set.seed(1)
# H0: tloustka vytycek neni mensi nez 4 cm
# H1: tloustka vytycek je mensi nez 4 cm
vytycky = rnorm(n = 10, mean = 3, sd = 0.5)

# test normality - H0: je normalni, H1: neni normalni
shapiro.test(vytycky) # 0.53 > 0.05 rozdeleni je normalni

# splneny predpoklady pro t-test
t.test(x = vytycky, alternative = "less", mu = 4) 
# p = 1.7e-05 < 0.05 H0 se zamita, tloustka je mensi nez 4 cm
boxplot(vytycky)
#_______________________________________________________________________

# 4)
set.seed(6)
# H0: typ managementu neovlivnil pocet koniklecu
# H1: typ managementu ovlivnil pocet koniklecu
kosene = rpois(n = 15, lambda = 20)
pasene = rpois(n = 15, lambda = 25)
# test normality - H0: je normalni, H1: neni normalni
shapiro.test(kosene) # 0.03 < 0.05
shapiro.test(pasene) # 0.64 > 0.05
#  --> rozdeleni neni normalni

wilcox.test(x = kosene, y = pasene)
# p = 0.0002 < 0.05 --> H0 se zamita, typ managementu ovlivnuje pocet koniklecu
#_______________________________________________________________________

# 5)
set.seed(42)
# H0: hloubky hlasu jsou stejne
# H1: hloubky hlasu jsou ruzne
samci = rnorm(n = 30, mean = 393, sd = 20.3)
samice = rnorm(n = 30, mean = 380, sd = 40.2)
# test normality - H0: je normalni, H1: neni normalni
shapiro.test(samci) # 0.35 > 0.05
shapiro.test(samice) # 0.06 > 0.05
# H0 nezamitam, rozdeleni je normalni

# rovnost rozptylu - H0: rozptyly jsou stejne H1: rozptyly nejsou stejne
var.test(x = samci, y = samice)
# p = 0.008 < 0.05 -> H0 zamitam
t.test(x = samci, y = samice, var.equal = F)
# p = 0.04 < 0.05 H0 zamitam
boxplot(samci, samice)
#_______________________________________________________________________

# 6)
set.seed(639767)
# H0: mnozstvi je stejne
# H1: mnozstvi je ruzne
dvoulete = rnorm(n = 20, mean = 50, sd = 4)
trilete = rnorm(n = 18, mean = 54, sd = 3.5)

# test normality - H0: je normalni, H1: neni normalni
shapiro.test(dvoulete) # 0.04 < 0.05
shapiro.test(trilete) # 0.04 < 0.05
# H0 zamitam, rozdeleni neni normalni

wilcox.test(x = dvoulete, y = trilete)
# p = 0.033 < 0.05 -> H0 zamitam
#_______________________________________________________________________

# 7)
set.seed(4631)
# H0: mereni jsou stejne presna
# H1: mereni nejsou stejne presna
moje_mereni = rnorm(n = 40, mean = 78, sd = 10)
kolega_mereni = rnorm(n = 40, mean = 78, sd = 15)

# test normality - H0: je normalni, H1: neni normalni
shapiro.test(moje_mereni) # 0.02 < 0.05
shapiro.test(kolega_mereni) # 0.12 > 0.05
# H0 zamitam, rozdeleni neni normalni

fligner.test(x = list(moje_mereni, kolega_mereni))
# p = 0.00078 < 0.05, H0 zamitam, mereni nejsou stejne presna
#_______________________________________________________________________

# 8)
set.seed(42)
# H0: vykony nebyly mensi nez 20 m
# H1: vykony byly mensi nez 20 m
strelci = rnorm(n = 80, mean = 19.5, sd = 2)

# test normality - H0: je normalni, H1: neni normalni
shapiro.test(strelci) # 0.15 > 0.05, rozdeleni je normalni

t.test(x = strelci, alternative = "less", mu = 20)

# p = 0.03 < 0.05, H0 se zamita, Bohousek ma pravdu
#_______________________________________________________________________

# 9) 
set.seed(42)
# H0: zito za tmy nevyrostlo (rozdil => 0)
# H1: zito za tmy vyrostlo (rozdil < 0)
prvni_mereni = rpois(n = 100, lambda = 35)
druhe_mereni = rpois(n = 100, lambda = 37)

# vybery jsou zavisle, pouzijeme parovy test
rozdil = prvni_mereni - druhe_mereni

# test normality - H0: je normalni, H1: neni normalni
shapiro.test(rozdil) # p = 0.16 > 0.05, je normalni

# muzeme pouzit parovy t-test
t.test(x = rozdil, alternative = "less") 
# p = 0.30 > 0.05, H0 se nezamita
#_______________________________________________________________________

# 10) 
# H0: nepostoupila
# H1: postoupila
set.seed(42)
ceta = rnorm(n = 20, mean = 3, sd = 0.1)
ceta2 = var(ceta) # vyberovy rozptyl

# test normality - H0: je normalni, H1: neni normalni
shapiro.test(ceta)

# testovaci statistika
varTest(x = ceta, alternative = "less", sigma.squared = 1)

# p hodnota
pchisq(q = TT, df = 19)
#_______________________________________________________________________

#11)
set.seed(2)
# H0: dostrel kmenu se nelisi
# H1: dostrel kmenu se lisi
kmen_A = rnorm(n = 20, mean = 50, sd = 10)
kmen_B = rnorm(n = 20, mean = 55, sd = 7)

# test normality - H0: je normalni, H1: neni normalni
shapiro.test(kmen_A) # p = 0.89 > 0.05, je normalni
shapiro.test(kmen_B) # p = 0.97 > 0.05, je normalni

var.test(x = kmen_A, y = kmen_B)
?var.test
# p = 0.29 > 0.05, H0 se nezamita, rozptyly jsou stejne

t.test(x = kmen_A, y = kmen_B, var.equal = T)
# p = 0.32 > 0.05, H0 se nezamita
#_______________________________________________________________________

# 12)
set.seed(6770)
# H0: rychlosti jsou stejne
# H1: rychlosti jsou ruzne
prirodni = rnorm(n = 20, mean = 7, sd = 1)
umely = rnorm(n = 20, mean = 6, sd = 2)

# zavislost --> parovy test
rozdil = prirodni - umely

# test normality - H0: je normalni, H1: neni normalni
shapiro.test(rozdil) # 0.0066 < 0.05, H0 se zamita
wilcox.test(x = rozdil)
# p = 0.0064 < 0.05, H0 se zamita, v rychlosti je rozdil
#_______________________________________________________________________

# 13)
set.seed(1)
# H0: inteligence ditete z vody je vetsi nebo stejna
# H1: inteligence ditete z vody je mensi
voda = rpois(n = 100, lambda = 95)
sous = rpois(n = 100, lambda = 105)

# zavislost --> parovy test
rozdil = sous - voda

# test normality - H0: je normalni, H1: neni normalni
shapiro.test(rozdil) # 0.16 > 0.05, je normalni

t.test(x = rozdil, alternative = "greater")
# p = =3.78e-9 < 0.05, H0 se zamita

#_______________________________________________________
#_______________________________________________________
set.seed(34)
nad = rpois(n = 45, lambda = 20)
pod = rpois(n = 45, lambda = 15)

# H0: delky ploutvi jsou stejne pod i nad zdrojem
# H1: delky ploutvi nejsou stejne pod i nad zdrojem

shapiro.test(nad) # p = 0.27 > 0.05
shapiro.test(pod) # p = 0.12 > 0.05

var.test(x = nad, y = pod) # 0.65 > 0.05

t.test(x = nad, y = pod, var.equal = T) # p = 1.614e-07 < 0.05 --> H0 se zamita
