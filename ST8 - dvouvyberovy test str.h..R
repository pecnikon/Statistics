# n = 40 lokalit pred a po managementovem zasahu
# pred z Po(lambda = 20)
# po z Po(lambda = 17)
set.seed(13)
pred = rpois(n = 40,lambda = 20)
po = rpois(n = 40, lambda = 17)
# ______________________________________________________________________________
# PAROVY TEST
# Na 40 lokalitach byla sledovana pocetnost bobru pred a po reintrodukci vlku
# Ma pritomnost vlku vliv na pocetnost bobru?

# vybery jsou zavisle, pouzijeme parovy test
rozdil = pred - po

# predpoklad pouziti paroveho t testu je normalni rozdeleni ROZDILU 
# H0: rozdeleni je normalni 
# H1: rozdeleni je jine nez normalni
shapiro.test(rozdil) # p = 0.07 > 0.05 takze je normalni

# muzeme pouzit parovy t-test
# H0: pritomnost vlku neovlivnuje pocetnost bobru (rozdil = 0)
# H1: pritomnost vlku ovlivnuje pocetnost bobru (rozdil!=0)

t.test(x = rozdil) # mu je automaticky 0
# p-value = 0.0496 < 0.05 tak se H0 zamita, kdyz je t kladne, znamena to, ze se pocetnost snizila
# pocetnost bobru je ovlivnena pritomnosti vlku

# predpokladejme, ze rozdeleni rozdilu neni normalni a ze chceme zjistit zda nedoslo k poklesu
# H0: k poklesu nedoslo (rozdil <= 0)
# H1:k poklesu doslo (rozdil > 0)
wilcox.test(x = rozdil, alternative = "greater", mu = 0, exact = F) # exact odstrani warning message
# p-value = 0.405 < 0.05 , H0 zamitame
# stupne volnosi pro t-test je 39
# stupne volnosti pro wilcox.test je 40

boxplot(rozdil, notch = T) # notch udela intervalovy odhad medianu

# ______________________________________________________________________________
# DVOUVYBEROVE TESTY  
# Zmerili jsme pocetnost bobru na 40 lokalitach kde vlk jeste neni a na dalsich 40 lokalitach, kde uz je
# ma pritomnost vlku vliv na pocetnost bobru?
# veliciny jsou nezavisle
# H0: pritomnost vlku neovlivnuje pocetnost bobru
# H1: pritomnost vlku ovlivnuje pocetnost bobru
bezvlk = pred
svlk = po

# normalita?
# H0: rozdeleni je normalni 
# H1: rozdeleni je jine nez normalni
shapiro.test(bezvlk) # 0.04 < 0.05
shapiro.test(svlk) # 0.31 > 0.05
# rozdeleni je normalni jen u jednoho -> DVOUVYBEROVY WILCOXONUV TEST  

wilcox.test(x = bezvlk, y = svlk)
# p-value = 0.06 > 0.05, H0 se nezamita

# predpokladejme, ze obe veliciny pochazi z normalniho rozdeleni
# rovnost rozptylu?
# H0: rozptyly jsou stejne
# H1: rozptyly nejsou stejne
var.test(x = bezvlk, y = svlk)
# 0.95 > 0.05 , H0 nezamitame
# splnili jsme predpoklady pro dvouvyberovy t-test
t.test(x = bezvlk, y = svlk, var.equal = T) # var.equal - kdyz je T, rozptyly jsou stejne a kdyz F, jsou ruzne
# p = 0.04 < 0.05, H0 se zamita, pritomnost vlku ovlivnuje pocetnost bobru

# predpokladejme, ze je rozdeleni obou vyberu normalni, ale rozptyl se lisi
# zjistete, zda doslo na lokalitach s vlkem k poklesu pocetnosti

# H0: k poklesu nedoslo (bezvlk <= svlk)
# H1: k poklesu doslo (bezvlk > svlk)
?t.test
t.test(x = bezvlk, y = svlk, var.equal = F, alternative = "greater")
# pocetnost vyznamne klesla

# stupne volnosti
# df pro dvouvyberovy t test je 78
# df pro welchuv t test je 77,993
# df pro dvouvyberovy wilcoxonuv t test je 40 a 40
