

#uloha 1
¨
set.seed(105)
predtim=rnorm(n=30,mean = 4.45,sd=1.1)
potom=rnorm(n=30,mean=4.55,sd=1.25)
 
#H0: počet parkovacich mist se nezvysil
#H1: pocet parkovacich mist se zvysil
rozdil=predtim-potom
#rozdil je mensi nez 0

shapiro.test(rozdil)
#p-value = 0.8218 - nezamítám 
#rozdeleni je noramlni
wilcox.test(rozdil, alternative = "less")
t.test(rozdil, alternative = "less", mu = 0) #ze zadani - podle toho jak jsem udelala hypotezu
#p-value = 0.1894 > 0.05 - nezamitam
#pocet parkovacich mist se nezvysil

mean(predtim)-mean(potom)
d=mean(rozdil)
# -0.2164689 

library(pwr)
pwr.t.test(n=30,d=d,type="paired",alternative = "less")
#power = 0.3131816

#_______________________________________________________________________________

#2.priklad

set.seed(12)
#dvouvyber
letos=rpois(n=15,lambda = 2)
vloni=rpois(n=15,lambda=3)

#H0: tombola byla bohatsi letos
#H1: tombola byla bohatsi vloni

shapiro.test(letos)
#p-value - 0.009 - zamitam
shapiro.test(vloni)
#p-value - 0.0340 - zamitam
#rozdeleni je nenormalni

wilcox.test(letos, alternative = "less", vloni) #jako veta - letos byla min nez vloni
wilcox.test(vloni, alternative = "greater", letos)
#0.002
#stupne volnosti = 30

#sila testu
pwr.t.test(n=15,d=v,type = "two.sample",alternative = "less")
#power=0.7399
n=(sd(letos)+sd(vloni))/2
v=(2-3)/n



