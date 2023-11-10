#pocet mrtvych na stisknuti spouste
#15 cisel 0 a 1 s pravdepodobnostmi 5/6 a 1/6

#Bi(n=15, p=1/6)

?sample
sample(c(0,1), 15, replace = T, prob=c(5/6,1/6))

#k=3, n=15 factorial()

pk=(factorial(n)/(factorial(k)*factorial(n-k)))*(((1/6)**k)*(1-(1/6))**(n-k))
sum(pk)
barplot(pk, 
        names=0:15, 
        main="Pocet mrtvol na 15 pokusu", 
        ylab="Pravdepodobnost", 
        col = "darkgreen")

#200 hodnot z Bi(n=15, p=1/6)

k=0:15

nv=sample(0:15, 200, replace = T, prob=pk)
xtabs(~nv)
barplot(xtabs(~nv)/200)

sum(nv)/(length(nv))
mean(nv)    

#median je stredni hodnota

x=sort(nv)
mean(x[c(100,101)])

median(nv) #median je 50% kvantil
quantile(x=nv, probs = 0.5)

xtabs(~nv) #modus

min(nv)
max(nv)

#miry variability

#rozptyl
sum((nv-mean(nv))**2)/(length(nv))
var(nv)

#smerodatna odchylka
sqrt(var(nv))
sd(nv)

#stredni chyba prumeru
sd(nv)/sqrt(200)

