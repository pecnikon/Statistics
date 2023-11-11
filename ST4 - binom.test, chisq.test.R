# z 15 kamaradu se poslednich komun. voleb zucastnilo 11, spocite konfidencni interval pro tento odhad
        p = 11/15 # bodovy odhad
        n = 25
        z = qnorm(p = 0.975) # musi byt mensi nez 0.025 nebo vetsi nez 0.975
    
    x1 = p - z * (((p*(1-p))/n)**(1/2)) # spodni hranice
    x2 = p + z * (((p*(1-p))/n)**(1/2)) # horni hranice

    
# lisi se volebni ucast od volebni ucasi v cele CR?
    # H0 (nulova hypoteza) = volebni ucast je stejna jako v CR
    # H1 (alternativni hypoteza) = volebni ucast neni stejna jako v CR
    
    ?binom.test
    binom.test(x = 11, n = 15, p = 0.45) # ve vysledku hledam p-value!! -> 0.03612

    # zamitnu hypotezu
    
    
# 100 odchycenych jedincu ( 32 cervenych, 20 zelenych, 19 modrych, 29 zlutych )
    #H0 = vsechny mutace se vyskytuji se stejnou frekvenci
    #H1 = mutace se nevyskytuji se stejnou frekvenci
    
        O = c(32, 20, 19, 29) # pozorovane hodnoty (observed)
        E = mean(O) # ocekavane (beru je z nulova hypoteza)
    
# testovaci statistika
    chi = ((O - E)**2)/E
    sum(chi) # -> 5.04
    
# p-hodnota
    pchisq(q = 5.04, df = 3) # df = degrees of freedom (stupne volnosti)
    
# kriticka hodnota
    qchisq(p = 0.95, df = 3)

#vypocet funkci
    chisq.test(x = O) # je to O ne nula!!!!
    
    # H0 relativni zastoupeni barev v populaci je 20:15:5:1
    # H1 relativni zastoupeni barev v populaci neni 20:15:5:1
    
    ?chisq.test
     vysledek = chisq.test(x = O, p = c(20, 15, 5, 1)/(20 + 15 + 5 + 1))
    # H0 zamitam, pomer je jiny nez 20:15:5:1  
    # # predpoklad je, ze chi kvadrat hodnota musi byt alespon 5
    
    
    
        