# je pomer pohlavi ovlivnen teplotou v inkubatoru?

# teplota - 10x "nizka", 10x "vysoka"
# pohlavi - 20x nahodne "m" nebo "f"

  teplota = rep(c("nizka", "vysoka"), each = 10)
  pohlavi = sample(c("M", "F"), size = 20, replace = T)

# hypoteza
  # H0: teplota neovlivnuje pohlavi
  # H1: teplota ovlivnuje pohlavi
  
# tabulka pozorovanych hodnot
  xtabs(~ pohlavi + teplota) # vlnovka - neco zavisi na necem

# test
  chisq.test(xtabs(~ pohlavi + teplota))
  chisq.test(xtabs(~ pohlavi + teplota), correct = F) # correct nelze pouzit, kdyz je vic nez jeden stupen volnosti
  
  barplot(xtabs(~pohlavi+teplota), beside = T)
  # NEZAMITAME
  
# p-hodnota = 0.37

#____________________________________________________________________________________________________________________
# MORCATA
  # plemena: Seltie, Perueanec, Moher, Merino,Texel
  # barvy: bila, cerna, rezava, hneda, flekata
  
plemena = rep(c("Seltie", "Perueanec", "Moher", "Merino", "Texel"), each = 200)
barvy = sample(c("bila", "cerna", "rezava", "hneda", "flekata"), size = 1000, replace = T)
# H0: zastoupeni jsou stejna
# H1: zastoupeni jsou jina
  xtabs(~plemena+barvy)
  chisq.test(xtabs(~plemena+barvy))
  
# NEZAMITAME

barplot(xtabs(~barvy+plemena), beside = T) # barploty az na specificke pripady nevyuzivat, beside rozdeli sloupce

#____________________________________________________________________________________________________________________
# MYSI
# 40:30:20:15:3
# H0: zastoupeni barev je stejne jako # 40:30:20:15:3
# H1: zastoupeni se lisi od 40:30:20:15:3

  barvy = sample(c("svetle hneda", "hnedoseda", "sedohneda", "kremova", "besamelova"), size = 1000, replace = T)
  xtabs(~barvy) # seradi podle abecedy!!
  chisq.test(x = xtabs(~barvy), p = (c(3, 30, 15, 20, 40)/108)) # pozor na poradi v pomeru
  
# ZAMITAME!! - p-value je mensi nez 0.05
