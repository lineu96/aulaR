
#-----------------------------------------------------------------------
# Simulação moeda
sorteio <- sample(x = c("cara", "coroa"), 
                  size = 10, 
                  replace = T)

plot(prop.table(table(sorteio)))
abline(h = 1/2, col = 2, lwd = 2)

#-----------------------------------------------------------------------
# Simulação dado
sorteio <- sample(x = 1:6, 
                  size = 10, 
                  replace = T)

plot(prop.table(table(sorteio)))
abline(h = 1/6, col = 2, lwd = 2)

#-----------------------------------------------------------------------
# Simulação dado 12 faces
sorteio <- sample(x = 1:12, 
                  size = 10, 
                  replace = T)

plot(prop.table(table(sorteio)))
abline(h = 1/12, col = 2, lwd = 2)

#-----------------------------------------------------------------------