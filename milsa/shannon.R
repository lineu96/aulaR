
#-----------------------------------------------------------------------
set.seed(20230331)
#-----------------------------------------------------------------------

cenario1 <- sample(letters[1:5], 
            size = 10000, 
            replace = T, 
            prob = c(0.2,0.2,0.2,0.2,0.2))

cenario2 <- sample(letters[1:5], 
            size = 1000, 
            replace = T, 
            prob = c(0.4,0.3,0.2,0.07,0.03))

cenario3 <- sample(letters[1:5], 
            size = 1000, 
            replace = T, 
            prob = c(0.35,0.35,0.2,0.05,0.05))

cenario4 <- sample(letters[1:5], 
            size = 1000, 
            replace = T, 
            prob = c(0.8,0.05,0.05,0.05,0.05))

tabela_cenario1 <- table(cenario1)
tabela_cenario2 <- table(cenario2)
tabela_cenario3 <- table(cenario3)
tabela_cenario4 <- table(cenario4)

par(mfrow = c(2,2))
barplot(tabela_cenario1, main = "cenário 1")
barplot(tabela_cenario2, main = "cenário 2")
barplot(tabela_cenario3, main = "cenário 3")
barplot(tabela_cenario4, main = "cenário 4")
