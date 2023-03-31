
#-----------------------------------------------------------------------
set.seed(20230331)
#-----------------------------------------------------------------------

a <- sample(letters[1:5], 
            size = 1000, 
            replace = T, 
            prob = c(0.2,0.2,0.2,0.2,0.2))

b <- sample(letters[1:5], 
            size = 1000, 
            replace = T, 
            prob = c(0.4,0.3,0.2,0.07,0.03))

c <- sample(letters[1:5], 
            size = 1000, 
            replace = T, 
            prob = c(0.35,0.35,0.2,0.05,0.05))

d <- sample(letters[1:5], 
            size = 1000, 
            replace = T, 
            prob = c(0.8,0.05,0.05,0.05,0.05))

table(a)
table(b)
table(c)
table(d)

barplot(table(a))
barplot(table(b))
barplot(table(c))
barplot(table(d))

#-----------------------------------------------------------------------

e <- sample(letters[1:3], size = 100, replace = T)
f <- sample(letters[4:6], size = 100, replace = T)

tabela <- table(e,f)
tabela

total_linha <- margin.table(tabela,2)
total_coluna <- margin.table(tabela,1)
total_linha
total_coluna

tabela_temp <- rbind(tabela, total_linha)
tabela_temp
tabela_final <- cbind(tabela_temp, c(total_coluna, sum(total_coluna)))
tabela_final
colnames(tabela_final)[4] <- "total_coluna"
tabela_final

total_linha
total_coluna

#-----------------------------------------------------------------------
