
#-----------------------------------------------------------------------
# Os comandos a seguir simulam duas variáveis qualitativas de 3 níveis
# cada

# Usando o R, obtenha a medida qui-quadrado de associação entre 
# variáveis qualitativas
#-----------------------------------------------------------------------
set.seed(20230331)
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

# SOLUÇÃO

esperados <- outer(total_coluna,
                   total_linha)/sum(tabela)

sum(((tabela - esperados)^2)/(esperados))

#-----------------------------------------------------------------------
