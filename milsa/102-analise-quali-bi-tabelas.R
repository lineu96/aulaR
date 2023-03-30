
#-----------------------------------------------------------------------

## livro Estatística Básica de W. Bussab e P. Morettin 
## traz no primeiro capítulo um conjunto de dados hipotético 
## de atributos de 36 funcionários da companhia Milsa

#-----------------------------------------------------------------------
library(tidyverse)
#-----------------------------------------------------------------------

dados <- read.csv("milsa.csv")

summary(dados)
names(dados)

#-----------------------------------------------------------------------

dados$estado_civil
dados$instrucao
dados$regiao

#-----------------------------------------------------------------------

tabela <- table(dados$estado_civil,
                dados$instrucao)

tabela

total_linha <- margin.table(tabela,2)
total_coluna <- margin.table(tabela,1)

total_linha
total_coluna

tabela_com_totais <-rbind(cbind(tabela,total_coluna),
                          c(total_linha, sum(total_coluna)))


dimnames(tabela_com_totais)[[1]][3] <- "Total linha"
dimnames(tabela_com_totais)[[2]][4] <- "Total coluna"

tabela_com_totais

#-----------------------------------------------------------------------

tabela <- round(prop.table(tabela),4)

tabela

total_linha <- margin.table(tabela,2)
total_coluna <- margin.table(tabela,1)

total_linha
total_coluna

tabela_com_totais <-rbind(cbind(tabela,total_coluna),
                          c(total_linha, sum(total_coluna)))


dimnames(tabela_com_totais)[[1]][3] <- "Total linha"
dimnames(tabela_com_totais)[[2]][4] <- "Total coluna"

tabela_com_totais

#-----------------------------------------------------------------------

tabela <- round(prop.table(tabela,2),3)

tabela

total_linha <- margin.table(tabela,2)
total_coluna <- margin.table(tabela,1)

total_linha
total_coluna

tabela_com_totais <-rbind(cbind(tabela,total_coluna),
                          c(total_linha, sum(total_coluna)))


dimnames(tabela_com_totais)[[1]][3] <- "Total linha"
dimnames(tabela_com_totais)[[2]][4] <- "Total coluna"

tabela_com_totais

#-----------------------------------------------------------------------

tabela <- round(prop.table(tabela,1),3)

tabela

total_linha <- margin.table(tabela,2)
total_coluna <- margin.table(tabela,1)

total_linha
total_coluna

tabela_com_totais <-rbind(cbind(tabela,total_coluna),
                          c(total_linha, sum(total_coluna)))


dimnames(tabela_com_totais)[[1]][3] <- "Total linha"
dimnames(tabela_com_totais)[[2]][4] <- "Total coluna"

tabela_com_totais

#-----------------------------------------------------------------------
