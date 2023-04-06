
#-----------------------------------------------------------------------
# Os comandos a seguir mostram diversas técnicas vistas em aula para
# análise exploratória bivariada de variáveis qualitativas

# Execute os comandos, discuta o que eles fazem, comente o código e 
# busque maneiras de customizar os gráficos e tabelas
#-----------------------------------------------------------------------

dados <- read.csv("https://raw.githubusercontent.com/fernandomayer/data/master/milsa.csv")

names(dados) <- c("funcionario", "estado_civil", 
                  "instrucao", "filhos", "salario", 
                  "anos", "meses", "regiao")

head(dados)
summary(dados)
names(dados)

dados$estado_civil
dados$instrucao

table(dados$estado_civil, dados$instrucao)
tabela7 <- table(dados$estado_civil, 
                 dados$instrucao)
tabela7
sum(tabela7)

prop.table(tabela7)
tabela8 <- prop.table(tabela7)
tabela8
sum(tabela8)

tabela8*100
tabela9 <- tabela8*100
tabela9
sum(tabela9)

plot(tabela7)
plot(tabela8)
plot(tabela9)

barplot(tabela7, legend.text = rownames(tabela7), beside = T)
barplot(tabela8, legend.text = rownames(tabela8), beside = T)
barplot(tabela9, legend.text = rownames(tabela8), beside = T)

barplot(t(tabela7), legend.text = colnames(tabela7), beside = T)
barplot(t(tabela8), legend.text = colnames(tabela8), beside = T)
barplot(t(tabela9), legend.text = colnames(tabela9), beside = T)

barplot(tabela7, legend.text = rownames(tabela7))
barplot(tabela8, legend.text = rownames(tabela8))
barplot(tabela9, legend.text = rownames(tabela8))

barplot(t(tabela7), legend.text = colnames(tabela7))
barplot(t(tabela8), legend.text = colnames(tabela8))
barplot(t(tabela9), legend.text = colnames(tabela9))

barplot(tabela7, horiz=T, legend.text = rownames(tabela7))
barplot(tabela8, horiz=T, legend.text = rownames(tabela8))
barplot(tabela9, horiz=T, legend.text = rownames(tabela9))

barplot(t(tabela7), horiz=T, legend.text = colnames(tabela7))
barplot(t(tabela8), horiz=T, legend.text = colnames(tabela8))
barplot(t(tabela9), horiz=T, legend.text = colnames(tabela9))

total_linha <- margin.table(tabela7,2)
total_coluna <- margin.table(tabela7,1)
total_linha
total_coluna

tabela7
tabela10_temp <- rbind(tabela7, total_linha)
tabela10_temp
tabela10_final <- cbind(tabela10_temp, c(total_coluna, sum(total_coluna)))
tabela10_final
colnames(tabela10_final)
colnames(tabela10_final)[4] <- "total_coluna"
tabela10_final

#-----------------------------------------------------------------------

prop.table(tabela7, margin = 1)
tabela11 <- prop.table(tabela7, margin = 1)
tabela11
sum(tabela11)

tabela11[1,]
sum(tabela11[1,])

tabela11[2,]
sum(tabela11[2,])

barplot(t(tabela11), legend.text = colnames(tabela11))

total_linha <- margin.table(tabela11,2)
total_coluna <- margin.table(tabela11,1)
total_linha
total_coluna

tabela11
tabela12_temp <- rbind(tabela11, total_linha)
tabela12_temp
tabela12_final <- cbind(tabela12_temp, c(total_coluna, sum(total_coluna)))
tabela12_final
colnames(tabela12_final)
colnames(tabela12_final)[4] <- "total_coluna"
tabela12_final

#-----------------------------------------------------------------------

prop.table(tabela7, margin = 2)
tabela13 <- prop.table(tabela7, margin = 2)
tabela13
sum(tabela13)

tabela13[,1]
sum(tabela13[,1])

tabela13[,2]
sum(tabela13[,2])

tabela13[,3]
sum(tabela13[,3])

barplot(tabela13, legend.text = row.names(tabela13))

total_linha <- margin.table(tabela13,2)
total_coluna <- margin.table(tabela13,1)
total_linha
total_coluna

tabela13
tabela14_temp <- rbind(tabela13, total_linha)
tabela14_temp
tabela14_final <- cbind(tabela14_temp, c(total_coluna, sum(total_coluna)))
tabela14_final
colnames(tabela14_final)
colnames(tabela14_final)[4] <- "total_coluna"
tabela14_final

#-----------------------------------------------------------------------
