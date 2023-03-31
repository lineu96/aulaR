
dados <- read.csv("https://raw.githubusercontent.com/fernandomayer/data/master/milsa.csv")

names(dados) <- c("funcionario", "estado_civil", "instrucao", 
"filhos", "salario", "anos", "meses", "regiao")

head(dados)
summary(dados)
names(dados)

dados$estado_civil

table(dados$estado_civil)
tabela1 <- table(dados$estado_civil)
tabela1
sum(tabela1)

prop.table(tabela1)
tabela2 <- prop.table(tabela1)
tabela2
sum(tabela2)

tabela2*100
tabela3 <- tabela2*100
tabela3
sum(tabela3)

plot(tabela1)
plot(tabela2)
plot(tabela3)

barplot(tabela1)
barplot(tabela2)
barplot(tabela3)

barplot(tabela1, horiz=T)
barplot(tabela2, horiz=T)
barplot(tabela3, horiz=T)

pie(tabela1)
pie(tabela2)
pie(tabela3)

tabela4 <- table(dados$estado_civil, rep(1,36))
barplot(tabela4)
barplot(tabela4, horiz = T)

tabela5 <- prop.table(tabela4)
barplot(tabela4)
barplot(tabela4, horiz = T)

tabela6 = data.frame(niveis = names(tabela1),
                     freq = as.vector(tabela1),
                     freq_r = as.vector(tabela2))

tabela6
tabela6[3,1] <- "TOTAL"
tabela6[3,2] <- sum(tabela6$freq, na.rm = T)
tabela6[3,3] <- sum(tabela6$freq_r, na.rm = T)
tabela6
