
#-----------------------------------------------------------------------
# Os comandos a seguir mostram diversas técnicas vistas em aula para
# análise exploratória univariadas de variáveis quantitativas

# Execute os comandos, discuta o que eles fazem, comente o código e 
# busque maneiras de customizar os gráficos e tabelas
#-----------------------------------------------------------------------

dados <- read.csv("https://raw.githubusercontent.com/fernandomayer/data/master/milsa.csv")

names(dados) <- c("funcionario", "estado_civil", "instrucao", 
                  "filhos", "salario", "anos", "meses", "regiao")

names(dados)

dados$salario

breaks <- seq(4,24,2)

classes <- cut(dados$salario, 
               breaks = breaks, 
               include.lowest = TRUE, 
               right = TRUE)

table(classes)
tabela15 <- table(classes)
tabela15
sum(tabela15)

prop.table(tabela15)
tabela16 <- prop.table(tabela15)
tabela16
sum(tabela16)

tabela16*100
tabela17 <- tabela16*100
tabela17
sum(tabela17)

mean(dados$salario)
median(dados$salario)

tabela_salarios <- as.data.frame(table(dados$salario))
subset(tabela_salarios, Freq == max(tabela_salarios$Freq))

quantile(dados$salario)

aic <- quantile(dados$salario)[4] - quantile(dados$salario)[2]

quantile(dados$salario, seq(0,1,0.1))

hist(dados$salario)
hist(dados$salario, probability = T)

densidade <- density(dados$salario)
plot(densidade)

hist(dados$salario, probability = T)
lines(densidade)

boxplot(dados$salario)
boxplot(dados$salario, horizontal = T)

#-----------------------------------------------------------------------
