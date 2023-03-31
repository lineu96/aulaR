dados <- read.csv("https://raw.githubusercontent.com/fernandomayer/data/master/milsa.csv")

names(dados) <- c("funcionario", "estado_civil", "instrucao", 
                  "filhos", "salario", "anos", "meses", "regiao")