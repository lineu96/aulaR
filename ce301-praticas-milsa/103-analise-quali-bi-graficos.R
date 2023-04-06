

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

tabela <- table(dados$estado_civil,
                dados$instrucao)

teste <- chisq.test(tabela)

#-----------------------------------------------------------------------

tabela <- as.data.frame(tabela)

tabela$freq_r <- tabela$Freq/sum(tabela$Freq)
tabela$freq_r <- round(tabela$freq_r, 2)

ggplot(tabela, 
       aes(x=Var1, y=Freq, fill=Var2)) + 
  geom_bar(stat = 'identity',
           col = 1,
           lwd = 1, 
           position = 'dodge') +  
  ylim(c(0, 
         (max(tabela$Freq)+ (max(tabela$Freq)*0.1))))+
  geom_text(aes(label = Freq),
            #hjust=-0.1, 
            color=1, 
            size=8, 
            position=position_dodge(width=0.9),
            vjust=-0.5
  )+
  xlab("") +
  ylab("Frequência") +
  #ggtitle("Situação de emprego x Expectativa após graduação")+
  theme_classic() + theme(legend.position = 'top',
                          plot.title = element_text(family = "Helvetica", 
                                                    face = "bold", 
                                                    size = (20),
                                                    hjust = 0.5),
                          axis.title = element_text(face = "bold",
                                                    size = 15),
                          #axis.text = element_blank(),
                          #legend.title = element_blank(),
                          text = element_text(size=15)) 

#-----------------------------------------------------------------------

ggplot(tabela, 
       aes(x=Var2, y=Freq, fill=Var1)) + 
  geom_bar(stat = 'identity',
           col = 1,
           lwd = 1, 
           position = 'dodge') +  
  ylim(c(0, 
         (max(tabela$Freq)+ (max(tabela$Freq)*0.1))))+
  geom_text(aes(label = Freq),
            #hjust=-0.1, 
            color=1, 
            size=8, 
            position=position_dodge(width=0.9),
            vjust=-0.5
  )+
  xlab("") +
  ylab("Frequência") +
  #ggtitle("Situação de emprego x Expectativa após graduação")+
  theme_classic() + theme(legend.position = 'top',
                          plot.title = element_text(family = "Helvetica", 
                                                    face = "bold", 
                                                    size = (20),
                                                    hjust = 0.5),
                          axis.title = element_text(face = "bold",
                                                    size = 15),
                          #axis.text = element_blank(),
                          #legend.title = element_blank(),
                          text = element_text(size=15)) 
#-----------------------------------------------------------------------

ggplot(tabela, 
       aes(x=Var1, y=Freq, fill=Var2)) + 
  geom_bar(stat = 'identity',
           col = 1,
           lwd = 1)+ 
  #  geom_text(aes(label = freq_r),
  #            hjust=0.5, 
  #            color=1, 
  #            size=8, 
  #            position=position_stack(), 
  #            vjust=1)+
  xlab("") +
  ylab("Frequência") +
  #ggtitle("Situação de emprego x Expectativa após graduação")+
  theme_classic() + theme(legend.position = 'top',
                          plot.title = element_text(family = "Helvetica", 
                                                    face = "bold", 
                                                    size = (20),
                                                    hjust = 0.5),
                          axis.title = element_text(face = "bold",
                                                    size = 15),
                          #axis.text = element_blank(),
                          #legend.title = element_blank(),
                          text = element_text(size=15)) 

#-----------------------------------------------------------------------

ggplot(tabela, 
       aes(x=Var2, y=Freq, fill=Var1)) + 
  geom_bar(stat = 'identity',
           col = 1,
           lwd = 1)+ 
  #  geom_text(aes(label = freq_r),
  #            hjust=0.5, 
  #            color=1, 
  #            size=8, 
  #            position=position_stack(), 
  #            vjust=1)+
  xlab("") +
  ylab("Frequência") +
  #ggtitle("Situação de emprego x Expectativa após graduação")+
  theme_classic() + theme(legend.position = 'top',
                          plot.title = element_text(family = "Helvetica", 
                                                    face = "bold", 
                                                    size = (20),
                                                    hjust = 0.5),
                          axis.title = element_text(face = "bold",
                                                    size = 15),
                          #axis.text = element_blank(),
                          #legend.title = element_blank(),
                          text = element_text(size=15)) 

#-----------------------------------------------------------------------

table2 <- as.matrix(
  prop.table(table(
    dados$instrucao,
    dados$estado_civil), 
    mar = 2))

table2 <- as.data.frame(table2)

ggplot(table2, 
       aes(x=Var2, y=Freq, fill=Var1)) + 
  geom_bar(stat = 'identity',
           col = 1,
           lwd = 1)+ 
  #  geom_text(aes(label = freq_r),
  #            hjust=0.5, 
  #            color=1, 
  #            size=8, 
  #            position=position_stack(), 
  #            vjust=1)+
  xlab("") +
  ylab("Proporção") +
  #ggtitle("Situação de emprego x Expectativa após graduação")+
  theme_classic() + theme(legend.position = 'top',
                          plot.title = element_text(family = "Helvetica", 
                                                    face = "bold", 
                                                    size = (20),
                                                    hjust = 0.5),
                          axis.title = element_text(face = "bold",
                                                    size = 15),
                          #axis.text = element_blank(),
                          #legend.title = element_blank(),
                          text = element_text(size=15)) 


#-----------------------------------------------------------------------

table2 <- as.matrix(
  prop.table(table(
    dados$estado_civil,
    dados$instrucao), 
    mar = 2))

table2 <- as.data.frame(table2)

ggplot(table2, 
       aes(x=Var2, y=Freq, fill=Var1)) + 
  geom_bar(stat = 'identity',
           col = 1,
           lwd = 1)+ 
  #  geom_text(aes(label = freq_r),
  #            hjust=0.5, 
  #            color=1, 
  #            size=8, 
  #            position=position_stack(), 
  #            vjust=1)+
  xlab("") +
  ylab("Proporção") +
  #ggtitle("Situação de emprego x Expectativa após graduação")+
  theme_classic() + theme(legend.position = 'top',
                          plot.title = element_text(family = "Helvetica", 
                                                    face = "bold", 
                                                    size = (20),
                                                    hjust = 0.5),
                          axis.title = element_text(face = "bold",
                                                    size = 15),
                          #axis.text = element_blank(),
                          #legend.title = element_blank(),
                          text = element_text(size=15)) 

#-----------------------------------------------------------------------

ggplot(data = dados) +
  geom_mosaic(aes(x = product(estado_civil, instrucao), 
                  fill=estado_civil)) +
  xlab("") +
  ylab("") +
  #ggtitle("Gráfico de mosaico para\n 2 variáveis categóricas")+
  theme_classic() + theme(legend.position = 'top',
                          plot.title = element_text(family = "Helvetica", 
                                                    face = "bold", 
                                                    size = (20),
                                                    hjust = 0.5),
                          axis.title = element_text(face = "bold",
                                                    size = 15),
                          #axis.text = element_blank(),
                          #legend.title = element_blank(),
                          text = element_text(size=15)) 

#-----------------------------------------------------------------------

ggplot(data = dados) +
  geom_mosaic(aes(x = product(instrucao, estado_civil), 
                  fill=instrucao)) +
  xlab("") +
  ylab("") +
  #ggtitle("Gráfico de mosaico para\n 2 variáveis categóricas")+
  theme_classic() + theme(legend.position = 'top',
                          plot.title = element_text(family = "Helvetica", 
                                                    face = "bold", 
                                                    size = (20),
                                                    hjust = 0.5),
                          axis.title = element_text(face = "bold",
                                                    size = 15),
                          #axis.text = element_blank(),
                          #legend.title = element_blank(),
                          text = element_text(size=15)) 

#-----------------------------------------------------------------------
