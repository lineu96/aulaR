
#-----------------------------------------------------------------------

## livro Estatística Básica de W. Bussab e P. Morettin 
## traz no primeiro capítulo um conjunto de dados hipotético 
## de atributos de 36 funcionários da companhia Milsa

#-----------------------------------------------------------------------

dados <- read.csv("milsa.csv")

summary(dados)
names(dados)

#-----------------------------------------------------------------------

tabela1 <- dados %>%
  group_by(estado_civil) %>%
  summarize(Frequência = n()) %>%
  mutate(`Freq. Relativa` = round(Frequência/sum(Frequência),2))

tabela1 <- 
  tabela1 %>% 
  add_row(estado_civil = 'Total', 
          Frequência = sum(tabela1$Frequência),
          `Freq. Relativa` = sum(tabela1$`Freq. Relativa`)
  )

names(tabela1)[1] <- "Estado civil" 

#-----------------------------------------------------------------------

knitr::kable(tabela1, 
             caption = "Tabela de frequências para o estado civil", 
             align ="ccc" )

#-----------------------------------------------------------------------

ggplot(data=tabela1[-nrow(tabela1),], 
       aes(x=`Estado civil`, 
           y=Frequência#, 
           #fill = Niveis
       )) + 
  geom_bar(stat="identity", 
           col=1,
           lwd=1)+
  ylim(c(0, 
         (max(tabela1[-nrow(tabela1),]$Frequência)+ (max(tabela1[-nrow(tabela1),]$Frequência)*0.2))))+
  geom_text(aes(label=Frequência), 
            vjust=-1, 
            color=1, 
            size=8)+
  ylab("Frequência") +
  xlab("") +
  ggtitle("Gráfico")+
  theme_classic() + theme(#legend.position = 'bottom',
    plot.title = element_text(family = "Helvetica", 
                              face = "bold", 
                              size = (20),
                              hjust = 0.5),
    axis.title = element_text(face = "bold",
                              size = 15),
    #axis.text = element_blank(),
    legend.title = element_blank(),
    text = element_text(size=15)) 

#-----------------------------------------------------------------------

ggplot(data=tabela1[-nrow(tabela1),], aes(x=1, 
                                          y=`Freq. Relativa`, 
                                          fill = `Estado civil`
)) + 
  geom_col(col = 1) +
  geom_text(aes(label = `Freq. Relativa`),
            position = position_stack(vjust = 0.5),
            size = 8) +
  ylab("Proporção") +
  xlab("") +
  ggtitle("Gráfico")+
  theme_classic() + theme(#legend.position = 'bottom',
    plot.title = element_text(family = "Helvetica", 
                              face = "bold", 
                              size = (20),
                              hjust = 0.5),
    axis.title = element_text(face = "bold",
                              size = 15),
    axis.text.x = element_blank(),
    legend.title = element_blank(),
    text = element_text(size=15))  +
  scale_fill_brewer(palette="Paired")

#-----------------------------------------------------------------------

tabela1 <- tabela1[-nrow(tabela1),] %>% 
  arrange(desc(`Estado civil`)) %>%
  mutate(ypos = cumsum(`Freq. Relativa`)- 0.5*`Freq. Relativa`)

ggplot(tabela1, 
       aes(x="", y=`Freq. Relativa`, fill=`Estado civil`)) +
  geom_bar(stat="identity", 
           width=1,
           col = 1,
           lwd = 1) +
  coord_polar("y", start=0) +
  theme_classic() + 
  #theme(legend.position="none") +
  geom_text(aes(y = ypos, label = `Freq. Relativa`), 
            color = 1, 
            size = 5)+
  ylab("") +
  xlab("") +
  ggtitle("Gráfico")+
  theme(legend.position = 'right',
        plot.title = element_text(family = "Helvetica", 
                                  face = "bold", 
                                  size = (20),
                                  hjust = 0.5),
        axis.title = element_text(face = "bold",
                                  size = 15),
        axis.text = element_blank(),
        legend.title = element_blank(),
        text = element_text(size=20)) +
  scale_fill_brewer(palette="Paired")

#-----------------------------------------------------------------------

tabela3 <- dados %>%
  group_by(regiao) %>%
  summarize(Frequência = n()) %>%
  mutate(`Freq. Relativa` = round(Frequência/sum(Frequência),2))

tabela3 <- 
  tabela3 %>% 
  add_row(regiao = 'Total', 
          Frequência = sum(tabela1$Frequência),
          `Freq. Relativa` = sum(tabela1$`Freq. Relativa`)
  )

names(tabela3)[1] <- "Região" 

#-----------------------------------------------------------------------

knitr::kable(tabela3, 
             caption = "Tabela de frequências para a região", 
             align ="ccc" )

#-----------------------------------------------------------------------

ggplot(data=tabela3[-nrow(tabela3),], 
       aes(x=Região, 
           y=Frequência#, 
           #fill = Niveis
       )) + 
  geom_bar(stat="identity", 
           col=1,
           lwd=1)+
  ylim(c(0, 
         (max(tabela3[-nrow(tabela3),]$Frequência)+ (max(tabela3[-nrow(tabela3),]$Frequência)*0.2))))+
  geom_text(aes(label=Frequência), 
            vjust=-1, 
            color=1, 
            size=8)+
  ylab("Frequência") +
  xlab("") +
  ggtitle("Gráfico")+
  theme_classic() + theme(#legend.position = 'bottom',
    plot.title = element_text(family = "Helvetica", 
                              face = "bold", 
                              size = (20),
                              hjust = 0.5),
    axis.title = element_text(face = "bold",
                              size = 15),
    #axis.text = element_blank(),
    legend.title = element_blank(),
    text = element_text(size=15)) 

#-----------------------------------------------------------------------

ggplot(data=tabela3[-nrow(tabela3),], aes(x=1, 
                                          y=`Freq. Relativa`, 
                                          fill = Região
)) + 
  geom_col(col = 1) +
  geom_text(aes(label = `Freq. Relativa`),
            position = position_stack(vjust = 0.5),
            size = 8) +
  ylab("Proporção") +
  xlab("") +
  ggtitle("Gráfico")+
  theme_classic() + theme(#legend.position = 'bottom',
    plot.title = element_text(family = "Helvetica", 
                              face = "bold", 
                              size = (20),
                              hjust = 0.5),
    axis.title = element_text(face = "bold",
                              size = 15),
    axis.text.x = element_blank(),
    legend.title = element_blank(),
    text = element_text(size=15))  +
  scale_fill_brewer(palette="Paired")

#-----------------------------------------------------------------------

tabela3 <- tabela3[-nrow(tabela3),] %>% 
  arrange(desc(Região)) %>%
  mutate(ypos = cumsum(`Freq. Relativa`)- 0.5*`Freq. Relativa`)

ggplot(tabela3, 
       aes(x="", y=`Freq. Relativa`, fill=Região)) +
  geom_bar(stat="identity", 
           width=1,
           col = 1,
           lwd = 1) +
  coord_polar("y", start=0) +
  theme_classic() + 
  #theme(legend.position="none") +
  geom_text(aes(y = ypos, label = `Freq. Relativa`), 
            color = 1, 
            size = 5)+
  ylab("") +
  xlab("") +
  ggtitle("Gráfico")+
  theme(legend.position = 'right',
        plot.title = element_text(family = "Helvetica", 
                                  face = "bold", 
                                  size = (20),
                                  hjust = 0.5),
        axis.title = element_text(face = "bold",
                                  size = 15),
        axis.text = element_blank(),
        legend.title = element_blank(),
        text = element_text(size=20)) +
  scale_fill_brewer(palette="Paired")

#-----------------------------------------------------------------------

tabela2 <- dados %>%
  group_by(instrucao) %>%
  summarize(Frequência = n()) %>%
  mutate(`Freq. Relativa` = round(Frequência/sum(Frequência),2))

tabela2 <- 
  tabela2 %>% 
  add_row(instrucao = 'Total', 
          Frequência = sum(tabela1$Frequência),
          `Freq. Relativa` = sum(tabela1$`Freq. Relativa`)
  )

names(tabela2)[1] <- "Instrução" 

#-----------------------------------------------------------------------

knitr::kable(tabela2, 
             caption = "Tabela de frequências para o grau de instrução", 
             align ="ccc" )

#-----------------------------------------------------------------------

ggplot(data=tabela2[-nrow(tabela2),], 
       aes(x=Instrução, 
           y=Frequência#, 
           #fill = Niveis
       )) + 
  geom_bar(stat="identity", 
           col=1,
           lwd=1)+
  ylim(c(0, 
         (max(tabela2[-nrow(tabela2),]$Frequência)+ (max(tabela2[-nrow(tabela2),]$Frequência)*0.2))))+
  geom_text(aes(label=Frequência), 
            vjust=-1, 
            color=1, 
            size=8)+
  ylab("Frequência") +
  xlab("") +
  ggtitle("Gráfico")+
  theme_classic() + theme(#legend.position = 'bottom',
    plot.title = element_text(family = "Helvetica", 
                              face = "bold", 
                              size = (20),
                              hjust = 0.5),
    axis.title = element_text(face = "bold",
                              size = 15),
    #axis.text = element_blank(),
    legend.title = element_blank(),
    text = element_text(size=15)) 

#-----------------------------------------------------------------------

ggplot(data=tabela2[-nrow(tabela2),], aes(x=1, 
                                          y=`Freq. Relativa`, 
                                          fill = Instrução
)) + 
  geom_col(col = 1) +
  geom_text(aes(label = `Freq. Relativa`),
            position = position_stack(vjust = 0.5),
            size = 8) +
  ylab("Proporção") +
  xlab("") +
  ggtitle("Gráfico")+
  theme_classic() + theme(#legend.position = 'bottom',
    plot.title = element_text(family = "Helvetica", 
                              face = "bold", 
                              size = (20),
                              hjust = 0.5),
    axis.title = element_text(face = "bold",
                              size = 15),
    axis.text.x = element_blank(),
    legend.title = element_blank(),
    text = element_text(size=15))  +
  scale_fill_brewer(palette="Paired")

#-----------------------------------------------------------------------

tabela2 <- tabela2[-nrow(tabela2),] %>% 
  arrange(desc(Instrução)) %>%
  mutate(ypos = cumsum(`Freq. Relativa`)- 0.5*`Freq. Relativa`)

ggplot(tabela2, 
       aes(x="", y=`Freq. Relativa`, fill=Instrução)) +
  geom_bar(stat="identity", 
           width=1,
           col = 1,
           lwd = 1) +
  coord_polar("y", start=0) +
  theme_classic() + 
  #theme(legend.position="none") +
  geom_text(aes(y = ypos, label = `Freq. Relativa`), 
            color = 1, 
            size = 5)+
  ylab("") +
  xlab("") +
  ggtitle("Gráfico")+
  theme(legend.position = 'right',
        plot.title = element_text(family = "Helvetica", 
                                  face = "bold", 
                                  size = (20),
                                  hjust = 0.5),
        axis.title = element_text(face = "bold",
                                  size = 15),
        axis.text = element_blank(),
        legend.title = element_blank(),
        text = element_text(size=20)) +
  scale_fill_brewer(palette="Paired")

#-----------------------------------------------------------------------