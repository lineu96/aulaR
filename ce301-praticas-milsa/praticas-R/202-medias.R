
#-----------------------------------------------------------------------
# Os comandos a seguir simulam cenários de uma variável quantitativa

# Para cada cenário calcule média aritmética simples, média geométrica e 
# média harmônica
#-----------------------------------------------------------------------

set.seed(20230406)

cenario1 <- rgamma(10, 10, 1)
cenario2 <- rgamma(10, 20, 1)
cenario3 <- rgamma(10, 30, 1)
cenario4 <- rgamma(10, 40, 1)

#-----------------------------------------------------------------------

# SOLUÇÃO
sum(cenario1)/length(cenario1) # média aritmética simples
prod(cenario1)^(1/length(cenario1)) # média geométrica
length(cenario1)/sum(1/cenario1) # média harmônica

#-----------------------------------------------------------------------
