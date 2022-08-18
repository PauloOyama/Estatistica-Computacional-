setwd("C:/Universidade/EstatisticaComputacional/códigos")
aves <- read.table("aves.txt", sep = ",", header = TRUE)

#1 
#A)

head(aves)
tail(aves)
str(aves)
summary(aves)


#B)

embaralha <- sample(nrow(aves))

aves <- aves[embaralha, ]


#C)


#Dinamarca
comp_asaDinamarca <- aves$comprimento_asa[aves$local == "dinamarca"]
peso_Dinamarca <- aves$peso[aves$local == "dinamarca"]

  #Comprimento da Asa
mean(comp_asaDinamarca)
sd(comp_asaDinamarca)

  #Peso
mean(peso_Dinamarca)
sd(peso_Dinamarca)



#Noruega
comp_asaNoruega <- aves$comprimento_asa[aves$local == "noruega"]
peso_Noruega <- aves$peso[aves$local == "noruega"]

  #Comprimento da Asa

mean(comp_asaNoruega)
sd(comp_asaNoruega)
  
  #Peso

mean(peso_Noruega)
sd(peso_Noruega)

#Islandia

comp_asaIslandia <- aves$comprimento_asa[aves$local == "islandia"]
peso_Islandia <- aves$peso[aves$local == "islandia"]

#Comprimento da Asa

mean(comp_asaIslandia)
sd(comp_asaIslandia)

#Peso

mean(peso_Islandia)
sd(peso_Islandia)



#Comentário: 
# Pelo estudo pode-se observar que com relação ao comprimento da asa, as aves na região da 
# Dinamarca possuem asam maiores e com valores mais consistentes, ou seja, em um grupo há pouca
# variação de tamanho de asa para asa, enquanto que na Noruega a asa é menor e possui maior desvio e na
# Islandia possui a menor asa de todos, porém fica em 2 com relação ao seu desvio 
# padrão. Além disso, podemos inferir que o peso da ave Dinamarquesa é maior do que a norueguesa
# e também no desvio padrão, contudo ambos os desvios padrões não são muito diferente, já a Islandesa
# fica por ultimo em ambos os aspectos tornando a ave no geral em menor forma fisica.



#D)


boolean <- aves$local == "dinamarca"
aux <- c()
for(i in 1:length(boolean)){
  if(boolean[i] == TRUE){
    #dinamarca
    aux[i] = "red"
  }else{
    if(aves$local[i] == "islandia"){
      #islandia
      aux[i] = "blue"
    }else{
      #noruega
      aux[i] = "orange"
    }
  }
}

plot(aves$comprimento_asa,aves$peso, col=aux)


#E)

rand <- round(nrow(aves)*0.8) 
ind <- sample(nrow(aves)) 
treino <- aves[1:n,]
teste <- aves[(n + 1):nrow(aves), ]    

#F) 


island <- treino[treino$local == "islandia", ]
dinamarc <- treino[treino$local == "dinamarca", ]
norueg <- treino[treino$local == "noruega", ]

summary(island)
summary(dinamarc)
summary(norueg)

classificacao <- c()
for (i in 1:nrow(treino)) {
  #27.53 Minimo dos Minimos
  if(treino$comprimento_asa[i] < 26.35){
    classificacao[i] <- "islandia"
  }else{
    #167.3 Minimo Peso
      if(treino$peso > 159.1){
        classificacao[i] <- "dinamarca"
      }else{
        classificacao[i] <- "noruega"
      }
  }
}

mean(treino$local == classificacao)

#G)

classificacao <- c()
for (i in 1:nrow(teste)) {
  if(teste$comprimento_asa[i] < 26.35){
    classificacao[i] <- "islandia"
  }else{
    if(teste$peso > 159.1){
      classificacao[i] <- "dinamarca"
    }else{
      classificacao[i] <- "noruega"
    }
  }
}

mean(teste$local == classificacao)


matriz_conf <- table(classificacao, teste$local)
matriz_conf

# O meu sistema de classificação  possui erros na hora de separar na condicional relacionado ao peso
# divindindo entre Noruega e Dinamarca enquanto que no primeiro if com relação ao comprimento da asa 
# o corte é praticamente perfeito como mostrado pela matriz


#2)

#a)

island <- aves[aves$local == "islandia", ]
dinamarc <- aves[aves$local == "dinamarca", ]
norueg <- aves[aves$local == "noruega", ]

cor(island$comprimento_asa,island$peso)
cor(dinamarc$comprimento_asa,dinamarc$peso)
cor(norueg$comprimento_asa,norueg$peso)

# A maior correlação ocorre na Islandia, seguida por Dinamarca e depois pela Noruega

# B)



linear_model <- function(x,y){
  
  auxCima <- ( length(x) * sum(x*y) ) - (sum(x)*sum(y))
  
  auxBaixo <- (length(x) * sum(x*x)) - (sum(x)*sum(x))
  
  m <- auxCima/auxBaixo
  
  b <- mean(y) - m*mean(x)
  
  aux <- c(m,b)
  
  return(aux)
}


#c)

result <- linear_model(aves$comprimento_asa,aves$peso)

# confere <- lm(peso ~ comprimento_asa, data = aves)

#D)

# Y0 = x0*a + b

# X1 = x0 + (1/2)
# Y1 = x1*a + b <=> (x0 + 1/2)*a + b
# Y1 = Y0 + a/2
# a é o coef. de variação

# Portanto como demonstado, o aumento de um 0.5(1/2)cm na variável comprimento de asa (x)
# ocasionaria o aumento da variável peso(Y) o equivalente a metade do valor do coeficiente
# linear (a)  em relação ao peso antes do aumento de meio cm. Isso é descrito com base
# na formula acima, um fato interessante é que o aumento de 1 unidade (cm) no comprimento da asa resultaria no aumento
# do peso igual ao coeficiente linear.


# E)

valor <- -50.274300 + 23*6.162284

# Sim, é possível utilizar os valores que encontramos para determinar o peso de uma ave para a 
# envergadura de 23 cm já que esse valor está dentro dos intervalo dos dados computados, no qual
# seus limites são de 17.77 cm até 41.47 cm, o valor do peso seria de 91,46 aproxidamente.
