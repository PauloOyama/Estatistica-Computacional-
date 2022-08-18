# Lista 4
# Gabriel Riquieri Campos - 11911BCC030
# Paulo Kiyoshi Oyama Filho - 11911BCC022

# 2 
# b)

heart <- read.table("heart.txt", header = TRUE, sep = ",")

heart <- heart[,-1]

head(heart)

# 1 -> chest pain
# 2 -> sex
# 3 -> smokes
# 4 -> exercise
# 5 -> heart attack

tree <- function(new_sample)
{
  if(new_sample[1] == "yes"){
    return("yes")
  }else{
    if(new_sample[2] == "yes"){
      return("no")
    }else{
      return("yes")
    }
  }
}


# testes

new_guy <- c("yes","yes","yes","yes","yes")
tree(new_guy)

ex1 <- c("yes","yes","no","yes","yes")
tree(ex1)

ex2 <- c("yes","yes","yes","no","yes")
tree(ex2)

ex3 <- c("no","no","yes","no","yes")
tree(ex3)

ex4 <- c("no","yes","no","yes","no")
tree(ex4)

ex5 <- c("yes","no","yes","yes","yes")
tree(ex5)

ex6 <- c("no","yes","yes","yes","no")
tree(ex6)

# ------------------------------------------------------------------------------

# 3

df <- read.csv("SBI.csv", header = TRUE)
head(df)
nrow(df)

# a)

table(df$sbi)

aux <- c()
for(i in 1:length(df$sbi)){
  if(df$sbi[i] == "NotApplicable"){
    aux[i] <- "nao"
  }else{
    aux[i] <- "sim"
  }
}


df$infection <- aux
head(df)

df$infection <- as.factor(df$infection)
str(df)

# b)

df <- df[,c(-1,-2,-8)]
head(df)

# c)

nAmostras <- nrow(df)

trainSize <- round(nAmostras*0.8)

indices_treino <- sample(nrow(df), size = 1878, replace=F)
indices_teste <- !(1:2348 %in% indices_treino)

train <- df[indices_treino,]
test <- df[indices_teste,]

nrow(df) == nrow(treino) + nrow(teste) #deu bom

# d)
library(rpart)
library(rpart.plot)

treeModel_Infection <- rpart(infection~., data=train)
rpart.plot(treeModel_Infection)

treeModel_previsao <- predict(treeModel_Infection, newdata = test, type = "class")
mean(test$infection == treeModel_previsao) #0.76

confusion_matrix <- table(treeModel_previsao, test$infection)

# e)

library(randomForest)
randonTreeModel_infection <- randomForest(infection~., data = train)

randonTreeModel_previsao <- predict(randonTreeModel_infection, newdata = test, type = "class")
mean(test$infection == randonTreeModel_previsao) #0.95

confusion_matrix2 <- table(randonTreeModel_previsao, test$infection)

# ------------------------------------------------------------------------------

# 4

penguins <- read.csv(file = "penguins_size.csv", header = TRUE, sep = ",")
head(penguins)
str(penguins)
table(penguins$species)
nrow(penguins)

# a)
pesoMedio <- mean(penguins$body_mass_g, na.rm = T)
comprimentoAsaMedio <- mean(penguins$flipper_length_mm, na.rm = T)

desvioPeso <- sd(penguins$body_mass_g, na.rm = T)
desvioComprimentoAsa <- sd(penguins$flipper_length_mm, na.rm = T)

# b)

adelie <- penguins[penguins$species == "Adelie", ]
chinstrap <- penguins[penguins$species == "Chinstrap", ]
gentoo <- penguins[penguins$species == "Gentoo", ]

pesoMedioAdelie <- mean(adelie$body_mass_g, na.rm = T)
pesoMedioChinstrap <- mean(chinstrap$body_mass_g, na.rm = T)
pesoMedioGentoo <- mean(gentoo$body_mass_g, na.rm = T)

desvioPesoAdelie <- sd(adelie$body_mass_g, na.rm = T)
desvioPesoChinstrap <- sd(chinstrap$body_mass_g, na.rm = T)
desvioPesoGentoo <- sd(gentoo$body_mass_g, na.rm = T)

comprimentoAsaMedioAdelie <- mean(adelie$flipper_length_mm, na.rm = T)
comprimentoAsaMedioChinstrap <- mean(chinstrap$flipper_length_mm, na.rm = T)
comprimentoAsaMedioGentoo <- mean(gentoo$flipper_length_mm, na.rm = T)

desvioComprimentoAsaAdelie <- sd(adelie$flipper_length_mm, na.rm = T)
desvioComprimentoAsaChinstrap <- sd(chinstrap$flipper_length_mm, na.rm = T)
desvioComprimentoAsaGentoo <- sd(gentoo$flipper_length_mm, na.rm = T)

# Comentario na parte escrita (.PDF)


# c)

male <- penguins[penguins$sex == "MALE", ]
female <- penguins[penguins$sex == "FEMALE", ]

pesoMedioMacho <- mean(male$body_mass_g, na.rm = T)
pesoMediaFemea <- mean(female$body_mass_g, na.rm = T)

desvioPesoMacho <- sd(male$body_mass_g, na.rm = T)
desvioPesoFemea <- sd(female$body_mass_g, na.rm = T)

comprimentoAsaMedioMacho <- mean(male$flipper_length_mm, na.rm = T)
comprimentoAsaMedioFemea <- mean(female$flipper_length_mm, na.rm = T)

desvioComprimentoAsaMacho <- sd(male$flipper_length_mm, na.rm = T)
desvioComprimentoAsaFemea <- sd(female$flipper_length_mm, na.rm = T)

# Comentario na parte escrita (.PDF)

# d)

p <- ggplot(penguins, mapping = aes(x = flipper_length_mm, y= body_mass_g, color= species))
p + geom_point(size=1.5)

q <- ggplot(penguins, mapping = aes(x = flipper_length_mm, y= body_mass_g, color= sex))
q + geom_point(size=2)

# e)

#Argumento complete.obs usado por causa que há argumentos faltantes(NA)
cor(penguins$flipper_length_mm,penguins$body_mass_g, use = "complete.obs")

# f)

# Comentario na parte escrita (.PDF)

# g)

reg_lin <- lm(body_mass_g ~ flipper_length_mm, data = penguins)

# h)

# Comentario na parte escrita (.PDF)

# i)

# Y = aX +b
# Y = 49.69X + (-5780.83)
# Y = 49.69X - 5780.83

penguin_204 <- 49.69*204 + (-5780.83 )

lim_inf <- mean(penguins$body_mass_g[penguins$flipper_length_mm == 203], na.rm = TRUE)
lim_sup <- mean(penguins$body_mass_g[penguins$flipper_length_mm == 205], na.rm = TRUE)

#Está dentro da media
margin <- c(lim_inf,lim_sup)



maior_indice <- length(sort(penguins$flipper_length_mm))

limites <- c(sort(penguins$flipper_length_mm)[1], sort(penguins$flipper_length_mm)[maior_indice])

# Comentario na parte escrita (.PDF)
