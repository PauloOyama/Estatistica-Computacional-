#Exercicio 4

res <- c()

for(i in 1:100000){
  sorteio <-  c(runif(1,0,1))
  j <- 2
  repeat{
    sorteio[j] <- runif(1,0,1)
    if(sum(sorteio) >= 1){
      res[i] <- j
      break;
    }
    j <- j +1
  }
}
res

mean(res)
mean(res == 3)


#Exercicio 5


#a)

result <- c()
for(j in 1:100000){
  
  aux1 <- c()
  sorteio <- c(sample(c(1:52),size =5, replace=FALSE))
  sorteio
  auxVerificaA <- sorteio %% 13 
  auxVerificaA
  for(i in 0:12){
    cont <- 0
   for(h in 1:5){
     if(auxVerificaA[h] == i){
       cont <- cont + 1
     }
   }
    if(cont == 2){
      aux1[i+1] <- 1
    }else{
      aux1[i+1] <- 0
    }
  }
  aux1
  result[j] <- sum(aux1)
}

100*mean(result == 1)



#b)

#Mesma Resolucao que a Anterior
result <- c()
for(j in 1:10000){
  
  aux1 <- c()
  sorteio <- c(sample(c(1:52),size =5, replace=FALSE))
  
  auxVerificaA <- sorteio %% 13 
  for(i in 0:12){
    cont <- 0
    for(h in 1:5){
      if(auxVerificaA[h] == i){
        cont <- cont + 1
      }
    }
    if(cont == 2){
      aux1[i+1] <- 1
    }else{
      aux1[i+1] <- 0
    }
  }
  result[j] <- sum(aux1)
}

#Se a soma eh 2, entao 1 (PAR) + 1 (PAR) = 2 (PAR + PAR)
100*mean(result == 2)



#c)

result <- c()
for(j in 1:1000000){
  
  aux1 <- c()
  sorteio <- c(sample(c(1:52),size =5, replace=FALSE))
  #transforma em um conjunto
  auxVerificaA <- sorteio %% 13 
  #Verifica os conjuntos do baralho
  for(i in 0:12){
    cont <- 0
    #Verifica com o sorteio
    for(h in 1:5){
      if(auxVerificaA[h] == i){
        cont <- cont + 1
      }
    }
    #Eh par ?
    if(cont == 2){
      aux1[i+1] <- 1
      #Eh trinca ? 
      }else if(cont == 3){
      aux1[i+1] <- 2
      } else{
      aux1[i+1] <- 0
    }
  }
  result[j] <- sum(aux1)
}

#Se soma igual a 3 entao 1 (Par) + 2 (Trinca) = 3 (PAR e TRINCA)
100*mean(result == 3)


#d)

result <- c()
for(j in 1:100000){
  
  aux1 <- c()
  sorteio <- c(sample(c(1:52),size =5, replace=FALSE))
  
  auxVerificaA <- sorteio %% 13
  #Vefica os conjuntos do baralho
  for(i in 0:12){
    cont <- 0
    for(h in 1:5){
      if(auxVerificaA[h] == i){
        cont <- cont + 1
      }
    }
    #Eh quadra?
    if(cont == 4){
      aux1[i] <- 1
    }else{
      aux1[i] <- 0
    }
  }
  result[j] <- sum(aux1)
}

100*mean(result == 1)

100*mean(result)


