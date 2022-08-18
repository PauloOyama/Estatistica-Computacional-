#Exercicio 1

estimativaIntegral <- function(a,b){
  x <- runif(10000, a, b)
  aux <- exp(2*x)*exp(x^2)
  integral <- mean(aux)
  return((b-a)*integral)
}


exericio1 <- estimativaIntegral(-1,2)
exericio1


#---------------------------------------------------------------------------------------#

#Exercicio 2

#a)
pacotes <- c(1:20)
resultado <- c()
for(x in 1:10000){
  j <- 1
  sorteio <- c()
  repeat {
    figura <- sample(pacotes, size=1,replace = TRUE)
    sorteio[j] <- figura
    if(sum(pacotes %in% sorteio) == 20){
      resultado[x] <- j
      break;
    }
    j <- j + 1
  }
}

mean(resultado)
mean(resultado <= 30)



#b)

figurinhas <- function(n){
  
  pacotes <- c(1:n)
  resultado <- c()
  for(x in 1:10000){
    j <- 1
    sorteio <- c()
    repeat {
      figura <- sample(pacotes, size=1,replace = TRUE)
      sorteio[j] <- figura
      if(sum(pacotes %in% sorteio) == n){
        resultado[x] <- j
        break;
      }
      j <- j + 1
    }
  }
  return(mean(resultado))
}

figurinhas(20)

#---------------------------------------------------------------------------------------#


#Exercicio 3 


#a) 

caminhosLuke <- function(l){
  resultado <- c()
  for(i in 1:1000){
    j <- 2
    caminho <- c(l)
    repeat{
      sorteio <- sample(c(-1, 1), size = 1)
      caminho[j] <- caminho[j-1] + sorteio
      if(caminho[j] == 20){
        resultado[i] <- 0
        break;
      } 
      if(caminho[j] == 0){
        resultado[i] <- 1
        break;
      }
      j <- j +1
    }
  }
  return(mean(resultado))
}



caminhosLuke(10)

#b)

y <- c()
for(j in 1:19){
y[j] <- caminhosLuke(j)  
}
x <- 1:19 
plot(x,y, color="red", pch=19)




