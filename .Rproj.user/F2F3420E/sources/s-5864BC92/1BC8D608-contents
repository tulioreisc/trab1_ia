source("Estado.R")

## Classe e métodos para o problema dos 3 Missionários e 3 Canibais
Aspirador <- function(desc = NULL, pai = NULL){
  
  e <- environment()
  
  assign("desc", desc, envir = e)
  assign("pai", pai, envir = e)
  assign("g", 0, envir = e)
  assign("h", Inf, envir = e)
  assign("f", Inf, envir = e)
  
  class(e) <- c("Aspirador", "Estado")
  
  return(e)
}


## Sobrecarregando o operador "==" para comparação entre estados
Ops.Aspirador = function(obj1, obj2){
  if(.Generic == "=="){
    return(identical(obj1$desc,obj2$desc))
  }
}

## Sobrecarga da função genérica "print" do R
print.Aspirador <- function(obj) {
  cat("Estado Atual:\n")
  cat("Q1 Q2 Q3 Q4 A:",obj$desc$W, "\n")
  cat("G(n): ", obj$g, "\n")
  cat("H(n): ", obj$h, "\n")
  cat("F(n): ", obj$f, "\n")
}

## Sobrecarga da função genérica "heuristica", definida por Estado.R
heuristica.Aspirador <- function(atual){
  
  ##Soma de quantos lixos existem no estado atual
  Total_Lixo <- (atual$desc[1]+atual$desc[2]+atual$desc[3]+atual$desc[4])
  
  pos <- atual$desc[5]
  
  if ((atual$desc[pos]) == 1){
    return(2*TotalLixo)
  }
  
  return((2*Tota_lLixo)+1)
}

geraFilhos.Aspirador <- function(obj) {
  
  filhos <- list()
  
  filhosDesc <- list()
  
  desc <- obj$desc
  
  
  ## gera filhos usando todos os operadores  
  pos <- obj$desc[5]
  
  if(pos == 1){          
    filhosDesc <- mapply(sum, obj$desc,  list(0,0,0,0,1))
    filhosDesc <- 2
    filhosDesc <- mapply(sum, obj$desc , list(0,0,0,0,2))
    filhosDesc <- 3
    filhosDesc <- mapply(sum, obj$desc, list(-1,0,0,0,0))
    filhosDesc <- 1
    
  } else if(pos == 2){
    filhosDesc <- mapply(sum, obj$desc, list(0,0,0,0,-1))
    filhosDesc <- 2
    filhosDesc <- mapply(sum, obj$desc, list(0,0,0,0,2))
    filhosDesc <- 3
    filhosDesc <- mapply(sum, obj$desc, list(0,-1,0,0,0))
    filhosDesc <- 1
    
  } else if(pos == 3){
    filhosDesc <- mapply(sum, obj$desc, list(0,0,0,0,1))
    filhosDesc <- 2
    filhosDesc <- mapply(sum, obj$desc, list(0,0,0,0,-2))
    filhosDesc <- 3
    filhosDesc <- mapply(sum, obj$desc, list(0,0,-1,0,0))
    filhosDesc <- 1
    
  } else {
    filhosDesc <- mapply(sum, obj$desc, c(0,0,0,0,-1))
    filhosDesc <- 2
    filhosDesc <- mapply(sum, obj$desc, c(0,0,0,0,-2))
    filhosDesc <- 3
    filhosDesc <- mapply(sum, obj$desc, c(0,0,0,-1,0))
    filhosDesc <- 1
  }
  
  ##### PAREI AQUI #####
  
  ## gera os objetos Canibais para os filhos
  for(i in length(filhosDesc)){
    if(i%%2==1){
      filho <- Aspirador(desc = i, pai = obj)
      filho$h <- heuristica(filho)
      filho$g <- filhosDesc[i+1]
      filhos <- c(filhos, list(filho))
    }
  }
  
  return(filhos)
}

