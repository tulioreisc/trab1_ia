source("Estado.R")

## Classe e métodos para o problema do Aspirador
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
Ops.Aspirador = function(obj1,obj2){
  if(.Generic == "=="){
    return(all(obj1$desc$A == obj2$desc$A))
  }
}

## Sobrecarga da função genérica "print" do R
print.Aspirador <- function(obj) {
  # P = posição atual
  # A = Array com o "mundo" do aspirador
  cat("A: (", obj$desc$A, ")\n")
  cat("G(n): ", obj$g, "\n")
  cat("H(n): ", obj$h, "\n")
  cat("F(n): ", obj$f, "\n")
}

## Sobrecarga da função genérica "heuristica", definida por Estado.R
heuristica.Aspirador <- function(atual){
  
  if(is.null(atual$desc))
    return(Inf)
  ## h(obj) = soma das posições que precisam ser limpas
  return(sum(atual$desc$A))
}

geraFilhos.Aspirador <- function(obj) {
  
  ## filhos
  filhos <- list()  
  filhosDesc <- list()
  
  ## descendentes (alias)
  desc <- obj$desc
  desc$C <- 0
  
  ## cria uma copia do array atual
  cls_op <- desc
  
  # define o index do aspirador no array
  if ((desc$P[1]==1) && 
      (desc$P[2]==1)) {
    index <- 1
  } else if ((desc$P[1]==2) && 
             (desc$P[2]==1)) {
    index <- 3
  } else if((desc$P[1]==1) && 
            (desc$P[2]==2)) {
    index <- 2
  } else {
    index <- 4
  }
  
  # limpa a posição da cópia
  cls_op$A[index] <- 0
  
  # lista de operadores:
  operadores <- list( 
    list(P = c( 0, 0), A = cls_op$A, C = 2), # limpa posição
    list(P = c( 0, 1), A = desc$A, C = 1), # move para a direita
    list(P = c( 1, 0), A = desc$A, C = 3), # move para baixo
    list(P = c( 0, -1), A = desc$A, C = 1), # move para a esquerda
    list(P = c(-1, 0), A = desc$A, C = 3)  # move para cima
  )
  
  
  # função que aplica arbitráriamente uma das operações definidas ao nó
  aplicaOperacoes <- function(op) {
    list(P = desc$P + op$P, A = op$A, C = op$C)
  }
  
  # utiliza a função apliaOperações para todos os operadores da lista
  filhosDesc <- lapply(operadores, aplicaOperacoes)
  
  
  ## verifica estados filhos incompatíveis com o problema  
  incompativeis <- sapply(1:length(filhosDesc),
                    function(i) {
                      fDesc <- filhosDesc[[i]]
                      if((any(fDesc$P < 1)) ||
                         (fDesc$P[1] > 2) ||
                         (fDesc$P[2] > 2)) # verifica se o aspirador não "fugiu" do espaço
                        i ## é incompatível: retorna índice
                      else
                        0 ## senão é compatível
                    })
  
  ## mantém no vetor apenas os que são incompatíveis
  incompativeis <- incompativeis[incompativeis != 0]
  
  ## remove estados filhos incompatíveis
  filhosDesc <- filhosDesc[-incompativeis]
  
  ## gera os objetos Aspirador para os filhos
  for(filhoDesc in filhosDesc){
    filho <- Aspirador(desc = list(P = filhoDesc$P, A = filhoDesc$A), pai = obj)
    filho$h <- heuristica(filho)
    filho$g <- obj$g + filhoDesc$C
    filhos <- c(filhos, list(filho))
  }
  
  return(filhos)
}