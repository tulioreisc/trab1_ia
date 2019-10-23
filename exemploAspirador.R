debugSource("Aspirador.R")
debugSource("buscaDesinformada.R")
debugSource("buscaInformada.R")

# Define estado inicial
inicial <- Aspirador(desc = list(P = c(1,2), A = c(0, 1, 1, 1)))

# Define estado objetivo
objetivo <- Aspirador(desc = list(A = c(0, 0, 0, 0)))

# Aplicação dos algoritmos
cat("====\tBusca de Custo Uniforme\t=====\n")
print(buscaCustoUniforme(inicial, objetivo))

cat("====\tBusca Best-First (Gulosa)\t=====\n")
print(buscaBestFirst(inicial, objetivo, "Gulosa"))

cat("====\tBusca Best-First (A*)\t=====\n")
print(buscaBestFirst(inicial, objetivo, "AEstrela"))