# simulated-and-verify-independence-statistics-in-two-random-variables
código para simulações e verificação da independência estatística entre duas variáveis aleatórias



# Definir as densidades marginais
fy1 <- function(y1) {
  return(exp(-y1*(y1+1)) * (y1+1))
}

fy2 <- function(y2) {
  return(exp(-(y2*(1-y2)+y2)) * (1-y2))
}

# Simular dados
set.seed(123)  # Definir semente para reprodutibilidade
n <- 10000     # Número de amostras
x1 <- rexp(n, rate = 1)
x2 <- rexp(n, rate = 1)
x3 <- rexp(n, rate = 1)

# Calcular Y1, Y2, Y3
y1 <- x1 / (x1 + x2)
y2 <- (x1 + x2) / (x1 + x2 + x3)
y3 <- x1 + x2 + x3

# Plotar histogramas
par(mfrow=c(3,1))
hist(y1, main="Histograma de Y1", col="lightblue", probability=TRUE)
curve(fy1, add=TRUE, col="red", lwd=2)

hist(y2, main="Histograma de Y2", col="lightgreen", probability=TRUE)
curve(fy2, add=TRUE, col="red", lwd=2)

hist(y3, main="Histograma de Y3", col="lightcoral", probability=TRUE)

# Calcular as densidades conjuntas (ainda não está considerando independência)
fconjunta <- function(y1, y2) {
  return(fy1(y1) * fy2(y2))
}

# Avaliar independência usando a fórmula de independência condicional
independencia <- function() {
  eps <- 1e-10
  for (i in 1:n) {
    if (abs(fconjunta(y1[i], y2[i]) - (fy1(y1[i]) * fy2(y2[i]))) > eps) {
      return(FALSE)
    }
  }
  return(TRUE)
}

is_independente <- independencia()
print(paste("As variáveis Y1 e Y2 são independentes: ", is_independente))

