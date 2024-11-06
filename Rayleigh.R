# ------------------------------
# Script: Estimação do Modelo Rayleigh
# ------------------------------

# 1. Função Densidade da Distribuição Rayleigh
# A função densidade de probabilidade (f(x)) do modelo Rayleigh é:

# f(x; σ) = (x / σ^2) * exp(-x^2 / (2 * σ^2))

rayleigh <- function(x, sigma) {
  # Verificação para garantir que x seja positivo
  if (any(x <= 0)) {
    stop("Os valores de x devem ser positivos.")
  }
  # Fórmula da função densidade da Rayleigh
  (x / (sigma^2)) * exp(-x^2 / (2 * sigma^2))
}

# 2. Função Quantílica da Rayleigh (Q(u))
# A função quantílica gera valores a partir de probabilidades acumuladas.
# A fórmula inversa da CDF da Rayleigh é:
# Q(u) = σ * sqrt(-2 * log(1 - u))

qrayleigh <- function(u, sigma) {
  if (any(u <= 0) | any(u >= 1)) {
    stop("Os valores de u devem estar no intervalo (0, 1).")
  }
  # Fórmula para gerar quantis da Rayleigh
  sigma * sqrt(-2 * log(1 - u))
}

# 3. Função Log-Verossimilhança
# A log-verossimilhança é usada para estimar o parâmetro sigma com base em uma amostra de dados.
# A fórmula da log-verossimilhança para n amostras é:
# logL(sigma) = sum(log(f(x_i; sigma))) para i = 1, ..., n

loglik_rayleigh <- function(sigma, x) {
  # Evita valores inválidos de sigma
  if (sigma <= 0) {
    return(Inf)  # Retorna infinito se sigma for não-positivo
  }
  # Calcular a log-verossimilhança somando os logaritmos das densidades
  log_likelihood <- sum(log(rayleigh(x, sigma)))
  
  # Retorna a log-verossimilhança negativa
  return(-log_likelihood)
}

# 4. Gerar Dados Simulados da Distribuição Rayleigh
# Usamos a função quantílica para gerar números aleatórios da distribuição

set.seed(123)  # Definir uma semente para reprodutibilidade
sigma_true <- 2  # Valor real de 'sigma'
n <- 10000  # Tamanho da amostra

p <- runif(n)

# Gerar valores simulados da distribuição
x_simulado <- qrayleigh(p, sigma_true)

# 5. Estimar os Parâmetros via Máxima Verossimilhança
# Usamos a função 'optim' para maximizar a log-verossimilhança e encontrar
# os melhores valores de sigma

# Valor inicial para sigma
valores_iniciais <- 1

# Usamos a função 'optim' para encontrar os valores que maximizam a log-verossimilhança
resultado <- optim(par = valores_iniciais, fn = loglik_rayleigh, x = x_simulado, method = "SANN")

# 6. Estimar os Parâmetros via Método de Momentos
# O estimador de sigma pelo método de momentos é dado por: 
# \hat{σ} = \bar{X} / sqrt(π / 2)

sigma_mm <- mean(x_simulado) / sqrt(pi / 2)

# 7. Estimar os Parâmetros via Método dos Quantis
# Usando a mediana amostral para estimar sigma
# \hat{σ} = median(x) / sqrt(2 * log(2))

sigma_quantis <- median(x_simulado) / sqrt(2 * log(2))

# 8. Exibir os Resultados
# Os resultados da otimização incluem os parâmetros estimados de sigma

# Extraindo o parâmetro estimado por máxima verossimilhança
sigma_mle <- resultado$par

cat("Parâmetro σ estimado (MV):", round(sigma_mle, 4), "\n")
cat("Parâmetro σ estimado (MM):", round(sigma_mm, 4), "\n")
cat("Parâmetro σ estimado (Quantis):", round(sigma_quantis, 4), "\n")
cat("Parâmetro σ verdadeiro:", sigma_true, "\n")
