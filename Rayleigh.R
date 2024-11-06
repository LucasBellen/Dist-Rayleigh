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






################ A partir daqui as simulações de Monte Carlo

### Simulação de Monte Carlo para o Método dos Momentos (MM) ###
# Parâmetros da simulação
set.seed(123)         # Para reprodutibilidade
sigma_true <- 2       # Valor verdadeiro de 'sigma'
n <- 100              # Tamanho da amostra em cada simulação
num_sim <- 1000       # Número de simulações de Monte Carlo

# Vetor para armazenar as estimativas de sigma em cada simulação
estimativas_sigma_mm <- numeric(num_sim)

# Função para realizar uma única estimativa de sigma pelo método dos momentos
simular_estimacao_mm <- function(n, sigma_true) {
  # Gerar probabilidades uniformemente distribuídas
  p <- runif(n)
  # Gerar uma amostra da distribuição Rayleigh
  x_simulado <- qrayleigh(p, sigma_true)
  # Estimar sigma pelo método dos momentos
  sigma_mm <- mean(x_simulado) / sqrt(pi / 2)
  # Retornar a estimativa de sigma
  return(sigma_mm)
}

# Realizar a simulação de Monte Carlo
for (i in 1:num_sim) {
  estimativas_sigma_mm[i] <- simular_estimacao_mm(n, sigma_true)
}

# Análise dos Resultados da Simulação
# Calculando estatísticas para avaliar a estimativa de sigma
media_estimativa_mm <- mean(estimativas_sigma_mm)
viés_mm <- media_estimativa_mm - sigma_true
erro_quadratico_medio_mm <- mean((estimativas_sigma_mm - sigma_true)^2)

# Exibir resultados
cat("Resultados da Simulação de Monte Carlo - Método dos Momentos:\n")
cat("Valor verdadeiro de σ:", sigma_true, "\n")
cat("Média das estimativas de σ:", round(media_estimativa_mm, 4), "\n")
cat("Viés da estimativa de σ:", round(viés_mm, 4), "\n")
cat("Erro quadrático médio (EQM):", round(erro_quadratico_medio_mm, 4), "\n")


### Simulação de Monte Carlo para o Método dos Quantis (MQ) ###

# Parâmetros da simulação (os mesmos de antes)
set.seed(123)         # Para reprodutibilidade
sigma_true <- 2       # Valor verdadeiro de 'sigma'
n <- 100              # Tamanho da amostra em cada simulação
num_sim <- 1000       # Número de simulações de Monte Carlo

# Vetor para armazenar as estimativas de sigma em cada simulação
estimativas_sigma_mq <- numeric(num_sim)

# Função para realizar uma única estimativa de sigma pelo método dos quantis
simular_estimacao_mq <- function(n, sigma_true) {
  # Gerar probabilidades uniformemente distribuídas
  p <- runif(n)
  # Gerar uma amostra da distribuição Rayleigh
  x_simulado <- qrayleigh(p, sigma_true)
  # Estimar sigma pelo método dos quantis usando a mediana amostral
  sigma_mq <- median(x_simulado) / sqrt(2 * log(2))
  # Retornar a estimativa de sigma
  return(sigma_mq)
}

# Realizar a simulação de Monte Carlo
for (i in 1:num_sim) {
  estimativas_sigma_mq[i] <- simular_estimacao_mq(n, sigma_true)
}

# Análise dos Resultados da Simulação
# Calculando estatísticas para avaliar a estimativa de sigma
media_estimativa_mq <- mean(estimativas_sigma_mq)
viés_mq <- media_estimativa_mq - sigma_true
erro_quadratico_medio_mq <- mean((estimativas_sigma_mq - sigma_true)^2)

# Exibir resultados
cat("\nResultados da Simulação de Monte Carlo - Método dos Quantis:\n")
cat("Valor verdadeiro de σ:", sigma_true, "\n")
cat("Média das estimativas de σ:", round(media_estimativa_mq, 4), "\n")
cat("Viés da estimativa de σ:", round(viés_mq, 4), "\n")
cat("Erro quadrático médio (EQM):", round(erro_quadratico_medio_mq, 4), "\n")

### Comparação dos Resultados entre os Métodos ###
# Criando uma tabela com os resultados dos três métodos
tabela_comparativa <- data.frame(
  Método = c("Máxima Verossimilhança", "Método dos Momentos", "Método dos Quantis"),
  `Média das estimativas de σ` = round(c(media_estimativa, media_estimativa_mm, media_estimativa_mq), 4),
  `Viés da estimativa de σ` = round(c(viés, viés_mm, viés_mq), 4),
  `Erro quadrático médio (EQM)` = round(c(erro_quadratico_medio, erro_quadratico_medio_mm, erro_quadratico_medio_mq), 4)
)

# Exibindo a tabela
print(tabela_comparativa)



### GRAFICO PARA DIFERENTES n's

# Função para estimar sigma com base em n
estimar_sigma <- function(n, sigma_true, valores_iniciais) {
  set.seed(123)  # Definir uma semente para reprodutibilidade
  p <- runif(n)  # Gerar probabilidades uniformes entre 0 e 1
  x_simulado <- qrayleigh(p, sigma_true)  # Gerar valores simulados da distribuição
  
  # Usar 'optim' para encontrar o valor estimado de sigma
  resultado <- optim(par = valores_iniciais, fn = loglik_rayleigh, x = x_simulado, method = "SANN")
  
  return(resultado$par)  # Retornar a estimativa
}

# Parâmetros
sigma_true <- 2
valores_iniciais <- 1
n_simulacoes <- c(10, 20, 30, 40, 50, 100, 150, 200)

# Calculando as estimativas para diferentes tamanhos de amostra
estimativas <- sapply(n_simulacoes, estimar_sigma, sigma_true = sigma_true, valores_iniciais = valores_iniciais)

# Exibindo as estimativas e o valor real de sigma
for (i in 1:length(n_simulacoes)) {
  cat("Para n =", n_simulacoes[i], ": σ estimado =", round(estimativas[i], 4), "; σ verdadeiro =", sigma_true, "\n")
}

# Criando o gráfico
plot(n_simulacoes, estimativas, type = "l", col = "blue", 
     ylim = c(1.5, 2.5), xlab = "Número de Simulações (n)", 
     ylab = "Estimativa", main = "Gráfico de Convergência", pch = 16)

# Adicionando a linha do valor real
abline(h = sigma_true, col = "red", lty = 2)

# Adicionando as etiquetas nos pontos
text(n_simulacoes, estimativas, labels = round(estimativas, 2), pos = 3, cex = 0.8, col = "blue")

# Marcando os pontos
points(n_simulacoes, estimativas, pch = 16, col = "blue")

# Adicionando uma legenda
legend("topright", legend = c("Estimativa", "Valor Real"),
       col = c("blue", "red"), lty = c(1, 2), pch = c(16, NA), lwd = c(1, 1))

### MONTE CARLO

# Parâmetros da simulação
set.seed(123)         # Para reprodutibilidade
sigma_true <- 2       # Valor verdadeiro de 'sigma'
n <- 100              # Tamanho da amostra em cada simulação
num_sim <- 1000       # Número de simulações de Monte Carlo

# Vetor para armazenar as estimativas de sigma em cada simulação
estimativas_sigma <- numeric(num_sim)

# Função para realizar uma única estimativa de sigma para uma amostra de tamanho n
simular_estimacao <- function(n, sigma_true) {
  # Gerar probabilidades uniformemente distribuídas
  p <- runif(n)
  # Gerar uma amostra da distribuição Rayleigh
  x_simulado <- qrayleigh(p, sigma_true)
  # Estimar sigma via máxima verossimilhança usando 'optim'
  resultado <- optim(par = 1, fn = loglik_rayleigh, x = x_simulado, method = "SANN")
  # Retornar a estimativa de sigma
  return(resultado$par)
}

# Realizar a simulação de Monte Carlo
for (i in 1:num_sim) {
  estimativas_sigma[i] <- simular_estimacao(n, sigma_true)
}

# 6. Análise dos Resultados da Simulação
# Calculando estatísticas para avaliar a estimativa de sigma
media_estimativa <- mean(estimativas_sigma)
viés <- media_estimativa - sigma_true
erro_quadratico_medio <- mean((estimativas_sigma - sigma_true)^2)

# Exibir resultados
cat("Resultados da Simulação de Monte Carlo:\n")
cat("Valor verdadeiro de σ:", sigma_true, "\n")
cat("Média das estimativas de σ:", round(media_estimativa, 4), "\n")
cat("Viés da estimativa de σ:", round(viés, 4), "\n")
cat("Erro quadrático médio (EQM):", round(erro_quadratico_medio, 4), "\n")


### TABELA

# Parâmetros da Simulação (já definidos anteriormente)
sigma_true <- 2        # Valor verdadeiro de 'sigma'
n <- 100               # Tamanho da amostra em cada simulação
num_sim <- 1000        # Número de simulações de Monte Carlo

# (Código para simulação de Monte Carlo já executado, como no exemplo anterior)

# Estatísticas calculadas para a análise
media_estimativa <- mean(estimativas_sigma)
viés <- media_estimativa - sigma_true
erro_quadratico_medio <- mean((estimativas_sigma - sigma_true)^2)

# Criando a tabela com os resultados
tabela_resultados <- data.frame(
  `Valor verdadeiro de σ` = sigma_true,
  `Média das estimativas de σ` = round(media_estimativa, 4),
  `Viés da estimativa de σ` = round(viés, 4),
  `Erro quadrático médio (EQM)` = round(erro_quadratico_medio, 4)
)

# Exibindo a tabela
print(tabela_resultados)


#### plot

# Carregar o pacote ggplot2 (caso não esteja instalado, use install.packages("ggplot2"))
library(ggplot2)

# Criar um data frame para as estimativas
df_estimates <- data.frame(Estimativas = estimativas_sigma)

# Criar o histograma das estimativas de sigma
grafico <- ggplot(df_estimates, aes(x = Estimativas)) +
  geom_histogram(binwidth = 0.05, color = "black", fill = "skyblue") +
  geom_vline(aes(xintercept = sigma_true), color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Distribuição das Estimativas de σ nas Simulações de Monte Carlo",
    x = "Estimativas de σ",
    y = "Frequência"
  ) +
  theme_minimal() +
  annotate("text", x = sigma_true + 0.1, y = max(table(round(estimativas_sigma, 1))), 
           label = paste("Valor verdadeiro σ =", sigma_true), color = "red", hjust = 0)

# Exibir o gráfico
print(grafico)









