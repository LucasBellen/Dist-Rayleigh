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




### GRAFICO PARA DIFERENTES n's

## n=10
set.seed(123)  # Definir uma semente para reprodutibilidade
sigma_true <- 2  # Valor real de 'sigma'
n1 <- 10 # Tamanho da amostra

# Gerar probabilidades uniformemente distribuídas entre 0 e 1
p <- runif(n1)

# Gerar valores simulados da distribuição
x_simulado <- qrayleigh(p, sigma_true)

# Valor inicial para sigma
valores_iniciais <- 1

# Usamos a função 'optim' para encontrar os valores que maximizam a log-verossimilhança
resultado <- optim(par = valores_iniciais, fn = loglik_rayleigh, x = x_simulado, method = "SANN")

# Extraindo o parâmetro estimado
estimates <- resultado$par

cat("Parâmetro σ estimado:", round(estimates[1], 4), "\n1")
cat("Parâmetro σ verdadeiro:", sigma_true, "\n1")

## n=20
set.seed(123)  # Definir uma semente para reprodutibilidade
sigma_true <- 2  # Valor real de 'sigma'
n2 <- 20 # Tamanho da amostra

# Gerar probabilidades uniformemente distribuídas entre 0 e 1
p <- runif(n2)

# Gerar valores simulados da distribuição
x_simulado <- qrayleigh(p, sigma_true)

# Usamos a função 'optim' para encontrar os valores que maximizam a log-verossimilhança
resultado <- optim(par = valores_iniciais, fn = loglik_rayleigh, x = x_simulado, method = "SANN")

# Extraindo o parâmetro estimado
estimates <- resultado$par

cat("Parâmetro σ estimado:", round(estimates[1], 4), "\n2")
cat("Parâmetro σ verdadeiro:", sigma_true, "\n2")

## n=30
set.seed(123)  # Definir uma semente para reprodutibilidade
sigma_true <- 2  # Valor real de 'sigma'
n3 <- 30 # Tamanho da amostra

# Gerar probabilidades uniformemente distribuídas entre 0 e 1
p <- runif(n3)

# Gerar valores simulados da distribuição
x_simulado <- qrayleigh(p, sigma_true)

# Usamos a função 'optim' para encontrar os valores que maximizam a log-verossimilhança
resultado <- optim(par = valores_iniciais, fn = loglik_rayleigh, x = x_simulado, method = "SANN")

# Extraindo o parâmetro estimado
estimates <- resultado$par

cat("Parâmetro σ estimado:", round(estimates[1], 4), "\n3")
cat("Parâmetro σ verdadeiro:", sigma_true, "\n3")


## n=40
set.seed(123)  # Definir uma semente para reprodutibilidade
sigma_true <- 2  # Valor real de 'sigma'
n4 <- 40 # Tamanho da amostra

# Gerar probabilidades uniformemente distribuídas entre 0 e 1
p <- runif(n4)

# Gerar valores simulados da distribuição
x_simulado <- qrayleigh(p, sigma_true)

# Usamos a função 'optim' para encontrar os valores que maximizam a log-verossimilhança
resultado <- optim(par = valores_iniciais, fn = loglik_rayleigh, x = x_simulado, method = "SANN")

# Extraindo o parâmetro estimado
estimates <- resultado$par

cat("Parâmetro σ estimado:", round(estimates[1], 4), "\n4")
cat("Parâmetro σ verdadeiro:", sigma_true, "\n4")

## n=50
set.seed(123)  # Definir uma semente para reprodutibilidade
sigma_true <- 2  # Valor real de 'sigma'
n5 <- 50 # Tamanho da amostra

# Gerar probabilidades uniformemente distribuídas entre 0 e 1
p <- runif(n5)

# Gerar valores simulados da distribuição
x_simulado <- qrayleigh(p, sigma_true)

# Usamos a função 'optim' para encontrar os valores que maximizam a log-verossimilhança
resultado <- optim(par = valores_iniciais, fn = loglik_rayleigh, x = x_simulado, method = "SANN")

# Extraindo o parâmetro estimado
estimates <- resultado$par

cat("Parâmetro σ estimado:", round(estimates[1], 4), "\n5")
cat("Parâmetro σ verdadeiro:", sigma_true, "\n5")

## n=100
set.seed(123)  # Definir uma semente para reprodutibilidade
sigma_true <- 2  # Valor real de 'sigma'
n6 <- 100 # Tamanho da amostra

# Gerar probabilidades uniformemente distribuídas entre 0 e 1
p <- runif(n6)

# Gerar valores simulados da distribuição
x_simulado <- qrayleigh(p, sigma_true)

# Usamos a função 'optim' para encontrar os valores que maximizam a log-verossimilhança
resultado <- optim(par = valores_iniciais, fn = loglik_rayleigh, x = x_simulado, method = "SANN")

# Extraindo o parâmetro estimado
estimates <- resultado$par

cat("Parâmetro σ estimado:", round(estimates[1], 4), "\n6")
cat("Parâmetro σ verdadeiro:", sigma_true, "\n6")

## n=150
set.seed(123)  # Definir uma semente para reprodutibilidade
sigma_true <- 2  # Valor real de 'sigma'
n7 <- 150 # Tamanho da amostra

# Gerar probabilidades uniformemente distribuídas entre 0 e 1
p <- runif(n7)

# Gerar valores simulados da distribuição
x_simulado <- qrayleigh(p, sigma_true)

# Usamos a função 'optim' para encontrar os valores que maximizam a log-verossimilhança
resultado <- optim(par = valores_iniciais, fn = loglik_rayleigh, x = x_simulado, method = "SANN")

# Extraindo o parâmetro estimado
estimates <- resultado$par

cat("Parâmetro σ estimado:", round(estimates[1], 4), "\n7")
cat("Parâmetro σ verdadeiro:", sigma_true, "\n7")

## n=200
set.seed(123)  # Definir uma semente para reprodutibilidade
sigma_true <- 2  # Valor real de 'sigma'
n8 <- 200 # Tamanho da amostra

# Gerar probabilidades uniformemente distribuídas entre 0 e 1
p <- runif(n8)

# Gerar valores simulados da distribuição
x_simulado <- qrayleigh(p, sigma_true)

# Usamos a função 'optim' para encontrar os valores que maximizam a log-verossimilhança
resultado <- optim(par = valores_iniciais, fn = loglik_rayleigh, x = x_simulado, method = "SANN")

# Extraindo o parâmetro estimado
estimates <- resultado$par

cat("Parâmetro σ estimado:", round(estimates[1], 4), "\n8")
cat("Parâmetro σ verdadeiro:", sigma_true, "\n8")


#### GRAFICO

# Definindo o valor real
valor_real <- 2  # Substitua pelo seu valor real

# Definindo os valores de n e as estimativas
n_simulacoes <- c(10,20,30,40,50,100,150,200)  # Substitua pelos seus valores de n
estimativas <- c(2.1747,2.1805,2.2459,2.2181,2.0852,1.975,2.0066,1.9855)    # Substitua pelas suas estimativas correspondentes

# Criando o gráfico
plot(n_simulacoes, estimativas, type = "l", col = "blue", 
     ylim = c(1.5, 2.5), xlab = "Número de Simulações (n)", 
     ylab = "Estimativa", main = "Gráfico de Convergência", pch = 16)

# Adicionando a linha do valor real
abline(h = valor_real, col = "red", lty = 2)

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










