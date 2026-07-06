# =========================================================
# RSS / sigma² ~ Qui-Quadrado(n - p)
# =========================================================
#
# Vamos demonstrar empiricamente que:
#
# RSS / sigma² ~ χ²(n - p)
#
# onde:
# RSS = soma dos quadrados dos resíduos
# n   = número de observações
# p   = número de parâmetros estimados
#
# =========================================================

set.seed(123)

# ---------------------------------------------------------
# Configurações
# ---------------------------------------------------------

n <- 30 # número de observações
beta0 <- 76.095
beta1 <- 5.8571

sigma <- 5.69 # desvio padrão do erro
sigma2 <- sigma^2

nsim <- 1000 # número de simulações

# Vetor para guardar RSS/sigma²
rss_scaled <- numeric(nsim)

# =========================================================
# Simulação
# =========================================================

for (i in 1:nsim) {
  # -------------------------------------------------------
  # Gerando dados
  # -------------------------------------------------------

  x <- runif(n, 0, 10)

  erro <- rnorm(n, mean = 0, sd = sigma)

  y <- beta0 + beta1 * x + erro

  # -------------------------------------------------------
  # Ajustando regressão
  # -------------------------------------------------------

  modelo <- lm(y ~ x)

  # -------------------------------------------------------
  # RSS
  # -------------------------------------------------------

  rss <- sum(residuals(modelo)^2)

  # -------------------------------------------------------
  # RSS/sigma²
  # -------------------------------------------------------

  rss_scaled[i] <- rss / sigma2
}

# =========================================================
# Graus de liberdade
# =========================================================

p <- 2 # beta0 e beta1

gl <- n - p

# =========================================================
# Plot
# =========================================================

hist(
  rss_scaled,
  probability = TRUE,
  breaks = 50,
  main = expression(RSS / sigma^2 ~ "vs Qui-Quadrado"),
  xlab = expression(RSS / sigma^2)
)

# Densidade teórica da qui-quadrado
curve(
  dchisq(x, df = gl),
  add = TRUE,
  lwd = 3
)

legend(
  "topright",
  legend = c(paste0("Qui-quadrado(", gl, ")")),
  lwd = 3
)

# =========================================================
# Comparação numérica
# =========================================================

cat("Média simulada:", mean(rss_scaled), "\n")
cat("Média teórica:", gl, "\n\n")

cat("Variância simulada:", var(rss_scaled), "\n")
cat("Variância teórica:", 2 * gl, "\n")
