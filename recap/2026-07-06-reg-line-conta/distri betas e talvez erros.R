# =========================================================
# Distribuição t do beta1 estimado
# =========================================================
#
# Vamos demonstrar empiricamente que:
#
# t =
# (beta1_hat - beta1_real) / SE(beta1_hat)
#
# ~ t(n - 2)
#
# =========================================================

set.seed(123)

# ---------------------------------------------------------
# Configurações
# ---------------------------------------------------------

n <- 30

beta0 <- 76.095
beta1_real <- 5.8571

sigma <- 5.69

nsim <- 10000

# Vetor para armazenar estatísticas t
t_beta1 <- numeric(nsim)
beta1_chap <- numeric(nsim)

# =========================================================
# Simulação
# =========================================================

for (i in 1:nsim) {
  # -------------------------------------------------------
  # Gerando dados
  # -------------------------------------------------------

  x <- runif(n, 0, 10)

  erro <- rnorm(n, mean = 0, sd = sigma)

  y <- beta0 + beta1_real * x + erro

  # -------------------------------------------------------
  # Ajustando regressão
  # -------------------------------------------------------

  modelo <- lm(y ~ x)

  # -------------------------------------------------------
  # Beta1 estimado
  # -------------------------------------------------------

  beta1_hat <- coef(modelo)[2]
  beta1_chap[i] <- beta1_hat

  # -------------------------------------------------------
  # Erro padrão do beta1
  # -------------------------------------------------------

  se_beta1 <- summary(modelo)$coefficients[2, 2]

  # -------------------------------------------------------
  # Estatística t
  # -------------------------------------------------------

  t_beta1[i] <-
    (beta1_hat - beta1_real) / se_beta1
}

# =========================================================
# Graus de liberdade
# =========================================================

gl <- n - 2

# =========================================================
# Histograma
# =========================================================

hist(
  beta1_chap,
  probability = TRUE,
  breaks = 50,
  # main = expression(
  #   frac(hat(beta)[1] - beta[1], SE(hat(beta)[1])) ~ "vs distribuição t"
  # ),
  xlab = "Estatística t"
)

# ---------------------------------------------------------
# Curva teórica t
# ---------------------------------------------------------

curve(
  dt(x, df = gl),
  add = TRUE,
  lwd = 3
)

legend(
  "topright",
  legend = c(paste0("t(", gl, ")")),
  lwd = 3
)

# =========================================================
# Comparações numéricas
# =========================================================

cat("Graus de liberdade:", gl, "\n\n")

cat("Média simulada:", mean(t_beta1), "\n")
cat("Média teórica:", 0, "\n\n")

cat("Variância simulada:", var(t_beta1), "\n")

cat(
  "Variância teórica:",
  gl / (gl - 2),
  "\n"
)

# =========================================================
# Distribuição do beta1 estimado
# =========================================================

beta1_hats <- numeric(nsim)

for (i in 1:nsim) {
  x <- runif(n, 0, 10)

  erro <- rnorm(n, mean = 0, sd = sigma)

  y <- beta0 + beta1_real * x + erro

  modelo <- lm(y ~ x)

  beta1_hats[i] <- coef(modelo)[2]
}

hist(
  beta1_hats,
  probability = TRUE,
  breaks = 40,
  main = expression("Distribuição de " * hat(beta)[1]),
  xlab = expression(hat(beta)[1])
)

abline(v = beta1_real, lwd = 3)


# teste ------------------------------------------------------------------

beta1_chap |> hist()

rss_scaled |> hist()


divisao <- beta1_chap / rss_scaled

divisao |> hist()
