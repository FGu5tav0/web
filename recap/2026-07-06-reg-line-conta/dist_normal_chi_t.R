# =========================================================
# Normal / raiz(Chi²/gl) = distribuição t
# =========================================================

library(tidyverse)

set.seed(123)


# =========================================================
# 1. DISTRIBUIÇÃO NORMAL PADRÃO
# =========================================================

# sequência de x
x <- seq(10, 30, length.out = 1000)

# densidade normal
dados_norm <- tibble(
  x = x,
  y = dnorm(x, mean = 20, sd = 2)
)

graf_normal <- ggplot(dados_norm, aes(x = x, y = y)) +

  # preenchimento
  geom_area(
    fill = "skyblue",
    alpha = 0.5
  ) +

  # linha
  geom_line(
    color = "red",
    linewidth = 1.2
  ) +
  xlim(10, 30) +

  labs(
    title = "Distribuição Normal Padrão",
    subtitle = "Área abaixo da curva",
    x = "Valores",
    y = "Densidade"
  ) +

  theme_minimal(base_size = 14)

graf_normal

# =========================================================
# 2. DISTRIBUIÇÃO QUI-QUADRADO
# =========================================================

gl <- 5

# sequência de x
x <- seq(0, 25, length.out = 1000)

# densidade teórica Qui²
dados_chi_teorico <- tibble(
  x = x,
  y = dchisq(x, df = gl)
)

graf_chi <- ggplot(dados_chi_teorico, aes(x = x, y = y)) +

  # preenchimento
  geom_area(
    fill = "orange",
    alpha = 0.5
  ) +

  # linha da densidade
  geom_line(
    color = "red",
    linewidth = 1.2
  ) +

  labs(
    title = paste0("Distribuição Qui-Quadrado (gl = ", gl, ")"),
    subtitle = "Área abaixo da curva de densidade",
    x = "Valores",
    y = "Densidade"
  ) +

  theme_minimal(base_size = 14)

graf_chi

# =========================================================
# 3. CONSTRUÇÃO DA DISTRIBUIÇÃO t
# =========================================================
Z <- rnorm(100000, mean = 20, sd = 2)
U <- rchisq(100000, df = gl)

T_sim <- (Z - 20) / 2 / sqrt(U / gl)

dados_t <- tibble(
  valor = T_sim
)

graf_t <- ggplot(dados_t, aes(x = valor)) +

  # área preenchida
  geom_density(
    fill = "skyblue",
    alpha = 0.4,
    color = NA
  ) +

  # densidade simulada
  geom_density(
    linewidth = 1.2
  ) +
  xlim(-10, 10) +

  labs(
    title = "Distribuição t construída via Normal / Qui²",
    subtitle = paste0("Graus de liberdade = ", gl),
    x = "Valores",
    y = "Densidade"
  ) +

  theme_minimal(base_size = 14)

graf_t
