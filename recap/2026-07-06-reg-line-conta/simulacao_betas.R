# =========================================================
# SIMULAÇÃO DE RETAS DE REGRESSÃO
# SALVANDO 3 ANIMAÇÕES SEPARADAS
#
# 1. Retas de regressão
# 2. Distribuição beta0
# 3. Distribuição beta1
# =========================================================

library(tidyverse)
library(gganimate)
library(transformr)

set.seed(123)

# =========================================================
# 1. DADOS
# =========================================================

n <- 30

x <- seq(1, 10, length.out = n)

beta0_real <- 5
beta1_real <- 2.3
sigma <- 3 # desvio padrão do erro
sigma2 <- sigma^2
erro <- rnorm(n, mean = 0, sd = sigma)
y <- beta0_real + beta1_real * x + erro

dados <- tibble(x, y)

# =========================================================
# 2. SIMULAÇÕES
# =========================================================

n_sim <- 1000

resultados <- map_dfr(1:n_sim, function(i) {
  erro <- rnorm(n, mean = 0, sd = sigma)
  y_sim <- beta0_real + beta1_real * x + erro

  modelo <- lm(y_sim ~ x)

  rss <- sum(residuals(modelo)^2) / sigma2

  tibble(
    sim = i,
    x = x,
    y_hat = predict(modelo),
    beta0 = coef(modelo)[1],
    beta1 = coef(modelo)[2],
    rss = rss
  )
})

# =========================================================
# 3. TABELA DOS BETAS
# =========================================================

betas <- resultados %>%
  distinct(sim, beta0, beta1)


rss_tab <- resultados %>%
  distinct(sim, rss)

# =========================================================
# 4. DADOS DOS HISTOGRAMAS PROGRESSIVOS
# =========================================================

hist_beta0 <- map_dfr(1:n_sim, function(i) {
  h <- hist(
    betas$beta0[1:i],
    breaks = 25,
    plot = FALSE
  )

  tibble(
    sim = i,
    xmin = h$breaks[-length(h$breaks)],
    xmax = h$breaks[-1],
    y = h$counts
  )
})

hist_beta1 <- map_dfr(1:n_sim, function(i) {
  h <- hist(
    betas$beta1[1:i],
    breaks = 25,
    plot = FALSE
  )

  tibble(
    sim = i,
    xmin = h$breaks[-length(h$breaks)],
    xmax = h$breaks[-1],
    y = h$counts
  )
})

# =========================================================
# 5. ANIMAÇÃO DAS RETAS
# =========================================================

g_linhas <- ggplot() +

  geom_point(
    data = dados,
    aes(x, y),
    size = 3,
    color = "black"
  ) +

  geom_line(
    data = resultados,
    aes(
      x = x,
      y = y_hat,
      group = sim,
      color = beta1,
      alpha = beta1
    ),
    # col = "gray50",
    linewidth = 1,
    # alpha = 0.55
  ) +

  scale_color_viridis_c(option = "C") +

  labs(
    title = "Possíveis retas de regressão",
    subtitle = "Simulação: {closest_state}",
    x = "x",
    y = "y",
    color = ""
  ) +

  theme_minimal(base_size = 16) +
  theme(legend.position = "none") +

  transition_states(
    sim,
    transition_length = 1,
    state_length = 1
  ) +

  shadow_mark(past = TRUE)

# =========================================================
# SALVAR
# =========================================================

animate(
  g_linhas,
  width = 900,
  height = 700,
  fps = 20,
  duration = 15,
  renderer = gifski_renderer(
    "linhas_regressao.gif"
  )
)

# =========================================================
# 6. ANIMAÇÃO BETA0
# =========================================================

g_beta0 <- ggplot(
  hist_beta0,
  aes(
    xmin = xmin,
    xmax = xmax,
    ymin = 0,
    ymax = y
  )
) +

  geom_rect(
    fill = "#FF6B6B",
    color = "white",
    alpha = 0.9
  ) +

  geom_vline(
    xintercept = beta0_real,
    linewidth = 1.2,
    linetype = 2
  ) +

  labs(
    title = "Distribuição de Beta 0",
    subtitle = "Simulação: {closest_state}",
    x = "Beta 0",
    y = "Frequência"
  ) +

  theme_minimal(base_size = 16) +

  transition_states(
    sim,
    transition_length = 1,
    state_length = 1
  )

# =========================================================
# SALVAR
# =========================================================

animate(
  g_beta0,
  width = 800,
  height = 600,
  fps = 20,
  duration = 15,
  renderer = gifski_renderer(
    "beta0.gif"
  )
)

# =========================================================
# 7. ANIMAÇÃO BETA1
# =========================================================

g_beta1 <- ggplot(
  hist_beta1,
  aes(
    xmin = xmin,
    xmax = xmax,
    ymin = 0,
    ymax = y
  )
) +

  geom_rect(
    fill = "#4ECDC4",
    color = "white",
    alpha = 0.9
  ) +

  geom_vline(
    xintercept = beta1_real,
    linewidth = 1.2,
    linetype = 2
  ) +

  labs(
    title = "Distribuição de Beta 1",
    subtitle = "Simulação: {closest_state}",
    x = "Beta 1",
    y = "Frequência"
  ) +

  theme_minimal(base_size = 16) +

  transition_states(
    sim,
    transition_length = 1,
    state_length = 1
  )

# =========================================================
# SALVAR
# =========================================================

animate(
  g_beta1,
  width = 800,
  height = 600,
  fps = 20,
  duration = 15,
  renderer = gifski_renderer(
    "beta1.gif"
  )
)


# animação erros ---------------------------------------------------------

hist_rss <- map_dfr(1:n_sim, function(i) {
  h <- hist(
    rss_tab$rss[1:i],
    breaks = 25,
    plot = FALSE
  )

  tibble(
    sim = i,
    xmin = h$breaks[-length(h$breaks)],
    xmax = h$breaks[-1],
    y = h$counts
  )
})

g_rss <- ggplot(
  hist_rss,
  aes(
    xmin = xmin,
    xmax = xmax,
    ymin = 0,
    ymax = y
  )
) +

  geom_rect(
    fill = "#FF6B6B",
    color = "white",
    alpha = 0.9
  ) +

  # geom_vline(
  #   xintercept = mean(hist()),
  #   linewidth = 1.2,
  #   linetype = 2
  # ) +

  labs(
    title = "Distribuição dos erros",
    subtitle = "Simulação: {closest_state}",
    x = expression(RSS / sigma^2),
    y = "Frequência"
  ) +

  theme_minimal(base_size = 16) +

  transition_states(
    sim,
    transition_length = 1,
    state_length = 1
  )

# =========================================================
# SALVAR
# =========================================================

animate(
  g_rss,
  width = 800,
  height = 600,
  fps = 20,
  duration = 15,
  renderer = gifski_renderer(
    "rss.gif"
  )
)
