# =========================================================
# SIMULAÇÃO DE RETAS DE REGRESSÃO
# SALVANDO 4 ANIMAÇÕES SEPARADAS
# =========================================================

library(tidyverse)
library(gganimate)
library(transformr)
library(gifski)
library(viridis)
library(latex2exp)

set.seed(123)

# =========================================================
# 1. DADOS
# =========================================================

n <- 30

x <- seq(1, 15, length.out = n)

beta0_real <- 76.0952381
beta1_real <- 5.857142857

sigma <- 40
sigma2 <- sigma^2

erro <- rnorm(n, 0, sigma)

y <- beta0_real + beta1_real * x + erro

dados <- tibble(x, y)

# =========================================================
# 2. SIMULAÇÕES
# =========================================================

n_sim <- 1000

resultados <- map_dfr(1:n_sim, function(i) {
  erro <- rnorm(n, 0, sigma)

  y_sim <- beta0_real + beta1_real * x + erro

  modelo <- lm(y_sim ~ x)

  tibble(
    sim = i,
    x = x,
    y_hat = predict(modelo),
    beta0 = coef(modelo)[1],
    beta1 = coef(modelo)[2],
    rss = sum(residuals(modelo)^2) / sigma2
  )
})

# =========================================================
# 3. TABELAS
# =========================================================

betas <- resultados %>%
  distinct(sim, beta0, beta1)

rss_tab <- resultados %>%
  distinct(sim, rss)

# =========================================================
# 4. HISTOGRAMAS PROGRESSIVOS
# =========================================================

# -------- beta0 --------

breaks_beta0 <- seq(
  min(betas$beta0),
  max(betas$beta0),
  length.out = 26
)

hist_beta0 <- map_dfr(1:n_sim, function(i) {
  h <- hist(
    betas$beta0[1:i],
    breaks = breaks_beta0,
    plot = FALSE
  )

  tibble(
    sim = i,
    grupo = seq_along(h$counts),
    xmin = h$breaks[-length(h$breaks)],
    xmax = h$breaks[-1],
    y = h$counts
  )
})

# -------- beta1 --------

breaks_beta1 <- seq(
  min(betas$beta1),
  max(betas$beta1),
  length.out = 26
)

hist_beta1 <- map_dfr(1:n_sim, function(i) {
  h <- hist(
    betas$beta1[1:i],
    breaks = breaks_beta1,
    plot = FALSE
  )

  tibble(
    sim = i,
    grupo = seq_along(h$counts),
    xmin = h$breaks[-length(h$breaks)],
    xmax = h$breaks[-1],
    y = h$counts
  )
})

# -------- rss --------

breaks_rss <- seq(
  min(rss_tab$rss),
  max(rss_tab$rss),
  length.out = 26
)

hist_rss <- map_dfr(1:n_sim, function(i) {
  h <- hist(
    rss_tab$rss[1:i],
    breaks = breaks_rss,
    plot = FALSE
  )

  tibble(
    sim = i,
    grupo = seq_along(h$counts),
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
      color = beta1
    ),
    linewidth = 1,
    alpha = 0.35
  ) +

  scale_color_viridis_c(option = "C") +

  labs(
    title = "Possíveis retas de regressão",
    subtitle = "Simulação: {closest_state}",
    x = "x",
    y = "y"
  ) +

  theme_minimal(base_size = 22) +
  theme(legend.position = "none") +

  transition_states(
    sim,
    transition_length = 1,
    state_length = 1
  ) +

  shadow_mark(
    past = TRUE,
    future = FALSE,
    alpha = 0.08
  )

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
  hist_beta0
) +

  geom_rect(
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = 0,
      ymax = y,
      group = grupo
    ),
    fill = "brown4",
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
    x = "\U03B2 \U0302 \U2080",
    y = "Frequência"
  ) +

  theme_minimal(base_size = 22) +

  transition_states(
    sim,
    transition_length = 1,
    state_length = 1
  )

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
  hist_beta1
) +

  geom_rect(
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = 0,
      ymax = y,
      group = grupo
    ),
    fill = "chartreuse4",
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
    x = "\U03B2 \U0302 \U2081",
    y = "Frequência"
  ) +

  theme_minimal(base_size = 22) +

  transition_states(
    sim,
    transition_length = 1,
    state_length = 1
  )

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

# =========================================================
# 8. ANIMAÇÃO RSS
# =========================================================

g_rss <- ggplot(
  hist_rss
) +

  geom_rect(
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = 0,
      ymax = y,
      group = grupo
    ),
    fill = "mediumpurple3",
    color = "white",
    alpha = 0.9
  ) +

  labs(
    title = "Distribuição de resíduos",
    subtitle = "Simulação: {closest_state}",
    x = "RSS / \U03C3 \U00B2",
    y = "Frequência"
  ) +

  theme_minimal(base_size = 22) +

  transition_states(
    sim,
    transition_length = 1,
    state_length = 1
  )

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
