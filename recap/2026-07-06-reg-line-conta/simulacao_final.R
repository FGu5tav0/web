# =========================================================
# SIMULAÇÃO DE RETAS DE REGRESSÃO
# =========================================================
# Autoria: —
# Descrição: Gera 4 GIFs animados mostrando a variabilidade
#   amostral de β̂₀, β̂₁, RSS/σ² e das retas ajustadas em
#   1 000 amostras simuladas de um modelo de regressão linear
#   simples.
# =========================================================

# ----- Pacotes -------------------------------------------
library(tidyverse)
library(gganimate)
library(gifski)
library(viridis)

# =========================================================
# CONFIGURAÇÕES GLOBAIS (edite aqui)
# =========================================================

CFG <- list(
  seed = 123,
  n_obs = 30, # observações por amostra
  n_sim = 1000, # número de simulações
  beta0 = 76.0952381, # intercepto verdadeiro
  beta1 = 5.857142857, # inclinação verdadeira
  sigma = 40, # desvio-padrão do erro
  n_breaks = 25, # barras nos histogramas
  # Animação
  n_frames = 120, # frames efetivos por GIF (↓ = mais rápido)
  #   Renderizar 1 000 frames é o maior gargalo. Com 120 frames
  #   em escala logarítmica a animação parece fluida: mudanças
  #   rápidas no início, convergência suave no final.
  fps = 20,
  duration = 15, # segundos por GIF
  # Saída
  out_lines = "linhas_regressao.gif",
  out_b0 = "beta0.gif",
  out_b1 = "beta1.gif",
  out_rss = "rss.gif"
)

# =========================================================
# TEMA PERSONALIZADO
# =========================================================

tema_sim <- theme_minimal(base_size = 20) +
  theme(
    plot.title = element_text(face = "bold", size = 22),
    plot.subtitle = element_text(color = "grey40"),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# =========================================================
# 1. DADOS OBSERVADOS (amostra fixa de referência)
# =========================================================

set.seed(CFG$seed)

x <- seq(1, 15, length.out = CFG$n_obs)

dados <- tibble(
  x = x,
  y = CFG$beta0 + CFG$beta1 * x + rnorm(CFG$n_obs, 0, CFG$sigma)
)

# =========================================================
# 2. SIMULAÇÕES VETORIZADAS
# =========================================================
# Gera todas as amostras de uma vez em matriz → muito mais
# rápido do que iterar com map_dfr linha a linha.
# =========================================================

set.seed(CFG$seed + 1)

# Matriz de erros: linhas = observações, colunas = simulações
erros <- matrix(
  rnorm(CFG$n_obs * CFG$n_sim, 0, CFG$sigma),
  nrow = CFG$n_obs,
  ncol = CFG$n_sim
)

# Matriz de respostas simuladas
Y <- CFG$beta0 + CFG$beta1 * x + erros # broadcasting via recycling

# Ajuste vetorizado via álgebra matricial de MQO
# X_mat é a matriz de design [1 | x]
X_mat <- cbind(1, x)
XtXinv <- solve(crossprod(X_mat)) # (X'X)^{-1} — calculado 1 única vez
B <- XtXinv %*% t(X_mat) %*% Y # (2 × n_sim): linha 1 = β̂₀, linha 2 = β̂₁

beta0_hat <- B[1, ]
beta1_hat <- B[2, ]

# Valores ajustados e RSS
Y_hat <- X_mat %*% B # (n_obs × n_sim)
RSS <- colSums((Y - Y_hat)^2) / CFG$sigma^2

# =========================================================
# 3. TABELAS PARA ANIMAÇÃO
# =========================================================

betas <- tibble(
  sim = seq_len(CFG$n_sim),
  beta0 = beta0_hat,
  beta1 = beta1_hat,
  rss = RSS
)

# Retas: expande apenas as simulações necessárias
resultados <- betas %>%
  select(sim, beta0, beta1) %>%
  mutate(
    # Gera pares (x, ŷ) para extremos — suficiente para geom_line
    x_min = min(x),
    x_max = max(x),
    y_min = beta0 + beta1 * min(x),
    y_max = beta0 + beta1 * max(x)
  )

# =========================================================
# 4. AMOSTRAGEM DE FRAMES (escala logarítmica)
# =========================================================
# Renderizar 1 000 frames é o maior gargalo do pipeline.
# Solução: selecionar ~120 estados representativos em escala
# logarítmica — muitos frames no início (onde a distribuição
# muda rapidamente) e poucos no final (convergência lenta).
#
# A percepção visual de "fluidez" depende das MUDANÇAS entre
# frames, não do número absoluto; log-spacing preserva isso.
# =========================================================

frames_log <- unique(round(
  exp(seq(log(1), log(CFG$n_sim), length.out = CFG$n_frames))
))
# Garante que o último estado (n_sim) sempre apareça
frames_log <- sort(union(frames_log, CFG$n_sim))

betas_frames <- betas %>% filter(sim %in% frames_log)
resultados_frames <- resultados %>% filter(sim %in% frames_log)

# =========================================================
# 5. HISTOGRAMAS PROGRESSIVOS — FUNÇÃO AUXILIAR
# =========================================================
# Calcula histogramas acumulados com cut() + complete(),
# evitando loops explícitos em R.
# =========================================================

hist_progressivo <- function(valores, n_breaks = CFG$n_breaks) {
  brks <- seq(min(valores), max(valores), length.out = n_breaks + 1)
  mids <- (brks[-length(brks)] + brks[-1]) / 2
  grupos <- seq_len(n_breaks)

  bins <- cut(valores, breaks = brks, include.lowest = TRUE, labels = FALSE)

  # Itera apenas sobre os frames selecionados (não todos os 1 000)
  map_dfr(frames_log, function(i) {
    contagens <- tabulate(bins[seq_len(i)], nbins = n_breaks)
    tibble(
      sim = i,
      grupo = grupos,
      xmin = brks[-length(brks)],
      xmax = brks[-1],
      xmid = mids,
      y = contagens
    )
  })
}

hist_beta0 <- hist_progressivo(betas$beta0)
hist_beta1 <- hist_progressivo(betas$beta1)
hist_rss <- hist_progressivo(betas$rss)

# =========================================================
# 6. CURVAS TEÓRICAS (sobreposição nos histogramas)
# =========================================================
# Variâncias teóricas de β̂₀ e β̂₁ pelo teorema de Gauss-Markov

Sxx <- sum((x - mean(x))^2)
var_b0 <- CFG$sigma^2 * (1 / CFG$n_obs + mean(x)^2 / Sxx)
var_b1 <- CFG$sigma^2 / Sxx

# Largura do bin (para escalar a densidade → frequência)
bw_b0 <- diff(range(betas$beta0)) / CFG$n_breaks
bw_b1 <- diff(range(betas$beta1)) / CFG$n_breaks

curva_b0 <- tibble(
  x = seq(min(betas$beta0), max(betas$beta0), length.out = 300),
  y = dnorm(x, mean = CFG$beta0, sd = sqrt(var_b0)) * CFG$n_sim * bw_b0
)

curva_b1 <- tibble(
  x = seq(min(betas$beta1), max(betas$beta1), length.out = 300),
  y = dnorm(x, mean = CFG$beta1, sd = sqrt(var_b1)) * CFG$n_sim * bw_b1
)

# Qui-quadrado teórica para RSS/σ² ~ χ²(n-2)
gl <- CFG$n_obs - 2
bw_rss <- diff(range(betas$rss)) / CFG$n_breaks

curva_rss <- tibble(
  x = seq(min(betas$rss), max(betas$rss), length.out = 300),
  y = dchisq(x, df = gl) * CFG$n_sim * bw_rss
)

# =========================================================
# 7. ANIMAÇÃO — RETAS DE REGRESSÃO
# =========================================================

g_linhas <- ggplot() +

  # Retas simuladas (segmentos — mais leve que geom_line com dados longos)
  geom_segment(
    data = resultados_frames,
    aes(
      x = x_min,
      xend = x_max,
      y = y_min,
      yend = y_max,
      color = beta1,
      group = sim
    ),
    linewidth = 0.8,
    alpha = 0.40
  ) +

  # Reta verdadeira
  geom_abline(
    intercept = CFG$beta0,
    slope = CFG$beta1,
    linewidth = 1.4,
    linetype = "dashed",
    color = "black"
  ) +

  # Pontos observados
  geom_point(
    data = dados,
    aes(x, y),
    size = 2.5,
    color = "black",
    alpha = 0.8
  ) +

  scale_color_viridis_c(
    option = "C",
    name = expression(hat(beta)[1])
  ) +

  labs(
    title = "Possíveis retas de regressão",
    subtitle = "Acumulado até simulação {closest_state}",
    x = "x",
    y = "y"
  ) +

  tema_sim +
  theme(legend.position = "right") +

  transition_states(
    sim,
    transition_length = 1,
    state_length = 0
  ) +

  shadow_mark(
    past = TRUE,
    future = FALSE,
    alpha = 0.06
  )

animate(
  g_linhas,
  nframes = length(frames_log),
  width = 900,
  height = 700,
  fps = CFG$fps,
  renderer = gifski_renderer(CFG$out_lines)
)

# =========================================================
# 8. FUNÇÃO AUXILIAR — ANIMA HISTOGRAMA
# =========================================================

animar_hist <- function(
  dados_hist,
  curva_teo,
  vline_x = NULL,
  cor_fill,
  titulo,
  eixo_x,
  arquivo
) {
  g <- ggplot(dados_hist) +

    # Barras acumuladas
    geom_rect(
      aes(
        xmin = xmin,
        xmax = xmax,
        ymin = 0,
        ymax = y,
        group = grupo
      ),
      fill = cor_fill,
      color = "white",
      alpha = 0.85
    ) +

    # Curva teórica (escalonada para frequência)
    geom_line(
      data = curva_teo,
      aes(x, y),
      color = "black",
      linewidth = 1.1,
      linetype = "solid"
    ) +

    labs(
      title = titulo,
      subtitle = "Acumulado até simulação {closest_state}",
      x = eixo_x,
      y = "Frequência"
    ) +

    tema_sim +

    transition_states(
      sim,
      transition_length = 1,
      state_length = 0
    )

  # Linha do parâmetro verdadeiro — só adiciona se fornecida
  if (!is.null(vline_x)) {
    g <- g +
      geom_vline(
        xintercept = vline_x,
        linewidth = 1.3,
        linetype = "dashed",
        color = "black"
      )
  }

  animate(
    g,
    nframes = length(frames_log), # 1 frame por estado selecionado
    width = 800,
    height = 600,
    fps = CFG$fps,
    renderer = gifski_renderer(arquivo)
  )
}

# =========================================================
# 9. ANIMAÇÕES — β̂₀, β̂₁, RSS/σ²
# =========================================================

animar_hist(
  dados_hist = hist_beta0,
  curva_teo = curva_b0,
  vline_x = CFG$beta0,
  cor_fill = "brown4",
  titulo = expression("Distribuição amostral de " * hat(beta)[0]),
  eixo_x = expression(hat(beta)[0]),
  arquivo = CFG$out_b0
)

animar_hist(
  dados_hist = hist_beta1,
  curva_teo = curva_b1,
  vline_x = CFG$beta1,
  cor_fill = "chartreuse4",
  titulo = expression("Distribuição amostral de " * hat(beta)[1]),
  eixo_x = expression(hat(beta)[1]),
  arquivo = CFG$out_b1
)

animar_hist(
  dados_hist = hist_rss,
  curva_teo = curva_rss,
  vline_x = NULL, # sem vline para RSS
  cor_fill = "mediumpurple3",
  titulo = expression(
    "Distribuição de RSS / " * sigma^2 * " ~ " * chi^2 * "(n-2)"
  ),
  eixo_x = expression("RSS / " * sigma^2),
  arquivo = CFG$out_rss
)
