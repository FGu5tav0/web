# =========================================================
# Animação do R²
# - mesma reta
# - mesmos x
# - mesmos pontos
# - apenas a distância vertical muda
# =========================================================

library(tidyverse)
library(gganimate)
library(extrafont)
library(tidyverse)
library(systemfonts)

# link www.fontsquirrel.com/fonts/latin-modern-roman

# execute once to add fonts:
# extrafont::font_install("fontcm")
# font_import()
# register_font("")
# fonts <- system_fonts()

# font_add_google("Gravitas One", "poppins")
# showtext_auto()
# showtext_opts(dpi = 300) # para manter o tamanho da fonte

# showtext_auto()
# showtext_opts(dpi = 300) # para manter o tamanho da fonte

set.seed(123)

# ---------------------------------------------------------
# Dados fixos
# ---------------------------------------------------------

n <- 80

x <- seq(0, 10, length.out = n)

# reta fixa
beta0 <- 5
beta1 <- 3

y_reta <- beta0 + beta1 * x

# ---------------------------------------------------------
# Resíduos fixos
# Cada ponto mantém sua identidade
# ---------------------------------------------------------

residuos_base <- rnorm(n)

# padronizar
residuos_base <- residuos_base / sd(residuos_base)

# ---------------------------------------------------------
# Intensidade do ruído
# ---------------------------------------------------------

ruidos <- seq(14, 0.3, length.out = 60)

# ---------------------------------------------------------
# Construção dos frames
# ---------------------------------------------------------

dados_anim <- map_dfr(seq_along(ruidos), function(i) {
  ruido <- ruidos[i]

  # mesmos pontos movendo apenas em y
  y <- y_reta + residuos_base * ruido

  # R² calculado em relação à reta fixa
  r2 <- cor(y, y_reta)^2

  tibble(
    id = 1:n,
    x = x,
    y = y,
    y_reta = y_reta,
    r2 = r2,
    frame = paste0(
      "R² = ",
      round(r2, 2)
    )
  )
})

# ---------------------------------------------------------
# Gráfico
# ---------------------------------------------------------

g <- ggplot(dados_anim, aes(x, y, group = id)) +

  # segmentos dos resíduos
  geom_segment(
    aes(
      xend = x,
      yend = y_reta
    ),
    alpha = 0.25
  ) +

  # pontos
  geom_point(
    size = 2.5,
    alpha = 0.85
  ) +

  # reta fixa
  geom_abline(slope = 3, intercept = 5, col = "red") +

  labs(
    title = "{closest_state}",
    # subtitle = "A reta permanece fixa; apenas os resíduos diminuem",
    x = "x",
    y = "y"
  ) +

  coord_cartesian(
    ylim = c(-25, 45)
  ) +
  theme(text = element_text(family = "Latin Modern Roman", size = 22)) +
  theme_minimal(base_size = 22, base_family = "Latin Modern Roman")

# ---------------------------------------------------------
# Animação
# ---------------------------------------------------------

animacao <- g +

  transition_states(
    frame,
    transition_length = 1,
    state_length = 1
  ) +

  ease_aes("cubic-in-out")

# ---------------------------------------------------------
# Renderizar GIF
# ---------------------------------------------------------

animate(
  animacao,
  width = 800,
  height = 600,
  fps = 20,
  duration = 15,
  renderer = gifski_renderer("r2_reta_fixa.gif")
)
