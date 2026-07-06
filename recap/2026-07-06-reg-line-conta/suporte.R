library(tidyverse)
library(gt)
library(ggview)
library(patchwork)
library(latex2exp)

dados <- data.frame(
  x = c(5, 7, 9, 11, 13, 15),
  y = c(110, 120, 120, 135, 155, 168)
)

mod <- lm(y ~ x, dados)
summary(mod)

broom::tidy(mod, conf.int = TRUE)

confint(object = mod, parm = "x", level = 0.95)
novo <- data.frame(x = c(5, 7, 9, 11, 13, 15))
banda <- predict(mod, newdata = novo, interval = "confidence", level = 0.95)
banda <- banda |> as.data.frame()
banda$x <- dados$x

dados |>
  gt() |>
  cols_label(
    x = html("x - Idade (anos) <br> (Variável independente)"),
    y = html("y - Altura (cm) <br> (Variável dependente)"),
  ) |>
  tab_header(
    title = "Dados de exemplo"
  ) |>
  cols_align(align = "center") |>
  tab_style(
    style = cell_fill(color = "#F2F2F2"),
    locations = cells_body()
  ) -> table1

gtsave(data = table1, filename = "tabela1.html")


# dispe ------------------------------------------------------------------

ggplot(dados) +
  aes(x, y) +
  xlim(0, 20) +
  ylim(0, 200) +
  geom_segment(
    mapping = aes(x = x, y = 0, xend = x, yend = y),
    linetype = "dashed",
    linewidth = .3,
    col = "gray59"
  ) +
  geom_segment(
    mapping = aes(x = 0, y = y, xend = x, yend = y),
    linetype = "dashed",
    linewidth = .3,
    col = "gray59"
  ) +
  geom_point(size = 2) +
  coord_cartesian(expand = F) +
  labs(x = "Idade, anos", y = "Altura, cm") +
  ggrepel::geom_text_repel(
    mapping = aes(x = x, y = y, label = paste0("(", x, ";", y + 10, ")")),
    size = 2.5,
    nudge_x = -0.5,
    nudge_y = .9
  ) +
  #   annotate(geom = "text", x = 13.8, y = 184, label = "(x;y)",
  #  size = 2.5, col = "red") +
  theme_classic(base_family = "Latin Modern Roman") +
  canvas(width = 5, height = 4, dpi = 600) -> p1

p1

save_ggplot(plot = p1, file = "disper.jpeg")


# com reg ----------------------------------------------------------------

media_y <- mean(dados$y)
media_x <- mean(dados$x)

ggplot(dados) +
  aes(x, y) +
  xlim(0, 20) +
  ylim(0, 200) +
  # geom_hline(yintercept = media_y, linetype = "dashed", col = "tomato") +
  # geom_vline(xintercept = media_x, linetype = "dashed", col = "tomato") +
  # geom_segment(
  #   mapping = aes(x = x, y = 0, xend = x, yend = y),
  #   linetype = "dashed",
  #   linewidth = .3,
  #   col = "gray59"
  # ) +
  # geom_segment(
  #   mapping = aes(x = 0, y = y, xend = x, yend = y),
  #   linetype = "dashed",
  #   linewidth = .3,
  #   col = "gray59"
  # ) +
  geom_point(size = 2) +
  coord_cartesian(expand = F) +
  # geom_point(data = banda, mapping = aes(x, lwr), col = "red", pch = 4) +
  # geom_point(data = banda, mapping = aes(x, upr), col = "red", pch = 4) +
  labs(x = "Idade, anos", y = "Altura, cm") +
  geom_smooth(method = "lm", se = T, level = 0.95, col = "firebrick") +
  ggpmisc::stat_poly_eq(
    ggpmisc::use_label("eq", "R2", "F", "p"),
    formula = y ~ x
  ) +
  theme_classic(base_family = "Latin Modern Roman") +
  canvas(width = 5, height = 4, dpi = 600) -> p2

p2

save_ggplot(plot = p2, file = "disper_reg_smot.jpeg")


ggplot(dados) +
  aes(x, y) +
  xlim(0, 20) +
  ylim(0, 200) +
  # geom_hline(yintercept = media_y, linetype = "dashed", col = "tomato") +
  # geom_vline(xintercept = media_x, linetype = "dashed", col = "tomato") +
  # geom_segment(
  #   mapping = aes(x = x, y = 0, xend = x, yend = y),
  #   linetype = "dashed",
  #   linewidth = .3,
  #   col = "gray59"
  # ) +
  # geom_segment(
  #   mapping = aes(x = 0, y = y, xend = x, yend = y),
  #   linetype = "dashed",
  #   linewidth = .3,
  #   col = "gray59"
  # ) +
  geom_point(size = 2) +
  coord_cartesian(expand = F) +
  geom_point(data = banda, mapping = aes(x, lwr), col = "red", pch = 4) +
  geom_line(
    data = banda,
    mapping = aes(x, lwr),
    col = "#ffab56",
    linetype = "dashed"
  ) +
  geom_point(data = banda, mapping = aes(x, upr), col = "red", pch = 4) +
  geom_line(
    data = banda,
    mapping = aes(x, upr),
    col = "#ffab56",
    linetype = "dashed"
  ) +
  labs(x = "Idade, anos", y = "Altura, cm") +
  geom_smooth(method = "lm", se = F, level = 0.95, col = "firebrick") +
  ggpmisc::stat_poly_eq(
    ggpmisc::use_label("eq", "R2"),
    formula = y ~ x
  ) +
  theme_classic(base_family = "Latin Modern Roman") -> p3

p3

ggplot(dados) +
  aes(x, y) +
  xlim(0, 20) +
  ylim(0, 200) +
  # geom_hline(yintercept = media_y, linetype = "dashed", col = "tomato") +
  # geom_vline(xintercept = media_x, linetype = "dashed", col = "tomato") +
  # geom_segment(
  #   mapping = aes(x = x, y = 0, xend = x, yend = y),
  #   linetype = "dashed",
  #   linewidth = .3,
  #   col = "gray59"
  # ) +
  # geom_segment(
  #   mapping = aes(x = 0, y = y, xend = x, yend = y),
  #   linetype = "dashed",
  #   linewidth = .3,
  #   col = "gray59"
  # ) +
  geom_point(size = 2) +
  coord_cartesian(expand = F) +
  # geom_point(data = banda, mapping = aes(x, lwr), col = "red", pch = 4) +
  # geom_line(
  #   data = banda,
  #   mapping = aes(x, lwr),
  #   col = "#ffab56",
  #   linetype = "dashed"
  # ) +
  # geom_point(data = banda, mapping = aes(x, upr), col = "red", pch = 4) +
  # geom_line(
  #   data = banda,
  #   mapping = aes(x, upr),
  #   col = "#ffab56",
  #   linetype = "dashed"
  # ) +
  labs(x = "Idade, anos", y = "Altura, cm") +
  geom_smooth(method = "lm", se = T, level = 0.95, col = "firebrick") +
  ggpmisc::stat_poly_eq(
    ggpmisc::use_label("eq", "R2"),
    formula = y ~ x
  ) +
  theme_classic(base_family = "Latin Modern Roman") -> p4

p4

pt <- p2 | p3 | p4
pt <- pt + canvas(width = 10, height = 5, dpi = 600)
pt

save_ggplot(plot = pt, file = "final.jpeg")
# p1 | p2

# reg nula ---------------------------------------------------------------

ggplot(dados) +
  aes(x, y) +
  xlim(0, 20) +
  ylim(0, 200) +
  geom_hline(yintercept = media_y, linetype = "dashed", col = "tomato") +
  geom_vline(xintercept = media_x, linetype = "dashed", col = "tomato") +
  # geom_segment(
  #   mapping = aes(x = x, y = y, xend = media_x, yend = y),
  #   linetype = "dashed",
  #   linewidth = .5,
  #   col = "gray59"
  # ) +
  # geom_segment(
  #   mapping = aes(x = x, y = y, xend = x, yend = media_y),
  #   linetype = "dashed",
  #   linewidth = .5,
  #   col = "gray59"
  # ) +
  geom_point(size = 2) +
  coord_cartesian(expand = F) +
  # geom_smooth(method = "lm", se = F) +
  labs(x = "Idade, anos", y = "Altura, cm") +
  annotate(
    geom = "point",
    x = media_x,
    y = media_y,
    col = "firebrick",
    size = 4
  ) +
  geom_text(
    mapping = aes(
      x = 5,
      y = 160,
      label = TeX(
        r"($\bar{y} = \frac{\sum y_i}{n} = 135~cm$)",
        output = "character",
        bold = F
      )
    ),
    col = "tomato",
    parse = TRUE,
    size = 3
  ) +
  geom_curve(
    aes(
      x = 6,
      y = 153,
      xend = 6,
      yend = 138
    ),
    arrow = arrow(
      length = unit(0.1, "cm")
    ),
    linewidth = 0.5,
    col = "tomato"
  ) +

  geom_text(
    mapping = aes(
      x = media_x + 4,
      y = 50,
      label = TeX(
        r"($\bar{x} = \frac{\sum x_i}{n} = 10~anos$)",
        output = "character",
        bold = F
      )
    ),
    col = "tomato",
    parse = TRUE,
    size = 3
  ) +
  geom_curve(
    aes(
      x = 14,
      y = 55,
      xend = 10.2,
      yend = 60
    ),
    arrow = arrow(
      length = unit(0.1, "cm")
    ),
    linewidth = 0.5,
    col = "tomato"
  ) +
  theme_classic(base_family = "Latin Modern Roman") -> p3
# canvas(width = 5, height = 4, dpi = 600) -> p3

p3

ggplot(dados) +
  aes(x, y) +
  xlim(0, 20) +
  ylim(0, 200) +
  geom_hline(yintercept = media_y, linetype = "dashed", col = "tomato") +
  geom_vline(xintercept = media_x, linetype = "dashed", col = "tomato") +
  geom_segment(
    mapping = aes(x = x, y = y, xend = media_x, yend = y),
    linetype = "dashed",
    linewidth = .5,
    col = "gray59"
  ) +
  geom_segment(
    mapping = aes(x = x, y = y, xend = x, yend = media_y),
    linetype = "dashed",
    linewidth = .5,
    col = "gray59"
  ) +
  geom_point(size = 2) +
  coord_cartesian(expand = F) +
  # geom_smooth(method = "lm", se = F) +
  labs(x = "Idade, anos", y = "Altura, cm") +
  annotate(
    geom = "point",
    x = media_x,
    y = media_y,
    col = "firebrick",
    size = 4
  ) +
  geom_text(
    mapping = aes(
      x = 5,
      y = 160,
      label = TeX(
        r"($\bar{y} = \frac{\sum y_i}{n} = 135~cm$)",
        output = "character",
        bold = F
      )
    ),
    col = "tomato",
    parse = TRUE,
    size = 3
  ) +
  geom_curve(
    aes(
      x = 6,
      y = 153,
      xend = 6,
      yend = 138
    ),
    arrow = arrow(
      length = unit(0.1, "cm")
    ),
    linewidth = 0.5,
    col = "tomato"
  ) +
  geom_text(
    mapping = aes(
      x = media_x + 4,
      y = 50,
      label = TeX(
        r"($\bar{x} = \frac{\sum x_i}{n} = 10~anos$)",
        output = "character",
        bold = F
      )
    ),
    col = "tomato",
    parse = TRUE,
    size = 3
  ) +
  geom_curve(
    aes(
      x = 14,
      y = 55,
      xend = 10.2,
      yend = 60
    ),
    arrow = arrow(
      length = unit(0.1, "cm")
    ),
    linewidth = 0.5,
    col = "tomato"
  ) +
  # calculo de desvios y
  geom_text(
    mapping = aes(
      x = 2,
      y = 125,
      label = TeX(
        r"($\sum(y_i - \bar{y})$)",
        output = "character",
        bold = F
      )
    ),
    col = "gray59",
    parse = TRUE,
    size = 3
  ) +
  geom_curve(
    aes(
      x = 2,
      y = 118,
      xend = 4.8,
      yend = 120
    ),
    arrow = arrow(
      length = unit(0.1, "cm")
    ),
    linewidth = 0.5,
    col = "gray59"
  ) +
  # calculo de desvios x
  geom_text(
    mapping = aes(
      x = 6,
      y = 95,
      label = TeX(
        r"($\sum(x_i - \bar{x})$)",
        output = "character",
        bold = F
      )
    ),
    col = "gray59",
    parse = TRUE,
    size = 3
  ) +
  geom_curve(
    aes(
      x = 6.5,
      y = 100,
      xend = 7,
      yend = 108
    ),
    arrow = arrow(
      length = unit(0.1, "cm")
    ),
    linewidth = 0.5,
    col = "gray59"
  ) +
  theme_classic(base_family = "Latin Modern Roman") -> p4
# canvas(width = 5, height = 4, dpi = 600)

p4


pt <- p3 | p4

pt2 <- pt +
  plot_annotation(tag_levels = "A") +
  canvas(width = 8, height = 4, dpi = 600)
pt2
save_ggplot(plot = pt2, file = "media_media_desvi.jpeg")

# fazer um gráfico mais bonito
# adiconar infos para dizer que se não tivesse x, seria a média

ggplot(dados) +
  aes(x, y) +
  xlim(4, 16) +
  ylim(80, 190) +
  geom_hline(yintercept = media_y, linetype = "dashed", col = "tomato") +
  coord_cartesian(expand = F) +
  labs(x = "Idade, anos", y = "Altura, cm") +
  annotate(
    geom = "point",
    x = media_x,
    y = media_y,
    col = "firebrick",
    size = 2
  ) +
  # média y
  annotate(
    geom = "text",
    x = 6,
    y = 143,
    label = TeX(
      r"($\bar{y} = \frac{\sum y_i}{n} = 135~cm$)",
      output = "character",
      bold = T
    ),
    colour = "tomato",
    parse = TRUE,
    size = 2.5
  ) +
  # explicado pelo modelo
  annotate(
    geom = "text",
    x = 14.7,
    y = 148,
    label = TeX(
      r"($(\hat{y_i} - \bar{y})$)",
      output = "character",
      bold = T
    ),
    colour = "gray59",
    parse = TRUE,
    size = 2.5
  ) +
  annotate(
    geom = "segment",
    x = 14,
    y = media_y,
    xend = 14,
    yend = 5.9 * 14 + 76,
    linewidth = 0.5,
    colour = "gray59",
    linetype = "dashed"
  ) +
  # explicado pelo erro
  annotate(
    geom = "text",
    x = 9.8,
    y = 126,
    label = TeX(
      r"($(y_i - \hat{y_i})$)",
      output = "character",
      bold = T
    ),
    colour = "red",
    parse = TRUE,
    size = 2.5
  ) +
  # erro total
  annotate(
    geom = "text",
    x = 10,
    y = 180,
    label = TeX(
      r"($y_i - \bar{y}=(\hat{y}_i - \bar{y})+(y_i - \hat{y}_i)$)",
      output = "expression",
      bold = T
    ),
    colour = "black",
    parse = TRUE,
    size = 4
  ) +
  ggpmisc::stat_fit_deviations(
    colour = "red",
    linewidth = .5,
    linetype = "dashed"
  ) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = F, linewidth = .5, col = "darkblue") +
  ggpubr::stat_regline_equation(
    label.x = 5,
    label.y = 100,
    col = "darkblue",
    size = 3
  ) +
  theme_classic(base_family = "Latin Modern Roman") +
  canvas(width = 4, height = 3, dpi = 600) -> p5

save_ggplot(plot = p5, file = "desvios_mod_erro.jpeg")


# com quadrados  ---------------------------------------------------------

ggplot(dados) +
  aes(x, y) +
  xlim(4, 16) +
  ylim(80, 190) +
  geom_hline(yintercept = media_y, linetype = "solid", col = "red") +
  coord_cartesian(expand = F, clip = "off") +
  labs(x = "Idade, anos", y = "Altura, cm") +
  geom_segment(
    mapping = aes(x = x, y = media_y, xend = x, yend = y),
    linetype = "dashed",
    linewidth = .3,
    col = "red"
  ) +
  # média y
  annotate(
    geom = "text",
    x = 6,
    y = 143,
    label = TeX(
      r"($\bar{y} = \frac{\sum y_i}{n} = 135~cm$)",
      output = "character",
      bold = T
    ),
    colour = "tomato",
    parse = TRUE,
    size = 2.5
  ) +
  # explicado pelo modelo
  # annotate(
  #   geom = "text",
  #   x = 14.7,
  #   y = 148,
  #   label = TeX(
  #     r"($(\hat{y_i} - \bar{y})$)",
  #     output = "character",
  #     bold = T
  #   ),
  #   colour = "gray59",
  #   parse = TRUE,
  #   size = 2.5
  # ) +

  # explicado pelo erro
  annotate(
    geom = "text",
    x = 9.8,
    y = 126,
    label = TeX(
      r"($(\hat{y_i} - \bar{y})$)",
      output = "character",
      bold = T
    ),
    colour = "red",
    parse = TRUE,
    size = 2.5
  ) +
  # erro total
  # annotate(
  #   geom = "text",
  #   x = 10,
  #   y = 180,
  #   label = TeX(
  #     r"($y_i - \bar{y}=(\hat{y}_i - \bar{y})+(y_i - \hat{y}_i)$)",
  #     output = "expression",
  #     bold = T
  #   ),
  #   colour = "black",
  #   parse = TRUE,
  #   size = 4
  # ) +
  # ggpmisc::stat_fit_deviations(
  #   colour = "red",
  #   linewidth = .5,
  #   linetype = "dashed"
  # ) +
  geom_point(size = 1) +
  # geom_smooth(method = "lm", se = F, linewidth = .5, col = "darkblue") +
  # ggpubr::stat_regline_equation(
  #   label.x = 5,
  #   label.y = 100,
  #   col = "darkblue",
  #   size = 3
  # ) +
  theme_classic() +
  canvas(width = 4, height = 3, dpi = 600) -> p43
p43

#

quadrados <- dados |>
  mutate(
    lado = abs(y - media_y),

    xmin = x,
    xmax = x + lado / 10,

    ymin = pmin(y, media_y),
    ymax = pmax(y, media_y)
  )


ggplot(dados) +
  aes(x, y) +
  xlim(0, 20) +
  ylim(50, 200) +
  geom_hline(yintercept = media_y, linetype = "solid", col = "red") +
  coord_cartesian(expand = F, clip = "off") +
  labs(x = "Idade, anos", y = "Altura, cm") +
  geom_segment(
    mapping = aes(x = x, y = media_y, xend = x, yend = y),
    linetype = "dashed",
    linewidth = .3,
    col = "red"
  ) +
  geom_point(size = 1) +
  geom_rect(
    data = quadrados,
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax
    ),
    fill = "red",
    alpha = .2,
    color = "red"
  ) +
  theme_classic() +
  canvas(width = 4, height = 3, dpi = 600) -> p432
p432

# R 2 --------------------------------------------------------------------

ggplot(dados) +
  aes(x, y) +
  xlim(4, 16) +
  ylim(80, 190) +
  geom_hline(
    yintercept = media_y,
    linetype = "solid",
    col = "tomato",
    linewidth = .8
  ) +
  coord_cartesian(expand = F) +
  labs(x = "Idade, anos", y = "Altura, cm") +
  # média y
  annotate(
    geom = "text",
    x = 5.5,
    y = 138,
    label = TeX(
      r"($\bar{y} = 135~cm$)",
      output = "character",
      bold = T
    ),
    colour = "tomato",
    parse = TRUE,
    size = 2.5
  ) +

  # erro total
  # annotate(
  #   geom = "text",
  #   x = 10,
  #   y = 170,
  #   label = TeX(
  #     r"($R^2 = 1 - \frac{\sum(y_i-\hat{y_i})^2}{\sum(y_i-\bar{y_i})^2}$)",
  #     output = "expression",
  #     bold = T
  #   ),
  #   colour = "black",
  #   parse = TRUE,
  #   size = 4
  # ) +
  geom_point(size = 1, alpha = .5) +
  geom_smooth(method = "lm", se = F, linewidth = .8, col = "blue") +

  theme_classic(base_family = "Latin Modern Roman") +
  canvas(width = 4, height = 3, dpi = 600) -> p6

p6

save_ggplot(plot = p6, file = "r2.jpeg")


# distribuição T ---------------------------------------------------------
library(ggplot2)
library(ggview)


#---------------------------
# Parâmetros
#---------------------------
t_obs <- 9.469889438
t_obs <- 7.70255878

gl <- 4
alpha <- 0.05

# p-valor (teste bicaudal)
p_valor <- 2 * pt(abs(t_obs), df = gl, lower.tail = FALSE)

# valor crítico
t_crit <- qt(1 - alpha / 2, df = gl)


# eixo x
x_lim <- c(-10, 10)
dados <- data.frame(x = seq(x_lim[1], x_lim[2], length.out = 1000))

#---------------------------
# Gráfico
#---------------------------
ggplot(dados, aes(x)) +

  ## Curva t
  stat_function(
    fun = dt,
    args = list(df = gl),
    linewidth = 1.1,
    colour = "#2166AC"
  ) +

  ## Região crítica esquerda
  stat_function(
    geom = "area",
    fun = dt,
    args = list(df = gl),
    xlim = c(x_lim[1], -t_crit),
    fill = "#D73027",
    alpha = .35
  ) +

  ## Região crítica direita
  stat_function(
    geom = "area",
    fun = dt,
    args = list(df = gl),
    xlim = c(t_crit, x_lim[2]),
    fill = "#D73027",
    alpha = .35
  ) +

  ## Valores críticos
  geom_vline(
    xintercept = c(-t_crit, t_crit),
    linetype = "dashed",
    colour = "grey40"
  ) +

  ## Estatística t observada
  annotate(
    geom = "segment",
    x = t_obs,
    y = 0,
    xend = t_obs,
    yend = 0.05,
    linetype = "dashed",
    colour = "darkgreen",
    linewidth = .5
  ) +

  ## Texto do t observado
  annotate(
    "text",
    x = t_obs + 1,
    y = dt(t_obs, gl) + .06,
    label = paste0("t obs = ", round(t_obs, 3)),
    colour = "darkgreen",
    hjust = 1,
    size = 3.4
  ) +

  ## Texto da região crítica
  annotate(
    "text",
    x = t_crit + 0.8,
    y = dt(0, gl) * .18,
    label = expression(alpha / 2),
    colour = "#D73027",
    size = 5
  ) +

  annotate(
    "text",
    x = -t_crit - 0.8,
    y = dt(0, gl) * .18,
    label = expression(alpha / 2),
    colour = "#D73027",
    size = 5
  ) +

  ## Valor crítico
  annotate(
    "text",
    x = 0,
    y = .4,
    label = paste0("t crítico = ±", round(t_crit, 3)),
    size = 4
  ) +

  labs(
    title = "Distribuição t de Student",
    subtitle = sprintf(
      "GL = %d   |   α = %.2f   |   p = %.5f",
      gl,
      alpha,
      p_valor
    ),
    x = "Estatística t",
    y = "Densidade"
  ) +

  scale_x_continuous(
    breaks = scales::pretty_breaks(10),
    expand = expansion(mult = c(0.02, 0.02))
  ) +

  scale_y_continuous(
    expand = expansion(mult = c(0, .05))
  ) +

  # theme_bw(base_size = 13) +
  theme_classic(base_size = 13, base_family = "Latin Modern Roman") +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 11)
  ) +
  canvas(width = 6, height = 5, dpi = 600) -> pt

pt

save_ggplot(plot = pt, file = "curva_t_beta.jpeg")

# distribuição T (teste unicaudal) ---------------------------------------
library(ggplot2)
library(ggview)

#---------------------------
# Parâmetros
#---------------------------
t_obs <- 7.70255878
gl <- 4
alpha <- 0.05

# p-valor (teste unicaudal à direita)
p_valor <- pt(t_obs, df = gl, lower.tail = FALSE)

# valor crítico (unicaudal)
t_crit <- qt(1 - alpha, df = gl)


# eixo x
x_lim <- c(0, 8)
dados <- data.frame(x = seq(x_lim[1], x_lim[2], length.out = 1000))

#---------------------------
# Gráfico
#---------------------------
ggplot(dados, aes(x)) +

  ## Curva t
  # stat_function(
  #   fun = dt,
  #   args = list(df = gl),
  #   linewidth = 1.1,
  #   colour = "#2166AC"
  # ) +
  stat_function(
    fun = dt,
    args = list(df = gl),
    xlim = c(0, x_lim[2]),
    linewidth = 1.1,
    colour = "#2166AC"
  ) +
  ## Região crítica (apenas à direita)
  stat_function(
    geom = "area",
    fun = dt,
    args = list(df = gl),
    xlim = c(t_crit, x_lim[2]),
    fill = "#D73027",
    alpha = .35
  ) +

  ## Valor crítico
  geom_vline(
    xintercept = t_crit,
    linetype = "dashed",
    colour = "grey40"
  ) +

  ## Estatística t observada
  annotate(
    geom = "segment",
    x = t_obs,
    y = 0,
    xend = t_obs,
    yend = 0.05,
    linetype = "dashed",
    colour = "darkgreen",
    linewidth = .5
  ) +

  ## Texto do t observado
  annotate(
    "text",
    x = t_obs + 1,
    y = dt(t_obs, gl) + .06,
    label = paste0("t obs = ", round(t_obs, 3)),
    colour = "darkgreen",
    hjust = 1,
    size = 3.4
  ) +

  ## Texto da região crítica
  annotate(
    "text",
    x = t_crit + 0.8,
    y = dt(0, gl) * .18,
    label = expression(alpha),
    colour = "#D73027",
    size = 5
  ) +

  ## Valor crítico
  annotate(
    "text",
    x = 1,
    y = .4,
    label = paste0("t crítico = ", round(t_crit, 3)),
    size = 4
  ) +

  labs(
    title = "Distribuição t de Student",
    subtitle = sprintf(
      "GL = %d   |   α = %.2f   |   p = %.5f",
      gl,
      alpha,
      p_valor
    ),
    x = "Estatística t",
    y = "Densidade"
  ) +

  scale_x_continuous(
    breaks = scales::pretty_breaks(10),
    expand = expansion(mult = c(0.02, 0.02))
  ) +

  scale_y_continuous(
    expand = expansion(mult = c(0, .05))
  ) +

  theme_bw(base_size = 13) +
  # ggforce::facet_zoom(xlim = c(2, 8), ylim = c(0, 0.055)) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 11)
  ) +
  canvas(width = 6, height = 5, dpi = 600) -> pt

pt

save_ggplot(plot = pt, file = "curva_t_unicaudal.jpeg")


# chi e normal -----------------------------------------------------------
library(ggplot2)

set.seed(123)

n <- 1000000
gl <- 4

z <- rnorm(n, mean = 0, sd = 1)
u <- rchisq(n, df = gl)

t <- z / sqrt(u / gl)

dados <- data.frame(Normal = z, `Qui.quadrado` = u, T = t) |>
  pivot_longer(
    cols = dplyr::everything(),
    names_to = "var",
    values_to = "valor"
  )

ggplot(dados, aes(x = valor, col = var)) +
  facet_wrap(~var, scales = "free_x") +
  # xlim(-10,15) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 80,
    fill = "#5e5e5e",
    color = "#bbbbbb"
  ) +
  stat_function(
    data = dados |> filter(var == "T"),
    fun = dt,
    args = list(df = gl),
    linewidth = 0.6,
    colour = "#D55E00"
  ) +
  stat_function(
    data = dados |> filter(var == "Normal"),
    fun = dnorm,
    # args = list(df = gl),
    linewidth = 0.6,
    colour = "#D55E00"
  ) +
  stat_function(
    data = dados |> filter(var == "Qui.quadrado"),
    fun = dchisq,
    args = list(df = gl),
    linewidth = 0.6,
    colour = "#D55E00"
  ) +
  labs(
    x = "",
    y = "Densidade"
  ) +
  scale_x_continuous(expand = expansion(mult = 0, add = 0)) +
  scale_y_continuous(expand = expansion(mult = 0, add = c(0.005, 0.01))) +
  theme_classic(base_size = 13, base_family = "Latin Modern Roman") +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 11)
  ) +
  canvas(width = 6, height = 5, dpi = 600) -> pt

pt
save_ggplot(plot = pt, file = "curva_t_da_norm_chi.jpeg")
