library(ggfx)
library(showtext)
library(ggtext)
library(shadowtext)
library(tidyverse)

set.seed(123)  # Para reprodutibilidade

# font_add_google("Fraunces", "fraunces")
# showtext::showtext_auto()

n <- 500
x <- rnorm(n, mean = 50, sd = 10)
y <- 0.9 * x + rnorm(n, mean = 0, sd = 20)  # Relação linear mais forte com menos ruído
p1 <- data.frame(
  x = x,
  y = y,
  size = runif(n, min = 1, max = 10),
  group = sample(1:5, n, replace = TRUE)
) |> 
ggplot(aes(x = x, y = y)) +
  with_outer_glow(geom_point(aes(size = size, color = factor(group)), alpha = 0.6), colour = "gray60", sigma = 10) +
  geom_smooth(method = "lm", col = "black", fill = "gray50", linewidth = 2) +
  # with_shadow(geom_richtext(
  #   mapping = aes(
  #     label = "Linear Regression",
  #     x = 50, y = 110, fontface = "bold"
  #   ), size = 13,
  #   fill = NA,
  #   label.color = NA,
  #   family = "fraunces",
  #   label.padding = unit(c(0.30, 0.30, 0.30, 0.30), "lines"),
  # ),sigma = 20, colour = "gray30") +
  # geom_shadowtext(mapping = aes(x = 50, y = 110,
  #                               label = "Linear Regression"), family = "fraunces",
  #                 size = 13, colour = "black", bg.colour = "white",
  #                 angle = 0,
  #                 fontface = "bold") +
  theme_minimal(20) +
  scale_size_continuous(range = c(1, 10)) +
  scale_color_manual(values = c("#B24745", "#D8C3A5", "#E98A15", "#606D72", "#9FC490")) +
  theme(panel.grid.major = element_line(color = "gray85"),
        panel.grid.minor = element_line(color = "gray90"),
        plot.background = element_rect(fill = "ivory"),
        panel.background = element_rect(fill = "ivory"),
        axis.line = element_line(color = "gray50"),
        legend.position = "none",
        text = element_text(family = "fraunces")) +
  labs(x = NULL, y = NULL, color = "Group")
p1
ggsave(plot = p1, filename = "ima_fundo.jpeg")
