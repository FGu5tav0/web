library(ggdag)
library(ggplot2)
library(dagitty)
library(patchwork)
library(dplyr)


# com confundidor --------------------------------------------------------

dag <- dagitty(
  "dag {
  Y <- X
  Y <- Z -> X
  X [exposure]
  Y [outcome]
  }"
)

td <- tidy_dagitty(dag, layout = "nicely")

# deixar apenas uma aresta pontilhada
td <- td %>%
  mutate(
    tipo = case_when(
      name == "X" & to == "Y" ~ "dashed",
      TRUE ~ "solid"
    )
  )

p1 <- ggplot(
  td,
  aes(
    x = x,
    y = y,
    xend = xend,
    yend = yend
  )
) +
  ggtitle("Confundidor") +

  geom_dag_edges(
    aes(edge_linetype = tipo),
    edge_width = 1.2
  ) +

  geom_dag_node() +

  geom_dag_text() +

  scale_linetype_identity() +

  theme_dag() +
  theme(plot.title = element_text(hjust = .5))


# p1

# sem confundidor --------------------------------------------------------

dag <- dagitty(
  "dag {
  X -> Y
  X [exposure]
  Y [outcome]
  }"
)

td <- tidy_dagitty(dag, layout = "nicely")


p2 <- ggplot(
  td,
  aes(
    x = x,
    y = y,
    xend = xend,
    yend = yend
  )
) +
  ggtitle("Relação direta") +
  geom_dag_edges(
    # aes(edge_linetype = tipo),
    edge_width = 1.2
  ) +

  geom_dag_node() +

  geom_dag_text() +

  # scale_linetype_identity() +

  theme_dag() +
  theme(plot.title = element_text(hjust = .5))

# p2

# com mediação -----------------------------------------------------------

dag <- dagitty(
  "dag {
  Y <- X
  X -> Z -> Y
  X [exposure]
  Y [outcome]
  }"
)

td <- tidy_dagitty(dag, layout = "nicely")


p3 <- ggplot(
  td,
  aes(
    x = x,
    y = y,
    xend = xend,
    yend = yend
  )
) +
  ggtitle("Mediação") +
  geom_dag_edges(
    # aes(edge_linetype = tipo),
    edge_width = 1.2
  ) +

  geom_dag_node() +

  geom_dag_text() +

  # scale_linetype_identity() +

  theme_dag() +
  theme(plot.title = element_text(hjust = .5))


# p3

# moderação --------------------------------------------------------------

dag <- dagitty(
  "dag {
  X -> Z -> Y
  X [exposure]
  Y [outcome]
  }"
)

td <- tidy_dagitty(dag, layout = "nicely")


p4 <- ggplot(
  td,
  aes(
    x = x,
    y = y,
    xend = xend,
    yend = yend
  )
) +
  ggtitle("Moderação") +
  geom_dag_edges(
    # aes(edge_linetype = tipo),
    edge_width = 1.2
  ) +

  geom_dag_node() +

  geom_dag_text() +

  # scale_linetype_identity() +

  theme_dag() +
  theme(plot.title = element_text(hjust = .5))

# p4

pt <- (p1 | p2) /
  (p3 | p4) +
  # plot_annotation(theme = theme(plot.background = element_rect(colour = "gray95"))) +
  ggview::canvas(width = 9, height = 9, dpi = 500)


# pt

ggview::save_ggplot(plot = pt, file = "plot_causa.jpeg")
