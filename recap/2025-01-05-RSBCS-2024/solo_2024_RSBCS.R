library(tidyverse)
library(pdftools)
library(ggtext)
library(gt)
library(patchwork)
library(ggwordcloud)

doc <- "/anais-2024.pdf"

texto <- pdftools::pdf_text(pdf = doc)

df <- texto[7:18] |> 
  str_split("(?<=\\d)\\n") |> 
  unlist() |>
  enframe(name = NULL, value = "linha") |> 
  filter(linha != "")

df2 <- df[-seq(1,18),]
df2 <- df2[-seq(321,320),]

base <- df2 |> separate(col = linha, 
                into = c("trabalho","pag"), 
                sep = "\\.{2,}\\s*")

writexl::write_xlsx(base, path = "F:/OneDrive/Área de Trabalho/anais.xlsx")


# dados -------------------------------------------------------------------

rm(list = ls())

df <- readxl::read_excel("F:/OneDrive/Área de Trabalho/anais.xlsx") |> 
  mutate(pag = as.numeric(pag))

area <- df |> filter(str_detect(trabalho, "ÁREA"))

df_filter <- anti_join(df, area, by = "pag")

areas <- 
  data.frame(
    code = paste("Área",seq(1,6)),
    nome = c(
      "BIOLOGIA DO SOLO, CICLOS BIOGEOQUÍMICOS, \nBIOINSUMOS, QUALIDADE E SAÚDE DO SOLO",
      "EDUCAÇÃO, SOCIOLOGIA, PÚBLICA E \nSEGURANÇA ALIMENTAR",
      "MANEJO E CONSERVAÇÃO DO SOLO E DA ÁGUA, \nPLANEJAMENTO DO USO DA TERRA, HIDROLOGIA",
      "MINERALOGIA, GÊNESE, MORFOLOGIA, \nLEVANTAMENTO, CLASSIFICAÇÃO DO SOLO, \nPEDOMETRIA E PALEOPEDOLOGIA",
      "PEDOFUNÇÕES, PROPRIEDADES E \nPROCESSOS FÍSICOS DO SOLO",
      "QUÍMICA, FERTILIDADE, NUTRIÇÃO DE PLANTAS, \n CORRETIVOS E FERTILIZANTES"
      )
    )

base <- df_filter |> 
  mutate(area = case_when(
    pag < 103 ~ "Biologia",
    between(x = pag, left = 103, 122) ~ "Educação",
    between(x = pag, left = 123, 186) ~ "Manejo",
    between(x = pag, left = 187, 204) ~ "Gênese",
    between(x = pag, left = 205, 224) ~ "Física",
    pag > 224 ~ "Química",
    .default = "none"
  ),
  area = str_to_title(area))


# plots -------------------------------------------------------------------
vit_c_palette <- c("Orange Juice" = "#fab909", 
                   "Vitamin C" = "#E93603",
                   light_text = "#323A30",
                   dark_text =  "#0C1509")

monochromeR::view_palette(vit_c_palette)

github_icon <- "&#xf16d"
git_gf <- "frosi.gustavo"
social_caption <- glue::glue(
  "<span style='font-family:\"Font Awesome 6 Brands\";'>{github_icon};</span>
  <span style='color: #000000'>{git_gf}</span><br> Dados: rsbcs2024.com.br"
)

base |>
  group_by(area) |>
  count() |>
  ggplot(aes(x = fct_reorder(area, n), y = n)) +
  geom_col(
    fill = "#3d643b", col = "#317a31",
    width = .4
  ) +
  scale_color_identity() +
  geom_text(mapping=aes(y=0, x=area, label=area),
            color="gray20", vjust=-1.3, hjust=0, fontface = "bold") +
  ggtext::geom_textbox(
    aes(
      label = n,
      hjust = case_when(
        n < 20 ~ 0,
        TRUE ~ 1
      ),
      halign = case_when(
        n < 20 ~ 0,
        TRUE ~ 1
      ),
      colour = case_when(
        n > 50 ~ "#FFFFFF",
        TRUE ~ "#36200d"
      )
    ),
    size = 4,
    fill = NA,
    box.colour = NA,
    family = "Cabin",
    fontface = "bold"
  ) +
  # scale_color_manual(values = c("tomato","green")) +
  coord_flip(expand = F, clip = "off") +
  labs(
    title = ' <span style = "color:#317a31"> Resumos apresentados na</span><span style = "color:#36200d"> XV RSBCS - 2024</span>',
    subtitle = "Número de resumos apresentados por área",
    y = "nº de trabalhos apresentados",
    x = element_blank(),
    caption = social_caption
  ) +
  theme_minimal(14) +
  theme(
    legend.position = "none",
    # text = element_text(colour = vit_c_palette["light_text"]),
    plot.title = ggtext::element_textbox_simple(
      # colour = vit_c_palette["dark_text"],
      size = rel(1.3),
      face = "bold",
      family = "Enriqueta",
      lineheight = 1,
      margin = margin(0.5, 7, 1, 0, "lines"),
      hjust = 0, halign = 0
    ),
    plot.subtitle = ggtext::element_textbox_simple(
      family = "Cabin", size = rel(1), 
      lineheight = 1,
      margin = margin(0, 0, 1.5, 0, "lines")),
    plot.caption = ggtext::element_textbox_simple(
      size = rel(.5)),
    panel.grid.major = element_blank(),
    axis.title.x = element_blank(),
    axis.text= element_blank(),
    axis.ticks = element_blank()
  ) -> p1

ggsave(plot = p1,filename = "ts.jpeg", width = 4, height = 4, dpi = 600)


# tabela ------------------------------------------------------------------

t1 <- areas |> 
  gt() |> 
  tab_header(
    title = md("**Divisão das áreas de pesquisa na XV RSBCS**"),
    subtitle = md("Reunião Sul Brasileira de Ciência do Solo")
  ) |> 
  cols_label(
    code = "Código",
    nome = "Área"
  ) |> 
  tab_source_note(
    source_note = md("Source: www.rsbcs2024.com.br")
  )

t1


library(wordcloud2)
library(stopwords)
library(tidytext)

to_remove <- c(stopwords(language = 'en'))
to_remove_pt <- c(stopwords(language = 'pt'))



chat1 <- base |>
  # filter(area == "Área 1") |>
  unnest_tokens(input = trabalho,output = word)  |>
  filter(!word %in% to_remove) |>
  filter(!word %in% to_remove_pt) |>
  group_by(word) |> count() |>
  mutate(word = as.factor(word)) |> 
  filter(n >= 4)

chat2 <- base |>
  filter(area == "Área 2") |>
  unnest_tokens(input = trabalho,output = word)  |>
  filter(!word %in% to_remove) |>
  filter(!word %in% to_remove_pt) |>
  group_by(word) |> count() |>
  mutate(word = as.factor(word)) |> 
  filter(n >= 2)

chat3 <- base |>
  filter(area == "Área 3") |>
  unnest_tokens(input = trabalho,output = word)  |>
  filter(!word %in% to_remove) |>
  filter(!word %in% to_remove_pt) |>
  group_by(word) |> count() |>
  mutate(word = as.factor(word)) |> 
  filter(n >= 2)

chat4 <- base |>
  filter(area == "Área 4") |>
  unnest_tokens(input = trabalho,output = word)  |>
  filter(!word %in% to_remove) |>
  filter(!word %in% to_remove_pt) |>
  group_by(word) |> count() |>
  mutate(word = as.factor(word)) |> 
  filter(n >= 2)

chat5 <- base |>
  filter(area == "Área 5") |>
  unnest_tokens(input = trabalho,output = word)  |>
  filter(!word %in% to_remove) |>
  filter(!word %in% to_remove_pt) |>
  group_by(word) |> count() |>
  mutate(word = as.factor(word)) |> 
  filter(n >= 2)

chat6 <- base |>
  filter(area == "Área 6") |>
  unnest_tokens(input = trabalho,output = word)  |>
  filter(!word %in% to_remove) |>
  filter(!word %in% to_remove_pt) |>
  group_by(word) |> count() |>
  mutate(word = as.factor(word)) |> 
  filter(n >= 2)


X11()


p1 <- ggplot(chat1, aes(
  label = word,
  size = n,
  color = n
)) +
  geom_text_wordcloud_area(eccentricity = 1,
    # aes(angle = 35 * sample(-2:2, nrow(chat1),
    #                         replace = TRUE,
    #                         prob = c(1, 1, 4, 1, 1)
    # )),
    mask = png::readPNG(source = "F:/OneDrive/Área de Trabalho/png-transparent-erlenmeyer-flask-erlenmeyer-flask-empty-laboratory-science-glass-chemistry-chemical-glassware-thumbnail2.png"),
    rm_outside = TRUE
  ) +
  scale_size_area(max_size = 21) +
  # scale_radius(range = c(0, 30), limits = c(0, NA)) +
  ggtitle('<span style = "color:#317a31"> Palavras mais frequentes de Títulos</span>',
          subtitle = '<span style = "color:#36200d"> Resumos da XV RSBCS - 2024</span>'
  ) +
  scale_color_gradient2(mid = "#36200d",low = "#0f5f2c", high = "#68ee68") +
  theme_minimal() +
  theme( plot.title = ggtext::element_textbox_simple(
    # colour = vit_c_palette["dark_text"],
    size = rel(1.2),
    face = "bold",
    family = "Enriqueta",
    lineheight = 1,
    margin = margin(0.5, 0, 0, 0, "lines"),
    hjust = 0, halign = 0
  ),
  plot.subtitle = ggtext::element_textbox_simple(
    family = "Cabin", size = rel(0.8), 
    lineheight = 1,
    margin = margin(0, 0, 1, 0, "lines"))
  )
p1

ggsave(filename = "palavras_gerais.jpeg", 
       plot = p1, dpi = 600,
       height = 4.7, width = 4.5)


p2 <- ggplot(chat2, aes(
  label = word,
  size = n,
  color = n
)) +
  geom_text_wordcloud_area(
    aes(angle = 35 * sample(-2:2, nrow(chat2),
                            replace = TRUE,
                            prob = c(1, 1, 4, 1, 1)
    )),
    mask = png::readPNG(source = "F:/OneDrive/Área de Trabalho/png-transparent-erlenmeyer-flask-erlenmeyer-flask-empty-laboratory-science-glass-chemistry-chemical-glassware-thumbnail.png"),
    rm_outside = TRUE
  ) +
  scale_size_area(max_size = 23) +
  ggtitle("Àrea 2") +
  scale_color_gradient(low = "#0f5f2c", high = "#265626") +
  theme_minimal()

p3 <- ggplot(chat3, aes(
  label = word,
  size = n,
  color = n
)) +
  geom_text_wordcloud_area(
    aes(angle = 35 * sample(-2:2, nrow(chat3),
                            replace = TRUE,
                            prob = c(1, 1, 4, 1, 1)
    )),
    mask = png::readPNG(source = "F:/OneDrive/Área de Trabalho/png-transparent-erlenmeyer-flask-erlenmeyer-flask-empty-laboratory-science-glass-chemistry-chemical-glassware-thumbnail.png"),
    rm_outside = TRUE
  ) +
  scale_size_area(max_size = 23) +
  ggtitle("Àrea 3") +
  scale_color_gradient(low = "#0f5f2c", high = "#265626") +
  theme_minimal()

p4 <- ggplot(chat4, aes(
  label = word,
  size = n,
  color = n
)) +
  geom_text_wordcloud_area(
    aes(angle = 35 * sample(-2:2, nrow(chat4),
                            replace = TRUE,
                            prob = c(1, 1, 4, 1, 1)
    )),
    mask = png::readPNG(source = "F:/OneDrive/Área de Trabalho/png-transparent-erlenmeyer-flask-erlenmeyer-flask-empty-laboratory-science-glass-chemistry-chemical-glassware-thumbnail.png"),
    rm_outside = TRUE
  ) +
  scale_size_area(max_size = 23) +
  ggtitle("Àrea 4") +
  scale_color_gradient(low = "#0f5f2c", high = "#265626") +
  theme_minimal()

p5 <- ggplot(chat5, aes(
  label = word,
  size = n,
  color = n
)) +
  geom_text_wordcloud_area(
    aes(angle = 35 * sample(-2:2, nrow(chat5),
                            replace = TRUE,
                            prob = c(1, 1, 4, 1, 1)
    )),
    mask = png::readPNG(source = "F:/OneDrive/Área de Trabalho/png-transparent-erlenmeyer-flask-erlenmeyer-flask-empty-laboratory-science-glass-chemistry-chemical-glassware-thumbnail.png"),
    rm_outside = TRUE
  ) +
  scale_size_area(max_size = 23) +
  ggtitle("Àrea 5") +
  scale_color_gradient(low = "#0f5f2c", high = "#265626") +
  theme_minimal()

p6 <- ggplot(chat6, aes(
  label = word,
  size = n,
  color = n
)) +
  geom_text_wordcloud_area(
    aes(angle = 35 * sample(-2:2, nrow(chat6),
                            replace = TRUE,
                            prob = c(1, 1, 4, 1, 1)
    )),
    mask = png::readPNG(source = "F:/OneDrive/Área de Trabalho/png-transparent-erlenmeyer-flask-erlenmeyer-flask-empty-laboratory-science-glass-chemistry-chemical-glassware-thumbnail.png"),
    rm_outside = TRUE
  ) +
  scale_size_area(max_size = 15) +
  ggtitle("Àrea 6") +
  scale_color_gradient(low = "#0f5f2c", high = "#265626") +
  theme_minimal()


plot <- (p1|p2|p3) / (p4|p5|p6)

ggsave(plot = plot, filename = "palavra.jpeg")
