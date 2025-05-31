
# pacotes ---------------------------------------------------------------------------
library(tidyverse)
library(pdftools)
library(rvest)
library(tm)
library(xml2)
library(httr)
library(showtext)
library(ggtext)


# data ------------------------------------------------------------------------------


primeira_parte <- "https://solosfloripa2023.com.br/evento/solos2023/trabalhosaprovados?titulo=&autor=&t1area_id="

id <- c(1,2,3,8,4,5,6,7,9,10,11,12,13,14,15,16)

segunda_parte <- "&modeloformaapresentacaofinal_id=&btBuscar=Buscar"


df <- as.data.frame(x = 1:length(id))
for (i in 1:16) {
  dados <- httr::GET(paste0(primeira_parte,id[i],segunda_parte))
  get_content <- httr::content(dados)

  raw_table <- get_content |>
    rvest::html_nodes(".container-fluid")

  df[i,] <-
    raw_table[[3]] |>
    rvest::html_text() |>
    strsplit("\n") |>
    unlist() |> 
    enframe(name = NULL, value = 'linha') |>  
    dplyr::filter(stringr::str_detect(linha, pattern = ".*registro")) 

}


divi <- raw_table[[3]] |>
  rvest::html_text() |>
  strsplit("\n") |>
  unlist() |>
  enframe(name = NULL, value = 'linha') |>
  dplyr::filter(stringr::str_detect(linha, pattern = " TODASDivisã")) |>
  str_split(pattern = "Di", simplify = T) |> as.data.frame() |>
  pivot_longer(cols = 2:17, names_to = "Divi", values_to = "nome") |>
  filter(Divi != "V1") |>
  mutate(di = "Di") |>
  transmute(Divi = paste(di,nome, sep = ""))


d_final <- cbind(divi, df)

dados.ok <- d_final |> mutate(partici = str_extract(`1:length(id)`,
                                                    pattern = "[:digit:]{1,}"),
                              partici = as.numeric(partici)
                              ) |>
  select(3)


comi <-  str_split(d_final$Divi, ":")
data_separado <- data.frame(do.call(rbind, comi))

df_ok <- cbind(data_separado,dados.ok)

df_ok <- df_ok |> mutate(remover = str_sub(df_ok$X2, start = 16, end = 90))

# plot --------------------------------------------------------------------
# didot

font_add("Didot", "recap/2023-12-17 CBCS 2023/GFS_Didot/GFSDidot-Regular.ttf")

showtext_auto()

d1 <- df_ok |> filter(X1 == "Divisão 1 – Solo no espaço e no tempo") |>
ggplot(aes(y = partici, x = fct_reorder(remover, partici))) +
  coord_flip() +
  geom_col(fill = "#ef8118", col = "#bc6a1d") +
  # facet_wrap(~X1, scales = "free") +
  labs(x = NULL, y = "Nº de trabalhos",
       title = 'Resumos no <span style = "color:#ef8118"> XXIII CLACS | XXXVIII CBCS </span>',
       subtitle = "Divisão 1 – Solo no espaço e no tempo",
       caption = "Fonte: https://solosfloripa2023.com.br/solos2023") +
  geom_text(aes(label = partici), nudge_y = -2, col = "black", size = 5) +
  theme_minimal(18) +
  theme(text = element_text(family = "Didot"),
        plot.title  = ggtext::element_markdown())

ggsave(plot = d1, filename = "d1.jpg", width = 10, height = 6, dpi = 600)

df_ok |> filter(X1 == "Divisão 2 – Processos e Propriedades do Solo") |>
  ggplot(aes(y = partici, x = fct_reorder(remover, partici))) +
  coord_flip() +
  geom_col(fill = "#ef8118", col = "#bc6a1d") +
  # facet_wrap(~X1, scales = "free") +
  labs(x = NULL, y = "Nº de trabalhos",
       title = 'Resumos no <span style = "color:#ef8118"> XXIII CLACS | XXXVIII CBCS </span>',
       subtitle = "Divisão 2 – Processos e Propriedades do Solo",
       caption = "Fonte: https://solosfloripa2023.com.br/solos2023") +
  geom_text(aes(label = partici), nudge_y = -4, col = "black", size = 6) +
  theme_minimal(18) +
  theme(text = element_text(family = "Didot"),
        plot.title  = element_markdown(size = 20, lineheight = 1.3))

df_ok |> filter(X1 == "Divisão 3 – Uso e Manejo do Solo") |>
  mutate(remover = factor(remover, labels = c("Fertilidade do Solo e \n Nutrição de Plantas",
                                "Corretivos e Fertilizantes",
                                "Manejo e Conservação \n do Solo e da Água",
                                "Planejamento do Uso da Terra",
                                "Poluição, Remediação do Solo e \n Recuperação de Áreas Degradadas"))) |>
  ggplot(aes(y = partici, x = fct_reorder(remover, partici))) +
  coord_flip() +
  geom_col(fill = "#ef8118", col = "#bc6a1d") +
  # facet_wrap(~X1, scales = "free") +
  labs(x = NULL, y = "Nº de trabalhos",
       title = 'Resumos no <span style = "color:#ef8118"> XXIII CLACS | XXXVIII CBCS </span>',
       subtitle = "Divisão 3 – Uso e Manejo do Solo",
       caption = "Fonte: https://solosfloripa2023.com.br/solos2023") +
  geom_text(aes(label = partici), nudge_y = -13, col = "black", size = 6) +
  theme_minimal(18) +
  theme(text = element_text(family = "Didot"),
        plot.title  = element_markdown(size = 20, lineheight = 1.3))

df_ok |> filter(X1 == "Divisão 4 - Solo, Ambiente e Sociedade") |>
  mutate(remover = factor(remover, labels = c("Educação em Solos e \n Percepção Pública do Solo",
                                              "História, Epistemologia e \n Sociologia da Ciência",
                                              "Solos e Segurança Alimentar"))) |>
  ggplot(aes(y = partici, x = fct_reorder(remover, partici))) +
  coord_flip() +
  geom_col(fill = "#ef8118", col = "#bc6a1d") +
  # facet_wrap(~X1, scales = "free") +
  labs(x = NULL, y = "Nº de trabalhos",
       title = 'Resumos no <span style = "color:#ef8118"> XXIII CLACS | XXXVIII CBCS </span>',
       subtitle = "Divisão 4 - Solo, Ambiente e Sociedade",
       caption = "Fonte: https://solosfloripa2023.com.br/solos2023") +
  geom_text(aes(label = partici), nudge_y = -2, col = "black", size = 6) +
  theme_minimal(18) +
  theme(text = element_text(family = "Didot"),
        plot.title  = element_markdown(size = 20, lineheight = 1.3))


df_ok |> group_by(X1) |>
  summarise(soma = sum(partici)) |>
  mutate(X1 = factor(X1, labels = c("Divisão 1 – Solo no \n espaço e no tempo",
                                              "Divisão 2 – Processos e \n  Propriedades do Solo",
                                              "Divisão 3 – Uso e \n Manejo do Solo",
                                              "Divisão 4 - Solo, \n Ambiente e Sociedade")),
         total = sum(soma)) |>
  ggplot(aes(y = soma, x = fct_reorder(X1, soma))) +
  coord_flip() +
  geom_col(fill = "#ef8118", col = "#bc6a1d") +
  # facet_wrap(~X1, scales = "free") +
  labs(x = NULL, y = "Nº de trabalhos",
       title = 'Resumos no <span style = "color:#ef8118"> XXIII CLACS | XXXVIII CBCS </span>',
       subtitle = "Total por divisão",
       caption = "Fonte: https://solosfloripa2023.com.br/solos2023") +
  geom_text(aes(label = soma), nudge_y = -32, col = "black", size = 6) +
  theme_minimal(18) +
  theme(text = element_text(family = "Didot"),
        plot.title  = element_markdown(size = 20, lineheight = 1.3))
