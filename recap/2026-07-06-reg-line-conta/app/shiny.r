library(shiny)
library(bslib)
library(ggplot2)

dados <- data.frame(
  x = c(1, 2, 3, 4, 5, 6, 7, 8),
  y = c(1, 3, 5, 4, 7, 6, 8, 8)
)

ols <- lm(y ~ x, data = dados)
ols_b0 <- coef(ols)[1]
ols_b1 <- coef(ols)[2]
ols_resid <- sum(resid(ols)^2)
sst <- sum((dados$y - mean(dados$y))^2) # SQT — fixo

tema <- bs_theme(
  version = 5,
  bootswatch = "darkly",
  bg = "#0f1117",
  fg = "#e8eaf0",
  primary = "#f5c518",
  secondary = "#2ecc71",
  base_font = font_google("IBM Plex Mono"),
  heading_font = font_google("Syne"),
  font_scale = 0.92
)

ui <- page_fillable(
  theme = tema,

  tags$head(tags$style(HTML(
    "
    body { background-color: #0f1117 !important; }
    .card { background-color: #181b24 !important; border: 1px solid #2a2d3a !important; border-radius: 12px !important; }
    .card-header { background-color: #1e2130 !important; border-bottom: 1px solid #2a2d3a !important; font-family: 'Syne', sans-serif; letter-spacing: .04em; }
    .value-box { border-radius: 12px !important; }
    .irs--shiny .irs-bar { background: #f5c518 !important; border-top-color: #f5c518 !important; border-bottom-color: #f5c518 !important; }
    .irs--shiny .irs-handle { background: #f5c518 !important; border-color: #f5c518 !important; }
    .irs--shiny .irs-single { background: #f5c518 !important; color: #0f1117 !important; font-weight: 700; }
    .btn-primary { background-color: #f5c518 !important; border-color: #f5c518 !important; color: #0f1117 !important; font-weight: 700; border-radius: 8px !important; }
    .btn-primary:hover { background-color: #d4a80e !important; }
    table.dataTable, .table { background-color: #181b24 !important; color: #e8eaf0 !important; }
    .table-striped > tbody > tr:nth-of-type(odd) > * { background-color: #1e2130 !important; color: #e8eaf0 !important; }
    hr { border-color: #2a2d3a; }
    .section-label { font-family: 'Syne', sans-serif; font-size: .75rem; letter-spacing: .12em; text-transform: uppercase; color: #6c7293; margin-bottom: .5rem; }
  "
  ))),

  layout_columns(
    col_widths = c(3, 9),
    fill = TRUE,

    # ── painel esquerdo ──────────────────────────────────────────────────────
    card(
      height = "100%",
      card_header("Controles"),
      card_body(
        gap = "1rem",

        tags$p(class = "section-label", "Parâmetros da reta"),

        sliderInput(
          "beta0",
          "β₀  —  intercepto",
          min = -3,
          max = 5,
          value = 1.11,
          step = 0.01,
          width = "100%"
        ),
        sliderInput(
          "beta1",
          "β₁  —  inclinação",
          min = -2,
          max = 3,
          value = 0.98,
          step = 0.01,
          width = "100%"
        ),

        tags$hr(),
        tags$p(class = "section-label", "Equação atual"),
        uiOutput("equacao_display"),

        tags$hr(),
        tags$p(class = "section-label", "Referência OLS"),
        uiOutput("ols_display"),

        tags$hr()
      )
    ),

    # ── painel direito ───────────────────────────────────────────────────────
    layout_columns(
      col_widths = 12,
      row_heights = c(1, 4),

      # value boxes ────────────────────────────────────────────────────────────
      layout_columns(
        col_widths = c(3, 3, 3, 3),

        value_box(
          title = "ΣRes² atual",
          value = uiOutput("vbox_sq"),
          showcase = bsicons::bs_icon("calculator"),
          theme = "dark",
          style = "background-color:#1e2130; border:1px solid #2a2d3a;"
        ),
        value_box(
          title = "ΣRes² mínimo (OLS)",
          value = textOutput("vbox_ols"),
          showcase = bsicons::bs_icon("graph-up-arrow"),
          theme = "dark",
          style = "background-color:#1e2130; border:1px solid #2ecc71;"
        ),
        value_box(
          title = "% acima do mínimo",
          value = uiOutput("vbox_pct"),
          showcase = bsicons::bs_icon("percent"),
          theme = "dark",
          style = "background-color:#1e2130; border:1px solid #2a2d3a;"
        ),
        value_box(
          title = "R²",
          value = uiOutput("vbox_r2"),
          showcase = bsicons::bs_icon("bar-chart-fill"),
          theme = "dark",
          style = "background-color:#1e2130; border:1px solid #7eb8f7;"
        )
      ),

      # gráfico ────────────────────────────────────────────────────────────────
      card(
        full_screen = TRUE,
        card_header("Visualização"),
        card_body(
          padding = "0.5rem",
          plotOutput("grafico", height = "420px")
        )
      )
    )
  )
)

# ── server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  dados_r <- reactive({
    d <- dados
    d$y_hat <- input$beta0 + input$beta1 * d$x
    d$residuo <- d$y - d$y_hat
    d$res2 <- d$residuo^2
    d
  })

  sq_atual <- reactive({
    sum(dados_r()$res2)
  })
  pct <- reactive({
    sq_atual() / ols_resid
  })
  r2 <- reactive({
    1 - sq_atual() / sst
  })

  cor_hex <- reactive({
    if (pct() < 1.05) {
      "#2ecc71"
    } else if (pct() < 1.5) {
      "#e67e22"
    } else {
      "#e74c3c"
    }
  })
  r2_cor <- reactive({
    r <- r2()
    if (r >= 0.9) {
      "#2ecc71"
    } else if (r >= 0.6) {
      "#e67e22"
    } else {
      "#e74c3c"
    }
  })

  # value boxes
  output$vbox_sq <- renderUI({
    tags$span(
      style = paste0("color:", cor_hex(), "; font-weight:700"),
      sprintf("%.2f", sq_atual())
    )
  })
  output$vbox_ols <- renderText({
    sprintf("%.2f", ols_resid)
  })
  output$vbox_pct <- renderUI({
    tags$span(
      style = paste0("color:", cor_hex(), "; font-weight:700"),
      sprintf("%.1f%%", (pct() - 1) * 100)
    )
  })
  output$vbox_r2 <- renderUI({
    tags$span(
      style = paste0("color:", r2_cor(), "; font-weight:700"),
      sprintf("%.4f", r2())
    )
  })

  # sidebar displays
  output$equacao_display <- renderUI({
    b0 <- input$beta0
    b1 <- input$beta1
    tags$code(
      style = "color:#f5c518; font-size:1.05rem",
      sprintf("ŷ = %.2f %s %.2f · x", b0, ifelse(b1 >= 0, "+", ""), b1)
    )
  })

  output$ols_display <- renderUI({
    tagList(
      tags$code(
        style = "color:#2ecc71; font-size:.9rem",
        sprintf("β₀ = %.2f  |  β₁ = %.2f", ols_b0, ols_b1)
      )
    )
  })

  # gráfico
  output$grafico <- renderPlot(
    {
      d <- dados_r()

      quadrados <- do.call(
        rbind,
        lapply(1:nrow(d), function(i) {
          xi <- d$x[i]
          yi <- d$y[i]
          yhat <- d$y_hat[i]
          r <- d$residuo[i]
          lado <- abs(r)
          y_bottom <- min(yi, yhat)
          y_top <- max(yi, yhat)
          x_far <- if (r <= 0) xi + lado else xi - lado
          data.frame(
            x = c(xi, x_far, x_far, xi),
            y = c(y_bottom, y_bottom, y_top, y_top),
            id = i
          )
        })
      )

      segmentos <- data.frame(x = d$x, y = d$y, xend = d$x, yend = d$y_hat)
      titulo_cor <- cor_hex()

      ggplot(d, aes(x, y)) +
        geom_polygon(
          data = quadrados,
          aes(x = x, y = y, group = id),
          fill = "tomato",
          alpha = 0.22,
          color = "tomato",
          linewidth = 0.5
        ) +
        geom_segment(
          data = segmentos,
          aes(x = x, y = y, xend = xend, yend = yend),
          color = "tomato",
          linewidth = 0.9,
          linetype = "dashed",
          lineend = "round"
        ) +
        geom_abline(
          intercept = input$beta0,
          slope = input$beta1,
          color = "#f5c518",
          linewidth = 1.4
        ) +
        geom_point(size = 3.5, color = "#7eb8f7") +
        geom_text(
          aes(
            x = x + 0.15,
            y = (y + y_hat) / 2,
            label = sprintf("e=%.1f", residuo)
          ),
          size = 3,
          color = "tomato",
          hjust = 0
        ) +
        coord_cartesian(clip = "off") +
        theme_minimal(base_size = 13) +
        theme(
          plot.background = element_rect(fill = "#181b24", color = NA),
          panel.background = element_rect(fill = "#181b24", color = NA),
          panel.grid.major = element_line(color = "#2a2d3a"),
          panel.grid.minor = element_line(color = "#1e2130"),
          text = element_text(color = "#e8eaf0", family = "IBM Plex Mono"),
          plot.subtitle = element_text(color = titulo_cor, face = "bold"),
          axis.text = element_text(color = "#6c7293"),
          aspect.ratio = 1
        ) +
        labs(
          title = NULL,
          subtitle = sprintf(
            "ŷ = %.2f + %.2f·x  |  ΣRes² = %.2f  |  R² = %.4f  (%.1f%% acima do mínimo OLS)",
            input$beta0,
            input$beta1,
            sq_atual(),
            r2(),
            (pct() - 1) * 100
          ),
          x = "X",
          y = "Y"
        )
    },
    bg = "#181b24"
  )
}

shinyApp(ui, server)