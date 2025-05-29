library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(DescTools)
library(shinythemes)
library(DT)
library(psych)

ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel(div(h2("📊 Análisis Estadístico Profesional", style = "color:#2c3e50; font-weight:bold"))),
  
  tabsetPanel(
    tabPanel("📁 Cargar Datos",
             fluidRow(
               column(4,
                      wellPanel(
                        fileInput("archivo", "📂 Selecciona un archivo (.csv o .xlsx):", accept = c(".csv", ".xlsx")),
                        uiOutput("varSeleccion"),
                        checkboxInput("estandarizar", "⚙️ Estandarizar datos", value = FALSE),
                        actionButton("analizar", "📈 Analizar", class = "btn btn-primary btn-block")
                      )
               ),
               column(8,
                      h4("🔍 Vista previa de los datos"),
                      DTOutput("vistaPrevia")
               )
             )
    ),
    
    tabPanel("📊 Estadísticas",
             fluidRow(
               column(6,
                      h4("📌 Medidas Descriptivas"),
                      DTOutput("estadisticas")
               ),
               column(6,
                      h4("📉 Histograma"),
                      plotOutput("grafico")
               )
             ),
             hr(),
             h4("📈 Diagrama de Cajas"),
             plotOutput("boxplot")
    ),
    
    tabPanel("🔬 Análisis",
             fluidRow(
               column(6,
                      h4("🧠 Prueba sugerida"),
                      verbatimTextOutput("sugerencia")
               ),
               column(6,
                      h4("📑 Resultado"),
                      verbatimTextOutput("resultado")
               )
             ),
             hr(),
             h4("🗣️ Interpretación del resultado"),
             textOutput("interpretacion")
    )
  )
)

server <- function(input, output) {
  datos <- reactive({
    req(input$archivo)
    ext <- tools::file_ext(input$archivo$name)
    if (ext == "csv") {
      read.csv(input$archivo$datapath)
    } else if (ext == "xlsx") {
      read_excel(input$archivo$datapath)
    } else {
      validate("Formato no soportado. Usa .csv o .xlsx")
    }
  })
  
  datos_filtrados <- reactive({
    req(input$vars)
    df <- datos()[, input$vars, drop = FALSE]
    if (input$estandarizar) {
      df <- as.data.frame(scale(df))
    }
    df
  })
  
  output$vistaPrevia <- renderDT({
    datatable(head(datos(), 10), options = list(scrollX = TRUE))
  })
  
  output$varSeleccion <- renderUI({
    req(datos())
    selectInput("vars", "🎯 Selecciona variables numéricas (2 o 3):", 
                choices = names(datos()), multiple = TRUE)
  })
  
  output$sugerencia <- renderText({
    req(input$vars)
    n <- nrow(datos())
    v <- input$vars
    
    if (length(v) == 2) {
      if (n <= 35) {
        return("Sugerencia: aplicar prueba t para comparar dos grupos.")
      } else {
        return("Sugerencia: aplicar ANOVA o regresión lineal.")
      }
    } else if (length(v) == 3) {
      return("Sugerencia: aplicar ANOVA factorial para evaluar efectos múltiples.")
    } else {
      return("Selecciona 2 o 3 variables para realizar un análisis.")
    }
  })
  
  output$estadisticas <- renderDT({
    req(input$vars)
    dat <- datos_filtrados()
    if (!all(sapply(dat, is.numeric))) return(NULL)
    
    stats <- psych::describe(dat)[, c("mean", "median", "sd", "min", "max", "range", "skew", "kurtosis")]
    datatable(round(stats, 3), options = list(pageLength = 5))
  })
  
  output$grafico <- renderPlot({
    req(input$vars)
    dat <- datos_filtrados()
    ggplot(dat, aes_string(x = input$vars[1])) +
      geom_histogram(bins = 15, fill = "#3498db", color = "white") +
      theme_minimal() +
      labs(title = paste("Histograma de", input$vars[1]),
           x = input$vars[1], y = "Frecuencia")
  })
  
  output$boxplot <- renderPlot({
    req(input$vars)
    dat <- datos_filtrados()
    ggplot(stack(dat), aes(x = ind, y = values, fill = ind)) +
      geom_boxplot() +
      theme_minimal() +
      labs(title = "Diagrama de cajas por variable", x = "Variable", y = "Valor") +
      theme(legend.position = "none")
  })
  
  resultado_analisis <- reactive({
    req(input$vars)
    dat <- datos()
    v <- input$vars
    
    if (!all(sapply(dat[, v], is.numeric))) return(NULL)
    
    if (length(v) == 2 && nrow(dat) <= 35) {
      return(t.test(dat[[v[1]]], dat[[v[2]]]))
    } else if (length(v) == 3) {
      formula <- as.formula(paste(v[1], "~", v[2], "+", v[3]))
      return(summary(aov(formula, data = dat)))
    } else {
      return(NULL)
    }
  })
  
  output$resultado <- renderPrint({
    res <- resultado_analisis()
    if (is.null(res)) return("No se pudo realizar el análisis con las variables seleccionadas.")
    print(res)
  })
  
  output$interpretacion <- renderText({
    res <- resultado_analisis()
    if (is.null(res)) return("No se aplicó ninguna prueba estadística.")
    
    if (inherits(res, "htest")) {
      p <- res$p.value
      if (p < 0.05) {
        return(paste("El valor p =", round(p, 4), "es menor que 0.05. Se rechaza la hipótesis nula: hay diferencias significativas."))
      } else {
        return(paste("El valor p =", round(p, 4), "es mayor o igual a 0.05. No se rechaza la hipótesis nula: no hay diferencias significativas."))
      }
    }
    
    if (inherits(res, "anova")) {
      p <- res[[1]][["Pr(>F)"]][1]
      if (p < 0.05) {
        return(paste("El valor p =", round(p, 4), "indica diferencias significativas entre los grupos. Se rechaza la hipótesis nula."))
      } else {
        return(paste("El valor p =", round(p, 4), "indica que no hay diferencias significativas entre los grupos."))
      }
    }
    
    return("Resultado no interpretable.")
  })
}

shinyApp(ui, server)

