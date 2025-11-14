library(shiny)
library(tidyverse)
library(DT) # Tablas interactivas
require(readxl)
require(writexl)
require(plotly)
library(bslib)  # Para el tema visual de la app

# Interfaz (UI)

navbarPage(
  theme = bs_theme(version = 5, bootswatch = "journal"),
  title = "R DAIRY TOOLS",
  
  # --- Pestaña 1: Datos (con sidebar) -------
  tabPanel("Visualización",
           sidebarLayout(
             # --- Panel lateral ---
             sidebarPanel(
               fluidRow(
                 column(12,
                        fileInput("file", "Subir base de datos (.xlsx o .csv)"),
                        hr()
                 )
               ),
               fluidRow(
                 column(12,
                        h4("Definir tipo de variable"),
                        helpText("Selecciona el tipo de cada columna de la base cargada:"),
                        uiOutput("var_type_ui")
                 )
               ),
               width = 3
             ),
             
             # --- Panel principal ---
             mainPanel(
               tabsetPanel(
                 id = "tabs",
                 # ---- Subpestaña: Vista de Datos ----
                 tabPanel("Vista de Datos",
                          h4("Vista previa de la base de datos"),
                          DTOutput("table")
                 ),
                 
                 # ---- Subpestaña: Estadística Descriptiva ----
                 tabPanel("Estadística Descriptiva",
                          uiOutput("resumen_ui"),
                          h4("Resumen general"),
                          tableOutput("tabla_general"),
                          h4("Histograma"),
                          plotOutput("histograma"),
                          h4("Resumen agrupado"),
                          actionButton("add_table", "Agregar tabla"),
                          br(), br(),
                          div(id = "tablas_container")
                 ),
                 
                 # ---- Subpestaña: Gráficos ----
                 tabPanel("Gráficos",
                          h4("Generar gráficos"),
                          actionButton("add_graf", "Agregar gráfico"),
                          br(),
                          div(id = "graficas_container")
                 )
               )
             )
           )
  ),
  
  # --- Pestaña 2: Modelo Lineal ---------
  tabPanel("Modelo Lineal",
           div(
             style = "display: grid; width: 100%; height: 80vh; 
             grid-template-columns: 380px 1fr; gap: 12px; 
             padding: 12px; box-sizing: border-box;",
             
             # Panel izquierdo interno para configuraciones del modelo
             div(
               style = "background-color: white; border-radius: 12px; padding: 18px; 
               box-shadow: 0 4px 12px rgba(0,0,0,0.12); overflow-y: auto;",
               h4("Variable Respuesta"),
               selectInput("respuesta", "Seleccione la variable a modelar",
                           choices = c("Pico de lactancia" = "pico",
                                       "Persistencia" = "persistencia",
                                       "Producción diaria" = "prod_dia")),
               numericInput("dia_produccion", "Día para producción diaria", value = 100, min = 1, max = 400),
               numericInput("dia_persistencia", "Día para cálculo de persistencia", value = 280, min = 30, max = 400),
               hr(),
               h4("Variables Predictoras"),
               uiOutput("selector_predictores"),
               uiOutput("controles_polinomio"),
               hr(),
               actionButton("ajustar_modelo", "Ajustar Modelo", class = "btn-primary")
             ),
             
             # Panel derecho para resultados
             div(
               style = "flex: 1 1 auto; min-height: 400px;",
               tabsetPanel(
                 tabPanel("Datos", DTOutput("tabla_datos")),
                 tabPanel("Modelo",
                          h4("Fórmula del modelo"),
                          verbatimTextOutput("formula_modelo"),
                          h4("ANOVA Tipo III"),
                          tableOutput("tabla_anova"),
                          h4("Coeficientes del modelo"),
                          DTOutput("tabla_coeficientes"),
                          h4("Resumen del modelo"),
                          tableOutput("resumen_modelo")),
                 tabPanel(
                   "Diagnósticos",
                   h4("Prueba de Normalidad (Shapiro-Wilk)"),
                   tableOutput("prueba_normalidad"),
                   h4("Prueba de Homocedasticidad (Breusch-Pagan)"),
                   tableOutput("prueba_homocedasticidad")
                 )
               )
             )
           )),
  # --- Pestaña 3: Curva de Lactancia  ----------
  tabPanel(
    "Modelo de Wood",
    
    # habilitar scroll del body (por si algún tema lo bloquea)
    header = tagList(
      tags$head(
        tags$style(HTML("
          html, body { height: auto; overflow-y: auto; }
        "))
      )
    ),
    
    # --- Área superior fija ---
    div(
      style = "
        display: grid;
        width: 100%;
        height: 83vh;
        grid-template-columns: 380px 1fr;  /* columna izquierda fija, derecha flexible */
        grid-template-rows: 1fr;
        gap: 12px;
        padding: 12px;
        box-sizing: border-box;
      ",
      
      # Columna izquierda: Controles
      div(
        style = "
          background-color: white;
          border-radius: 12px;
          padding: 18px;
          box-shadow: 0 4px 12px rgba(0,0,0,0.12);
          overflow-y: auto;
        ",
        selectInput(
          "Raza",
          label = h3("Raza:"),
          choices = c("HOL", "1/2HOL1/2JER", "1/2HOL1/2BON", "3/4HOL1/4BON"),
          selected = "HOL"
        ),
        selectInput(
          "Parto",
          label = h3("Número de parto:"),
          choices = c(1, 2, 3, 4, 5, 6),
          selected = 1
        ),
        tags$hr(),
        sliderInput("a", "Parámetro a (Prod inicial):", min = 3, max = 40, value = 15),
        sliderInput("b", "Parámetro b (Ascenso al pico):",  min = 0, max = 1,     value = 0.12),
        sliderInput("c", "Parámetro c (Descenso post-pico):", min = 0, max = 0.01, value = 0.0026)
      ),
      
      # Columna derecha: Gráfico
      div(
        style = "flex: 1 1 auto; min-height: 400px;",
        uiOutput("plot_slot")     # <- aquí se renderiza uno u otro
      )
    ),
    
    # --- Sección inferior a TODO el ancho (aparece al scrollear) ---
    div(
      style = "width:100%; padding:12px; box-sizing:border-box;",
      
      # CONTENEDOR HORIZONTAL
      div(
        style = paste(
          "display:flex; gap:16px; align-items:flex-start; flex-wrap:wrap;",
          # cuando hay espacio, van lado a lado; en pantallas angostas se apilan
          sep = " "
        ),
        
        # ---- TARJETA IZQUIERDA: selector + botón ----
        div(
          style = paste(
            "background-color:white; border-radius:12px; padding:16px;",
            "box-shadow:0 4px 12px rgba(0,0,0,0.12);",
            "flex:0 0 360px; max-width:420px;"  # ancho fijo cómodo para controles
          ),
          selectInput(
            "ID",
            label = h3("Seleccione hembra:"),
            choices = NULL
          ),
          actionButton("Btn_plot_real", label = "Graficar curva real")
        ),
        
        # ---- TARJETA DERECHA: resumen 305d ----
        div(
          style = paste(
            "background-color:white; border-radius:12px; padding:16px;",
            "box-shadow:0 4px 12px rgba(0,0,0,0.12);",
            "flex:1 1 0; min-width:280px;"        # ocupa el resto del espacio
          ),
          h3("Resumen a 305 días"),
          p("Cálculo sobre la curva de Wood. La columna del animal aparece al pulsar ",
            strong("Graficar curva real"), "."),
          tableOutput("tabla_305")
        )
      )
    )
  )
)

