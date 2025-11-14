require(shiny)
require(readxl)
require(tidyverse)
require(writexl)
require(plotly)
require(ggplot2)
require(car) # Modelos
library(lubridate)
require(broom) # Modelos
require(lmtest) # Modelos


# Servidor
options(shiny.maxRequestSize = 100 * 1024^2) # Para poder cargar archivos grandes

function(input, output, session) {
  
  # Cargar base de datos ----
  data <- reactive({
    req(input$file)
    read_excel(input$file$datapath) |>
      mutate(fnac = as.Date(fnac),
             fp = as.Date(fp),
             f_control = as.Date(f_control))
  })
  
  # Mostrar base cargada ----
  output$table <- renderDT({
    req(data())
    datatable(data(), options = list(pageLength = 10))
  })
  
  # Seleccionar tipo de variable ----
  output$var_type_ui <- renderUI({
    req(data())
    df <- data()
    
    lapply(names(df), function(col) {
      selectInput(
        inputId = paste0("type_", col),
        label = paste("Tipo de variable:", col),
        choices = c("numérica", "categórica", "texto", "fecha"),
        selected = if (is.numeric(df[[col]])) "numérica" else "categórica",
      )
    })})
  
  # Aplicar tipo de variable seleccionada ----
  data_typed <- reactive({
    req(data())
    df <- data()
    
    for (col in names(df)) {
      tipo <- input[[paste0("type_", col)]]
      if (!is.null(tipo)) {
        if (tipo == "numérica") {
          df[[col]] <- as.numeric(df[[col]])
        } else if (tipo == "categórica") {
          df[[col]] <- as.factor(df[[col]])
        } else if (tipo == "texto") {
          df[[col]] <- as.character(df[[col]])
        } else if (tipo == "fecha") {
          df[[col]] <- lubridate::as_date(df[[col]])
        }
      }
    }
    
    df
  })
  
  # Elección de variable para estadistica descriptiva ----
  
  output$resumen_ui <- renderUI({
    req(data_typed())
    df <- data_typed()
    tagList(
      selectInput("var_interes", "Variable de interés:",
                  choices = names(df), selected = names(df)[7]))
  })
  
  # --- TABLA GENERAL ---
  output$tabla_general <- renderTable({
    req(input$var_interes)
    df <- data_typed()
    var <- input$var_interes
    
    data.frame(
      Variable = var,
      Media = mean(df[[var]], na.rm = TRUE),
      'Desviación Estándar' = sd(df[[var]], na.rm = TRUE),
      Mínimo = min(df[[var]], na.rm = TRUE),
      Máximo = max(df[[var]], na.rm = TRUE),
      Mediana = median(df[[var]], na.rm = TRUE),
      n = sum(!is.na(df[[var]]))
    )
  })
  
  # --- HISTOGRAMA ---
  output$histograma <- renderPlot({
    req(input$var_interes)
    df <- data_typed()
    var <- input$var_interes
    
    if (!is.numeric(df[[var]])) {
      showNotification("La variable seleccionada no es numérica", type = "error")
      return(NULL)
    }
    
    ggplot(df, aes_string(x = var)) +
      geom_histogram(fill = "skyblue", color = "white", bins = 30) +
      labs(x = var, y = "Frecuencia", title = paste("Histograma de", var)) +
      theme_minimal()
  })
  
  # --- TABLA POR GRUPO ---
  observeEvent(input$add_table, {
    req(data_typed())
    df <- data_typed()
    
    # ID único para cada nueva tabla
    new_id <- paste0("tabla_grupo_", input$add_table)
    
    # Insertar nueva tabla
    insertUI(
      selector = "#tablas_container",
      where = "beforeEnd",
      ui = tagList(
        hr(),
        uiOutput(paste0("grupo_ui_", new_id)),
        tableOutput(paste0("tabla_grupos_", new_id))
      )
    )
    
    output[[paste0("grupo_ui_", new_id)]] <- renderUI({
      selectInput(
        paste0("var_grupo_", new_id),
        "Agrupar por:",
        choices = c("Ninguna", names(df)),
        selected = "Ninguna"
      )
    })
    
    # Tabla según la variable seleccionada
    output[[paste0("tabla_grupos_", new_id)]] <- renderTable({
      req(input[[paste0("var_grupo_", new_id)]])
      var_grp <- input[[paste0("var_grupo_", new_id)]]
      var_int <- input$var_interes
      
      if (var_grp == "Ninguna") return(NULL)
      
      df %>%
        group_by(!!sym(var_grp)) %>%
        summarise(
          Media = mean(!!sym(var_int), na.rm = TRUE),
          `Desviación Estándar` = sd(!!sym(var_int), na.rm = TRUE),
          Mínimo = min(!!sym(var_int), na.rm = TRUE),
          Máximo = max(!!sym(var_int), na.rm = TRUE),
          Mediana = median(!!sym(var_int), na.rm = TRUE),
          n = sum(!is.na(!!sym(var_int))),
          .groups = "drop"
        )
    })
  })
  
  # Elección de variables para graficar ----
  observeEvent(input$add_graf, {
    req(data_typed())
    df <- data_typed()
    
    # ID único para cada nueva grafica
    new_gf <- paste0("graf_", input$add_graf)
    
    # Insertar nueva grafica
    insertUI(
      selector = "#graficas_container",
      where = "beforeEnd",
      ui = tagList(
        hr(),
        fluidRow(
          column(4,
                 selectInput(paste0("xvar_", new_gf), "Variable X", choices = names(df))),
          column(4,
                 selectInput(paste0("yvar_", new_gf), "Variable Y", choices = names(df), 
                             selected = names(df)[2])),
          column(4,
                 selectInput(paste0("plot_type_", new_gf), "Tipo de gráfico",
                             choices = c("Boxplot" = "box", "Barras" = "bar", "Dispersión" = "scatter")))
        ),
        plotlyOutput(paste0("plot_", new_gf))
      )
    )
    
    local({
      id <- new_gf  # Para evitar problemas de entorno
      
      output[[paste0("plot_", id)]] <- renderPlotly({
        req(input[[paste0("xvar_", id)]], input[[paste0("yvar_", id)]], input[[paste0("plot_type_", id)]])
        df <- data_typed()
        
        xvar <- input[[paste0("xvar_", id)]]
        yvar <- input[[paste0("yvar_", id)]]
        tipo <- input[[paste0("plot_type_", id)]]
        
        # Tipos de gráfico
        if (tipo == "scatter") {
          ggplotly(ggplot(df, aes_string(x = xvar, y = yvar)) +
                     geom_point(color = "steelblue", size = 3) +
                     theme_minimal())
        } else if (tipo == "bar") {
          ggplotly(ggplot(df, aes_string(x = xvar, fill = yvar)) +
                     geom_bar(position = "dodge") +
                     theme_minimal())
        } else if (tipo == "box") {
          ggplotly(ggplot(df, aes_string(x = xvar, y = yvar)) +
                     geom_boxplot(fill = "lightblue") +
                     theme_minimal())
        }
      })
    })
  })
  # ==========================================================
  # MODELO LINEAL
  # ==========================================================
  
  # ==========================================================
  # FUNCIONES AUXILIARES PARA LACTANCIAS
  # ==========================================================
  estimar_valor_dia <- function(datos_lactancia, dia_objetivo) {
    datos_lactancia <- datos_lactancia %>%
      group_by(del) %>%
      summarise(pl_dia = mean(pl_dia, na.rm = TRUE), .groups = "drop") %>%
      arrange(del)
    
    if (nrow(datos_lactancia) < 2) return(NA_real_)
    if (min(datos_lactancia$del) <= dia_objetivo && max(datos_lactancia$del) >= dia_objetivo) {
      approx_resultado <- approx(x = datos_lactancia$del, y = datos_lactancia$pl_dia, 
                                 xout = dia_objetivo, rule = 1)
      return(as.numeric(approx_resultado$y))
    } else return(NA_real_)
  }
  
  calcular_caracteristicas <- function(datos, dia_produccion = 100, dia_persistencia = 280) {
    
    # PASO 1: VERIFICAR/CALCULAR DEL (Días en Leche)
    if (!("del" %in% names(datos))) {
      # Verificar si existen las columnas necesarias para calcular DEL
      if (!("f_control" %in% names(datos)) || !("fp" %in% names(datos))) {
        stop("Para calcular DEL, el dataset debe contener las columnas 'del' o, en su defecto, 'f_control' y 'fp'.")
      }
      
      # Calcular DEL
      datos <- datos %>%
        mutate(
          # Convertir a Date si aún no lo están (útil si la función se llama antes de data_typed())
          f_control = lubridate::as_date(f_control),
          fp = lubridate::as_date(fp),
          # Calcular los días en leche (DEL)
          del = as.numeric(difftime(f_control, fp, units = "days")),
          # Reemplazar DEL <= 0 por NA (o por 1 si se prefiere)
          del = ifelse(is.na(del) | del <= 0, NA_real_, del)
        ) %>%
        # Eliminar filas con DEL no válido
        filter(!is.na(del))
    }
    
    # Verificar que las columnas clave para el agrupamiento existan antes de agrupar
    required_cols <- c("nombre", "np", "pl_dia", "del")
    if (!all(required_cols %in% names(datos))) {
      missing_cols <- required_cols[!required_cols %in% names(datos)]
      stop(paste("Faltan columnas esenciales para el cálculo de características de lactancia:", paste(missing_cols, collapse = ", ")))
    }
    
    # Continuar con el código original, que ahora usa la columna 'del'
    datos %>%
      group_by(nombre, np) %>%
      group_split() %>%
      map_dfr(function(datos_lactancia) {
        # ... el resto del código es el mismo, asumiendo que 'del' existe ...
        datos_lactancia <- arrange(datos_lactancia, del)
        tibble(
          nombre = first(datos_lactancia$nombre),
          np = first(datos_lactancia$np),
          # Incluir raza, fnac, fp, f_control solo si existen en el subconjunto
          raza = if("raza" %in% names(datos_lactancia)) first(datos_lactancia$raza) else NA,
          fnac = if("fnac" %in% names(datos_lactancia)) first(datos_lactancia$fnac) else NA,
          fp = first(datos_lactancia$fp),
          f_control = first(datos_lactancia$f_control),
          pico = suppressWarnings(max(datos_lactancia$pl_dia, na.rm = TRUE)),
          prod_dia_objetivo = estimar_valor_dia(datos_lactancia, dia_produccion),
          prod_dia_persistencia = estimar_valor_dia(datos_lactancia, dia_persistencia)
        )
      }) %>%
      mutate(
        persistencia = ifelse(is.finite(pico) & !is.na(pico) & !is.na(prod_dia_persistencia) & pico > 0,
                              100 * prod_dia_persistencia / pico, NA_real_),
        # Cálculos basados en fechas (si fnac y fp existen)
        edad_parto_dias = ifelse("fnac" %in% names(.) & "fp" %in% names(.), 
                                 as.numeric(as_date(fp) - as_date(fnac)), NA_real_),
        anio_parto = year(as_date(fp)),
        anio_nacimiento = ifelse("fnac" %in% names(.), year(as_date(fnac)), NA_integer_),
        anio_control = year(as_date(f_control)),
        epoca_parto = factor(case_when(
          month(as_date(fp)) %in% 1:3 ~ "E1",
          month(as_date(fp)) %in% 4:6 ~ "E2",
          month(as_date(fp)) %in% 7:9 ~ "E3",
          month(as_date(fp)) %in% 10:12 ~ "E4"
        ), levels = c("E1", "E2", "E3", "E4"))
      )
  }
  
  # ==========================================================
  # PROCESAR DATOS DE LACTANCIA
  # ==========================================================
  variables_predictoras <- reactiveVal(NULL)
  
  datos_procesados <- reactive({
    req(data_typed())
    datos <- data_typed()
    
    caracteristicas <- calcular_caracteristicas(datos, input$dia_produccion, input$dia_persistencia)
    
    if (input$respuesta == "pico") {
      datos_final <- caracteristicas %>% rename(respuesta = pico)
    } else if (input$respuesta == "persistencia") {
      datos_final <- caracteristicas %>% rename(respuesta = persistencia)
    } else {
      datos_final <- caracteristicas %>% rename(respuesta = prod_dia_objetivo)
    }
    
    datos_final <- datos_final %>% 
      filter(!is.na(respuesta) & is.finite(respuesta)) %>%
      mutate(across(where(is.character), as.factor)) %>%
      mutate(
        across(
          .cols = c(anio_parto, anio_nacimiento, anio_control),
          .fns = ~ {
            if (length(unique(.x)) >= 2) {
              as.factor(.x)
            } else {
              as.numeric(.x)
            }
          }
        )
      )
      
    vars_posibles <- c("np", "raza", "edad_parto_dias", "anio_parto", 
                       "anio_nacimiento", "anio_control", "epoca_parto")
    vars_posibles <- vars_posibles[vars_posibles %in% names(datos_final)]
    variables_predictoras(vars_posibles)
    
    datos_final
  })
  
  output$selector_predictores <- renderUI({
    req(variables_predictoras())
    selectizeInput("predictores", "Seleccione las variables predictoras",
                   choices = variables_predictoras(), multiple = TRUE)
  })
  
  output$tabla_datos <- DT::renderDataTable({
    req(datos_procesados())
    
    df_rounded <- datos_procesados() %>%
      mutate(across(where(is.numeric), round, 2)) %>% 
      mutate(fnac = as.Date(fnac),
             fp = as.Date(fp),
             f_control = as.Date(f_control),
             anio_parto = as.factor(anio_parto),
             anio_nacimiento = as.factor(anio_nacimiento),
             anio_control = as.factor(anio_control))
    
    DT::datatable(df_rounded, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # ==========================================================
  # AJUSTE DEL MODELO LINEAL
  # ==========================================================
  modelo_ajustado <- reactiveVal(NULL)
  
  observeEvent(input$ajustar_modelo, {
    req(datos_procesados(), input$predictores)
    df <- datos_procesados()
    
    options(contrasts = c("contr.sum", "contr.poly"))
    formula_txt <- paste("respuesta ~", paste(input$predictores, collapse = " + "))
    formula_mod <- as.formula(formula_txt)
    
    mod <- lm(formula_mod, data = df)
    modelo_ajustado(mod) # <-- Guardamos el modelo para usarlo después
    
    output$formula_modelo <- renderPrint({ print(formula_mod) })
    output$resumen_modelo <- renderTable({
      broom::glance(mod) %>% mutate(across(where(is.numeric), round, 2)) 
    })
    
    output$formula_modelo <- renderPrint({ 
  # Extrae y pega la variable de respuesta y los predictores
  formula_txt <- paste(all.vars(formula_mod), collapse = " ~ ")
  cat(formula_txt)
})
    
    output$tabla_coeficientes <- renderDT({
      broom::tidy(mod, conf.int = TRUE) %>% mutate(across(where(is.numeric), round, 2)) # CAMBIO: round, 2
    })
    
    output$tabla_anova <- renderTable({
      tryCatch({
        a <- car::Anova(mod, type = 3)
        as.data.frame(a) %>% tibble::rownames_to_column("Término") %>%
          mutate(across(where(is.numeric), round, 2))
      }, error = function(e) {
        data.frame(Mensaje = "⚠️ No se pudo calcular ANOVA tipo III. Revise colinealidad o contrastes.")
      })
      })
  })
  
  # ==========================================================
  # PRUEBAS DE SUPUESTOS
  # ==========================================================
  
  output$prueba_normalidad <- renderTable({
    req(modelo_ajustado())
    prueba <- shapiro.test(resid(modelo_ajustado()))
    data.frame(
      Estadistico_W = round(prueba$statistic, 2), # CAMBIO: round, 2
      Valor_p = round(prueba$p.value, 2) # CAMBIO: round, 2
    )
  })
  output$prueba_homocedasticidad <- renderTable({
    req(modelo_ajustado())
    prueba <- lmtest::bptest(modelo_ajustado())
    data.frame(
      Estadistico_BP = round(prueba$statistic, 2), # CAMBIO: round, 2
      Valor_p = round(prueba$p.value, 2) # CAMBIO: round, 2
    )
  })
  
  # ==========================================================
  # MODELO DE WOOD
  # ==========================================================
  # La función observe se ejecuta al inicio y cada vez que cambia data_typed.
  observe({
    df <- data_typed()
    # Requiere que la base esté cargada y tenga las columnas 'raza' y 'np'
    req(df, "raza" %in% names(df), "np" %in% names(df))  
    
    # --- 1. Razas ---
    razas <- df$raza |> 
      as.character() |> 
      unique() |> 
      sort()
    
    selected_raza <- if ("HOL" %in% razas) "HOL" else if (length(razas) > 0) razas[1] else NULL
    
    updateSelectInput(
      session, 
      "Raza", 
      choices = razas, 
      selected = selected_raza
    )
    
    # --- Número de Parto (NP) ---
    partos <- df$np |> 
      as.character() |> 
      unique() |> 
      sort()
    
    selected_parto <- if ("1" %in% partos) "1" else if (length(partos) > 0) partos[1] else NULL
    
    updateSelectInput(
      session, 
      "Parto", 
      choices = partos, 
      selected = selected_parto
    )
  })
  # 1. Datos de Lactancia Filtrados (Lac()) - ¡CORREGIDO!
  Lac <- reactive({
    req(input$Raza, input$Parto)
    df_raw <- data_typed()
    req(df_raw)
    
    # columnas requeridas
    req_cols <- c("nombre", "raza", "np", "f_control", "fp", "pl_dia")
    present <- req_cols %in% names(df_raw)
    
    if (!all(present)) {
      missing_cols <- req_cols[!present]
      showNotification(paste0("Faltan columnas en la base: ", paste(missing_cols, collapse = ", ")),
                       type = "error", duration = 8)
      return(tibble()) # tibble vacía para evitar errores posteriores
    }
    
    df <- df_raw %>%
      # Seleccionar y renombrar (si es necesario) para consistencia
      dplyr::select(nombre = !!sym("nombre"),
                    raza = !!sym("raza"),
                    np = !!sym("np"),
                    f_control = !!sym("f_control"),
                    fp = !!sym("fp"),
                    pl_dia = !!sym("pl_dia")) %>%
      # Conversiones y limpieza básica
      mutate(
        # Convertir a character para asegurar que la comparación funcione
        raza_char = as.character(raza),
        np_char = as.character(np),
        pl_dia = as.numeric(pl_dia),
        f_control = lubridate::as_date(f_control),
        fp = lubridate::as_date(fp)
      ) %>%
      # Filtrar filas con datos mínimos
      filter(!is.na(nombre), !is.na(raza_char), !is.na(np_char), !is.na(pl_dia)) %>%
      # Calcular Del_Ctrl
      mutate(
        Del_Ctrl = as.numeric(difftime(f_control, fp, units = "days"))
      ) %>%
      # Filtrar Del_Ctrl válidos
      filter(!is.na(Del_Ctrl), Del_Ctrl >= 1, is.finite(pl_dia)) # DEL debe ser >= 1
    
    # --- FILTRADO CORRECTO POR RAZA Y PARTO (AQUÍ ESTABA EL DETALLE) ---
    df_filtered <- df %>%
      filter(
        raza_char == input$Raza,
        np_char == input$Parto
      )
    # ------------------------------------------------------------------
    
    # Si no hay datos suficientes, aviso y devuelvo tibble vacía
    if (nrow(df_filtered) == 0) {
      showNotification(paste0("No hay datos para ", input$Raza, " en el Parto ", input$Parto), 
                       type = "warning", duration = 5)
      return(tibble())
    }
    
    # Si quedan menos de 4 puntos no tiene sentido ajustar nls; devolvemos tibble pero con aviso
    if (nrow(df_filtered) < 4) {
      showNotification("Muy pocos puntos (< 4) después de limpieza. La optimización NLS podría fallar.", 
                       type = "warning", duration = 6)
    }
    
    # Ordenar por Del_Ctrl
    df_filtered %>% arrange(Del_Ctrl)
  })
  
  
  # 2. Curva Wood (Modelo)
  Wood_m <- reactive({
    # Al requerir Lac(), si Lac() devuelve una tibble vacía, Wood_m() no se ejecuta.
    req(input$a, input$b, input$c, Lac()) 
    
    df_lac <- Lac()
    # Si Lac() está vacío, salimos con tibble vacía para no dibujar.
    if (nrow(df_lac) == 0) return(tibble())
    
    # Usamos el valor de los sliders para dibujar la curva
    tibble::tibble(Del_Ctrl = 1:400) |>
      dplyr::mutate(
        PL = input$a * (Del_Ctrl^input$b) * exp(-input$c * Del_Ctrl),
        PL_min = PL * (1 - 1.96 * 0.04),
        PL_max = PL * (1 + 1.96 * 0.04)
      )
  })
  
  # 3. Optimización Automática de Parámetros (al cambiar Raza/Parto)
  observeEvent(
    list(input$Raza, input$Parto),
    {
      df_lac <- Lac()
      
      # Volver al gráfico base al cambiar los filtros de Parto/Raza
      show_real(FALSE) 
      
      req(nrow(df_lac) > 3) # Se necesitan al menos 4 puntos para el ajuste nls
      
      # Solo ajustamos si hay datos
      fit_try <- try(
        nls(
          pl_dia ~ a * (Del_Ctrl^b) * exp(-c * Del_Ctrl),
          data = df_lac,
          start = list(a = input$a, b = input$b, c = input$c),
          algorithm = "port",
          lower = c(1e-6, 1e-6, 1e-6)
        ),
        silent = TRUE
      )
      
      if (!inherits(fit_try, "try-error")) {
        co <- coef(fit_try)
        updateSliderInput(session, "a", value = unname(co["a"]))
        updateSliderInput(session, "b", value = unname(co["b"]))
        updateSliderInput(session, "c", value = unname(co["c"]))
      } else {
        # Opcional: Notificación si el ajuste falla
        # showNotification("El ajuste NLS poblacional falló. Usando valores por defecto.", type = "warning")
      }
    },
    ignoreInit = FALSE # Queremos que se ejecute la primera vez para actualizar los sliders
  )
  
  # 4. Actualizar SelectInput de ID de Animal
  observe({
    df <- Lac()
    req(df)
    ids <- df$nombre |> as.character() |> unique() |> sort()
    updateSelectInput(session, "ID", choices = ids, selected = if (length(ids) > 0) ids[1] else NULL)
  })
  
  # 5. Flags y Eventos para alternar gráficos
  show_real <- reactiveVal(FALSE)
  
  # Ahora, solo mostrará el individual si se hace click en el botón.
  # El cambio de ID o el click son los eventos que disparan la visualización individual.
  observeEvent(input$Btn_plot_real, {
    show_real(TRUE) # mostrar gráfico con curva real al dar click
  })
  
  # Si el usuario cambia el animal, pero el flag está en TRUE, fuerza el reajuste del Individual (para que se use el ID correcto)
  observeEvent(input$ID, {
    if (show_real() == TRUE) {
      show_real(FALSE)
      show_real(TRUE)
    }
  }, ignoreInit = TRUE)
  
  # 6. Serie real del animal seleccionado (EventReactive para el ajuste)
  # Lo mantenemos como EventReactive para que el ajuste individual solo se haga al presionar el botón
  real_sel <- eventReactive(input$Btn_plot_real, { 
    Lac() %>%
      dplyr::filter(nombre == input$ID) %>% # Ya filtrado por Raza/Parto en Lac()
      dplyr::arrange(Del_Ctrl)
  })
  
  # 7. Slot que intercambia los output
  output$plot_slot <- renderUI({
    # Si el flag está en TRUE (después del click), se muestra el Individual vs Poblacional
    if (show_real()) {
      plotlyOutput("plot_Wood_ind_vs_pop", height = "100%") # Muestra Individual vs Poblacional
    } else {
      plotlyOutput("plot_Wood_m", height = "100%")           # Muestra Curva Poblacional base
    }
  })
  
  # 8. Gráfico base (Modelo Poblacional)
  output$plot_Wood_m <- plotly::renderPlotly({
    df <- Wood_m()
    p_pk <- df[which.max(df$PL), , drop = FALSE]
    p_st <- df[df$Del_Ctrl == 1, , drop = FALSE]
    
    g <- ggplot2::ggplot(df, ggplot2::aes(Del_Ctrl, PL)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = PL_min, ymax = PL_max),
                           fill = "skyblue", alpha = 0.3) +
      ggplot2::geom_line(colour = "#112446", linewidth = 1) +
      ggplot2::geom_point(data = p_pk, ggplot2::aes(Del_Ctrl, PL), size = 3, color = "#d62728") +
      ggplot2::geom_point(data = p_st, ggplot2::aes(Del_Ctrl, PL), size = 3, color = "#2ca02c") +
      ggplot2::labs(
        x = "Días en leche (DEL)", y = "Producción diaria (kg/día)",
        title = paste0("Curva de lactancia (Wood) • ", input$Raza, " • Parto ", input$Parto)
      ) +
      ggplot2::theme_minimal()
    
    plotly::ggplotly(g, tooltip = c("x","y"))
  })
  
  # 9. Coeficientes individuales al pulsar el botón
  coefs_ind <- eventReactive(input$Btn_plot_real, {
    df_ind <- Lac() %>%
      dplyr::filter(nombre == input$ID) %>%
      dplyr::filter(is.finite(pl_dia), is.finite(Del_Ctrl), Del_Ctrl > 0)
    
    if (nrow(df_ind) < 4) {
      showNotification("Datos insuficientes para ajuste individual. Usando coeficientes poblacionales.", type = "warning")
      return(c(a = input$a, b = input$b, c = input$c))
    }
    
    # Intento principal: nls con límites (parámetros > 0)
    fit_try <- try(
      nls(
        pl_dia ~ a * (Del_Ctrl^b) * exp(-c * Del_Ctrl),
        data = df_ind,
        start = list(a = input$a, b = input$b, c = input$c),
        algorithm = "port",
        lower = c(1e-6, 1e-6, 1e-6)
      ),
      silent = TRUE
    )
    
    if (inherits(fit_try, "try-error")) {
      
      fit_try2 <- try(
        nls(
          pl_dia ~ a * (Del_Ctrl^b) * exp(-c * Del_Ctrl),
          data = df_ind,
          start = list(
            a = max(1e-3, input$a * 0.9),
            b = max(1e-3, input$b * 0.9),
            c = max(1e-3, input$c * 1.1)
          ),
          algorithm = "port",
          lower = c(1e-6, 1e-6, 1e-6)
        ),
        silent = TRUE
      )
      if (inherits(fit_try2, "try-error")) {
        showNotification("Ajuste NLS individual fallido. Usando coeficientes poblacionales.", type = "error")
        return(c(a = input$a, b = input$b, c = input$c))
      } else {
        return(coef(fit_try2))
      }
    } else {
      return(coef(fit_try))
    }
  })
  
  # 10. Curva Wood individual (mismo grid 1:400)
  Wood_ind <- reactive({
    co <- coefs_ind()
    tibble::tibble(Del_Ctrl = 1:400) |>
      dplyr::mutate(
        PL_ind = co[["a"]] * (Del_Ctrl^co[["b"]]) * exp(-co[["c"]] * Del_Ctrl)
      )
  })
  
  # 11. Gráfico Wood INDIVIDUAL vs POBLACIONAL (SOLO CURVAS)
  output$plot_Wood_ind_vs_pop <- plotly::renderPlotly({
    req(input$ID)
    df_pop  <- Wood_m()
    df_indc <- Wood_ind()
    
    # Quitamos df_real del req() ya que no se usa para graficar puntos reales en este output
    
    p_pk <- df_pop[which.max(df_pop$PL), , drop = FALSE]
    p_st <- df_pop[df_pop$Del_Ctrl == 1, , drop = FALSE]
    
    g <- ggplot2::ggplot(df_pop, ggplot2::aes(Del_Ctrl, PL)) +
      # Banda de incertidumbre poblacional
      ggplot2::geom_ribbon(ggplot2::aes(ymin = PL_min, ymax = PL_max),
                           fill = "skyblue", alpha = 0.3) +
      # Curva Wood poblacional
      ggplot2::geom_line(colour = "#112446", linewidth = 1, aes(text = "Poblacional"))
    
    # Añade la curva Wood individual
    if (!is.null(df_indc) && nrow(df_indc) > 0) {
      g <- g +
        # Añadido linetype = "dashed" para curva punteada y quitado geom_point.
        ggplot2::geom_line(
          data = df_indc,
          ggplot2::aes(Del_Ctrl, PL_ind, text = "Individual"),
          linewidth = 1, linetype = "dashed", color = "#e67e22" # <--- LÍNEA PUNTEADA
        )
    }
    
    g <- g +
      ggplot2::geom_point(data = p_pk, ggplot2::aes(Del_Ctrl, PL), size = 3, color = "#d62728") +
      ggplot2::geom_point(data = p_st, ggplot2::aes(Del_Ctrl, PL), size = 3, color = "#2ca02c") +
      ggplot2::labs(
        x = "Días en leche (DEL)", y = "Producción diaria (kg/día)",
        title = paste0("Curva Wood (Poblacional vs Individual: ", input$ID, ")")
      ) +
      ggplot2::theme_minimal()
    
    plotly::ggplotly(g, tooltip = c("text", "x", "y"))
  })
  
  # 12. HELPERS
  fmt_num <- function(x, d = 3) format(round(x, d), big.mark = ".", decimal.mark = ",", nsmall = d)
  
  # 13. A 305 días: curva poblacional (siempre disponible)
  stats_305_pop <- reactive({
    df <- Wood_m()
    df <- df[df$Del_Ctrl <= 305 & df$Del_Ctrl > 0, , drop = FALSE]
    total <- sum(df$PL, na.rm = TRUE)
    list(total = total, mean = total / 305)
  })
  
  # 14. A 305 días: curva individual (solo tras botón)
  stats_305_ind <- eventReactive(input$Btn_plot_real, {
    df <- Wood_ind()
    if (is.null(df) || nrow(df) == 0) {
      return(NULL)
    }
    df <- df[df$Del_Ctrl <= 305 & df$Del_Ctrl > 0, , drop = FALSE]
    total <- sum(df$PL_ind, na.rm = TRUE)
    list(total = total, mean = total / 305)
  })
  
  # 15. Tabla combinada para UI
  output$tabla_305 <- renderTable({
    pop <- stats_305_pop()
    ind <- stats_305_ind()
    
    df_out <- tibble::tibble(
      Métrica = c("Producción total a 305 d (kg)", "Promedio diario (kg/día)"),
      `Wood poblacional` = c(fmt_num(pop$total), fmt_num(pop$mean))
    )
    
    if (!is.null(ind)) {
      df_out$`Wood individual (animal)` <- c(fmt_num(ind$total), fmt_num(ind$mean))
      
      dif_total <- ind$total - pop$total
      df_out <- dplyr::bind_rows(
        df_out,
        tibble::tibble(
          Métrica = "Diferencia total vs poblacional (kg)",
          `Wood poblacional` = "—",
          `Wood individual (animal)` = fmt_num(dif_total)
        )
      )
    }
    
    df_out
  }, striped = TRUE, bordered = TRUE, digits = 1, na = "—")
  
}
