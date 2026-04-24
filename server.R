server <- function(input, output, session) {

  datos <- eventReactive(input$calcular, {
    req(input$csv_input)

    sep <- if (grepl(";", input$csv_input)) ";" else ","
    df <- tryCatch(
      read.csv(text = input$csv_input, sep = sep, stringsAsFactors = FALSE,
               strip.white = TRUE, encoding = "UTF-8"),
      error = function(e) NULL
    )

    if (is.null(df)) return(NULL)

    names(df) <- str_trim(names(df))
    col_nombre <- names(df)[str_detect(tolower(names(df)), "nombre|name|alumno")]
    col_punt   <- names(df)[str_detect(tolower(names(df)), "punt|score|nota")]

    if (length(col_nombre) == 0 || length(col_punt) == 0) return(NULL)

    df <- df %>%
      select(Nombre = all_of(col_nombre[1]), Puntuacion = all_of(col_punt[1])) %>%
      mutate(
        Nombre     = str_trim(Nombre),
        Puntuacion = as.numeric(Puntuacion)
      ) %>%
      filter(!is.na(Nombre), !is.na(Puntuacion), Nombre != "")

    procesar_lista(df, input$sorteo_num)
  }, ignoreNULL = FALSE)

  # All students matching the current search term
  candidatos <- reactive({
    res <- datos()
    if (is.null(res)) return(data.frame())
    buscar <- str_trim(input$nombre_buscar)
    if (buscar == "") return(data.frame())
    res$resultado %>% filter(str_detect(tolower(Nombre), tolower(buscar)))
  })

  # Disambiguation selector rendered when multiple students match
  output$selector_alumno <- renderUI({
    cands <- candidatos()
    if (nrow(cands) <= 1) return(NULL)

    choices <- setNames(
      cands$Nombre,
      paste0(cands$Nombre, " — puesto ", cands$Posicion)
    )

    div(
      style = "background:#fff8e6; border:1px solid #e0d0a0; border-radius:6px; padding:16px 20px; margin-bottom:16px;",
      p(
        style = "margin:0 0 10px; font-size:13px; color:#7a5200; font-weight:600;",
        paste0("⚠️ Se encontraron ", nrow(cands),
               " alumnos con ‘", str_trim(input$nombre_buscar),
               "’. Selecciona el alumno:")
      ),
      selectInput("alumno_sel", NULL, choices = choices, width = "100%")
    )
  })

  # The single active student (resolved from 1 match or selection)
  fila_activa <- reactive({
    cands <- candidatos()
    if (nrow(cands) == 0) return(NULL)
    if (nrow(cands) == 1) return(cands[1, ])

    sel <- input$alumno_sel
    if (is.null(sel) || sel == "") return(cands[1, ])

    fila <- cands %>% filter(Nombre == sel)
    if (nrow(fila) == 0) return(cands[1, ])
    fila[1, ]
  })

  output$resultado_alumno <- renderUI({
    res <- datos()
    if (is.null(res)) return(NULL)
    buscar <- str_trim(input$nombre_buscar)
    if (buscar == "") return(NULL)

    cands <- candidatos()
    if (nrow(cands) == 0) {
      return(div(class = "panel-custom",
        p(style = "color:#b03030;",
          paste0("⚠️ No se encontró ningún alumno con '", buscar, "' en su nombre."))))
    }

    fila <- fila_activa()
    if (is.null(fila)) return(NULL)

    corte <- input$corte
    entra <- fila$Posicion <= corte

    div(class = "resultado-box",
      div(class = "nombre", fila$Nombre),
      div(
        style = "display:flex; gap:32px; align-items:flex-start; margin-top:12px;",
        div(
          div(class = "puesto", fila$Posicion),
          div(class = "puesto-label", "Posición final")
        ),
        div(
          div(style = "font-size:13px; opacity:0.7; margin-bottom:4px;", "Puntuación"),
          div(style = "font-size:28px; font-weight:700;", fila$Puntuacion),
          div(
            if (entra)
              tags$span(class = "entra-si",
                paste0("✓ ENTRA (puesto ", fila$Posicion, " de ", corte, ")"))
            else
              tags$span(class = "entra-no",
                paste0("✗ NO ENTRA (puesto ", fila$Posicion, ", corte en ", corte, ")"))
          )
        )
      )
    )
  })

  output$razonamiento_ui <- renderUI({
    res <- datos()
    if (is.null(res)) return(NULL)
    buscar <- str_trim(input$nombre_buscar)
    if (buscar == "") return(NULL)

    fila <- fila_activa()
    if (is.null(fila)) return(NULL)

    punt_str <- as.character(fila$Puntuacion)
    razon <- res$razonamientos[[punt_str]]
    corte <- input$corte
    df_res <- res$resultado

    plazas_antes <- df_res %>% filter(Puntuacion > fila$Puntuacion) %>% nrow()
    plazas_grupo <- max(0, corte - plazas_antes)

    div(class = "panel-custom",
      h4("\U0001f50d Razonamiento paso a paso"),

      div(class = "razonamiento-step",
        tags$strong("Paso 1 · Puntuación del alumno/a"),
        paste0(fila$Nombre, " tiene ", fila$Puntuacion, " puntos.")
      ),

      div(class = "razonamiento-step",
        tags$strong("Paso 2 · Alumnos con mayor puntuación"),
        paste0(plazas_antes, " alumnos tienen más de ", fila$Puntuacion,
               " puntos y ocupan los primeros ", plazas_antes, " puestos.")
      ),

      if (razon$n > 1) {
        tagList(
          div(class = "razonamiento-step",
            tags$strong("Paso 3 · Grupo de empate"),
            paste0("Hay ", razon$n, " alumnos empatados a ", fila$Puntuacion, " puntos. ",
                   "Quedan ", plazas_grupo, " plaza(s) disponibles para este grupo (",
                   corte, " - ", plazas_antes, " = ", plazas_grupo, ").")
          ),

          div(class = "razonamiento-step",
            tags$strong("Paso 4 · Cálculo del punto de inicio (Instrucción 9ª BOCM)"),
            tags$code(class = "formula-code", razon$calculo),
            paste0("→ Se empieza a adjudicar desde el alumno nº ", razon$p,
                   " de la lista alfabética.")
          ),

          div(class = "razonamiento-step",
            tags$strong("Paso 5 · Lista alfabética del grupo"),
            tags$p(
              style = "font-size:12px; margin-bottom:6px;",
              paste0("🟡 = inicio del sorteo (nº ", razon$p, ")  |  🔵 = alumno seleccionado")
            ),
            div(class = "lista-alfa",
              lapply(seq_along(razon$lista_alfa), function(i) {
                nombre_i   <- razon$lista_alfa[i]
                es_inicio  <- i == razon$p
                es_buscado <- nombre_i == fila$Nombre
                clase <- if (es_inicio && es_buscado) "alfa-item buscado"
                         else if (es_inicio)          "alfa-item inicio"
                         else if (es_buscado)         "alfa-item buscado"
                         else                         "alfa-item"
                icono <- if (es_inicio && es_buscado) "🟡🔵"
                         else if (es_inicio)          "🟡"
                         else if (es_buscado)         "🔵"
                         else                         ""
                tags$div(class = clase, paste0(i, ". ", icono, " ", nombre_i))
              })
            )
          ),

          div(class = "razonamiento-step",
            tags$strong("Paso 6 · Orden final tras rotación"),
            paste0(fila$Nombre, " ocupa el puesto ",
                   razon$n - which(razon$lista_alfa == fila$Nombre) + 1, " ... "),
            tags$em(
              style = "color:#1a3a5c;",
              paste0("Dentro del grupo ocupa la posición ", fila$Orden_grupo,
                     " → posición global: ", fila$Posicion, ".")
            )
          )
        )
      } else {
        div(class = "razonamiento-step",
          tags$strong("Sin empate"),
          "Este alumno no comparte puntuación con nadie, por lo que su posición se determina directamente."
        )
      },

      div(
        style = paste0(
          "background:", if (fila$Posicion <= corte) "#2d7d46" else "#b03030",
          "; color:white; padding:14px 18px; border-radius:4px; margin-top:8px;"
        ),
        tags$strong(
          style = "font-size:15px;",
          if (fila$Posicion <= corte)
            paste0("✓ ", fila$Nombre, " entra en el top ", corte,
                   " (posición ", fila$Posicion, ")")
          else
            paste0("✗ ", fila$Nombre, " no entra. Queda en la posición ", fila$Posicion,
                   " (corte en ", corte, ")")
        )
      )
    )
  })

  output$tabla_resultado <- renderDT({
    res <- datos()
    req(res)

    buscar <- str_trim(input$nombre_buscar)
    corte  <- input$corte
    df_show <- res$resultado %>%
      mutate(Estado = ifelse(Posicion <= corte, "✓ Entra", "✗ No entra")) %>%
      select(Posicion, Nombre, Puntuacion, Estado)

    datatable(
      df_show,
      options = list(
        pageLength = 20,
        dom = "ftp",
        order = list(list(0, "asc")),
        rowCallback = JS(sprintf("
          function(row, data, index) {
            var pos = parseInt(data[0]);
            var corte = %d;
            var buscar = '%s';
            if (pos === corte + 1) {
              $(row).css('border-top', '3px solid #b03030');
            }
            if (pos > corte) {
              $(row).css('color', '#aaa');
            }
            if (buscar !== '' && data[1].toLowerCase().indexOf(buscar.toLowerCase()) >= 0) {
              $(row).css({'background-color': '#fff8e6', 'font-weight': '600'});
            }
          }
        ", corte, buscar))
      ),
      rownames = FALSE,
      class = "stripe hover"
    ) %>%
      formatStyle("Estado",
        color = styleEqual(c("✓ Entra", "✗ No entra"), c("#2d7d46", "#b03030")),
        fontWeight = "bold"
      )
  })
}
