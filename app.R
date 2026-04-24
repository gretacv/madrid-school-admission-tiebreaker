library(shiny)
library(dplyr)
library(stringr)
library(DT)

# ── Lógica de desempate ──────────────────────────────────────────────────────

sorteo <- 0.1283

calcular_orden_grupo <- function(nombres_alfa, sorteo) {
n <- length(nombres_alfa)
p <- floor(sorteo * n + 1)

# Rotación circular empezando en p

indices <- ((seq_len(n) - 1 + (p - 1)) %% n) + 1
nombres_alfa[indices]
}

procesar_lista <- function(df, sorteo) {

# Ordenar por puntuación descendente

df <- df %>% arrange(desc(Puntuacion))

# Identificar grupos empatados

grupos <- df %>%
group_by(Puntuacion) %>%
summarise(n = n(), .groups = “drop”) %>%
arrange(desc(Puntuacion))

resultado <- data.frame()
razonamientos <- list()
pos_global <- 0

for (punt in grupos$Puntuacion) {
grupo_df <- df %>%
filter(Puntuacion == punt) %>%
arrange(Nombre)  # orden alfabético

```
n <- nrow(grupo_df)

if (n == 1) {
  pos_global <- pos_global + 1
  fila <- grupo_df
  fila$Posicion <- pos_global
  fila$Orden_grupo <- 1
  resultado <- bind_rows(resultado, fila)
  razonamientos[[as.character(punt)]] <- list(
    puntuacion = punt,
    n = 1,
    p = NA,
    calculo = "Sin empate",
    orden = grupo_df$Nombre
  )
} else {
  # Calcular punto de inicio
  producto <- sorteo * n
  suma <- producto + 1
  p <- floor(suma)
  
  calculo_str <- sprintf(
    "%.4f × %d = %.4f → + 1 = %.4f → parte entera = %d",
    sorteo, n, producto, suma, p
  )
  
  # Orden rotado
  nombres_alfa <- grupo_df$Nombre
  orden_rotado <- calcular_orden_grupo(nombres_alfa, sorteo)
  
  for (i in seq_along(orden_rotado)) {
    pos_global <- pos_global + 1
    nombre_i <- orden_rotado[i]
    fila <- grupo_df %>% filter(Nombre == nombre_i)
    fila$Posicion <- pos_global
    fila$Orden_grupo <- i
    resultado <- bind_rows(resultado, fila)
  }
  
  razonamientos[[as.character(punt)]] <- list(
    puntuacion = punt,
    n = n,
    p = p,
    calculo = calculo_str,
    lista_alfa = nombres_alfa,
    orden = orden_rotado
  )
}
```

}

list(resultado = resultado, razonamientos = razonamientos)
}

# ── UI ───────────────────────────────────────────────────────────────────────

ui <- fluidPage(
tags$head(
tags$style(HTML(”
@import url(‘https://fonts.googleapis.com/css2?family=Playfair+Display:wght@600;700&family=Source+Sans+3:wght@400;600&display=swap’);

```
  body {
    background-color: #f7f4ef;
    font-family: 'Source Sans 3', sans-serif;
    color: #2c2c2c;
    margin: 0;
    padding: 0;
  }

  .header-bar {
    background-color: #1a3a5c;
    color: white;
    padding: 28px 40px 20px 40px;
    margin-bottom: 30px;
    border-bottom: 5px solid #c8a84b;
  }

  .header-bar h1 {
    font-family: 'Playfair Display', serif;
    font-size: 28px;
    margin: 0 0 4px 0;
    letter-spacing: 0.5px;
  }

  .header-bar p {
    font-size: 13px;
    margin: 0;
    opacity: 0.8;
    letter-spacing: 0.3px;
  }

  .sorteo-badge {
    display: inline-block;
    background-color: #c8a84b;
    color: #1a3a5c;
    font-family: 'Playfair Display', serif;
    font-weight: 700;
    font-size: 15px;
    padding: 6px 16px;
    border-radius: 3px;
    margin-top: 10px;
    letter-spacing: 1px;
  }

  .panel-custom {
    background: white;
    border: 1px solid #ddd8cf;
    border-radius: 6px;
    padding: 24px;
    margin-bottom: 20px;
    box-shadow: 0 2px 8px rgba(0,0,0,0.05);
  }

  .panel-custom h4 {
    font-family: 'Playfair Display', serif;
    color: #1a3a5c;
    font-size: 17px;
    margin-top: 0;
    margin-bottom: 16px;
    padding-bottom: 10px;
    border-bottom: 2px solid #c8a84b;
  }

  .resultado-box {
    background: #1a3a5c;
    color: white;
    border-radius: 6px;
    padding: 20px 24px;
    margin-bottom: 20px;
  }

  .resultado-box .nombre {
    font-family: 'Playfair Display', serif;
    font-size: 22px;
    margin-bottom: 8px;
  }

  .resultado-box .puesto {
    font-size: 48px;
    font-weight: 700;
    color: #c8a84b;
    line-height: 1;
    font-family: 'Playfair Display', serif;
  }

  .resultado-box .puesto-label {
    font-size: 12px;
    opacity: 0.7;
    text-transform: uppercase;
    letter-spacing: 1.5px;
    margin-top: 4px;
  }

  .entra-si {
    display: inline-block;
    background: #2d7d46;
    color: white;
    padding: 5px 14px;
    border-radius: 3px;
    font-size: 13px;
    font-weight: 600;
    letter-spacing: 0.5px;
    margin-top: 10px;
  }

  .entra-no {
    display: inline-block;
    background: #b03030;
    color: white;
    padding: 5px 14px;
    border-radius: 3px;
    font-size: 13px;
    font-weight: 600;
    letter-spacing: 0.5px;
    margin-top: 10px;
  }

  .razonamiento-step {
    background: #f7f4ef;
    border-left: 4px solid #c8a84b;
    padding: 12px 16px;
    margin-bottom: 10px;
    border-radius: 0 4px 4px 0;
    font-size: 14px;
    line-height: 1.6;
  }

  .razonamiento-step strong {
    color: #1a3a5c;
    display: block;
    margin-bottom: 4px;
    font-size: 12px;
    text-transform: uppercase;
    letter-spacing: 0.8px;
  }

  .formula-code {
    font-family: 'Courier New', monospace;
    background: #1a3a5c;
    color: #c8a84b;
    padding: 10px 14px;
    border-radius: 4px;
    font-size: 13px;
    margin: 8px 0;
    display: block;
  }

  .lista-alfa {
    display: flex;
    flex-wrap: wrap;
    gap: 6px;
    margin-top: 8px;
  }

  .alfa-item {
    background: #e8e4dc;
    border-radius: 3px;
    padding: 4px 10px;
    font-size: 12px;
    color: #2c2c2c;
  }

  .alfa-item.inicio {
    background: #c8a84b;
    color: #1a3a5c;
    font-weight: 700;
  }

  .alfa-item.buscado {
    background: #1a3a5c;
    color: white;
    font-weight: 700;
  }

  .corte-line {
    border: none;
    border-top: 2px dashed #b03030;
    margin: 4px 0;
    position: relative;
  }

  textarea.form-control {
    font-family: 'Courier New', monospace;
    font-size: 12px;
    border: 1px solid #ddd8cf;
    border-radius: 4px;
    background: #fdfcf9;
  }

  .btn-primary {
    background-color: #1a3a5c !important;
    border-color: #1a3a5c !important;
    font-family: 'Source Sans 3', sans-serif;
    font-size: 14px;
    letter-spacing: 0.5px;
    padding: 8px 24px;
    border-radius: 3px;
  }

  .btn-primary:hover {
    background-color: #c8a84b !important;
    border-color: #c8a84b !important;
    color: #1a3a5c !important;
  }

  .form-control:focus {
    border-color: #c8a84b;
    box-shadow: 0 0 0 2px rgba(200,168,75,0.2);
  }

  label {
    font-weight: 600;
    font-size: 13px;
    color: #1a3a5c;
    text-transform: uppercase;
    letter-spacing: 0.5px;
    margin-bottom: 6px;
  }

  .corte-info {
    font-size: 13px;
    color: #666;
    margin-top: 6px;
  }

  .dataTables_wrapper {
    font-size: 13px;
  }

  table.dataTable thead th {
    background-color: #1a3a5c;
    color: white;
    font-family: 'Source Sans 3', sans-serif;
    font-size: 12px;
    text-transform: uppercase;
    letter-spacing: 0.5px;
    border: none !important;
  }

  .highlight-row {
    background-color: #fff8e6 !important;
    font-weight: 600;
  }
"))
```

),

# Cabecera

div(class = “header-bar”,
h1(“🏫 Desempate Admisión Escolar — Comunidad de Madrid”),
p(“Resolución de 26 de noviembre de 2025 · Instrucción Novena · Curso 2026/2027”),
div(class = “sorteo-badge”, “Número de sorteo oficial: 0,1283”)
),

# Disclaimer

div(style = “background:#fff8e6; border-top: 4px solid #c8a84b; border-bottom: 1px solid #e0d8c8; padding: 14px 40px; margin-bottom: 10px;”,
tags$p(style = “margin:0; font-size:13px; color:#5a4a1a; line-height:1.6;”,
tags$strong(“⚠️ Aviso importante: “),
“Esta herramienta es una interpretación orientativa de la metodología de desempate recogida en la “,
tags$strong(“Resolución de 26 de noviembre de 2025 de la Viceconsejería de Política y Organización Educativa”),
“ (BOCM núm. 293, de 9 de diciembre de 2025) y en el “,
tags$strong(“Resultado del Sorteo Público de 8 de abril de 2026.”),
“ Los cálculos se realizan de forma automática a partir de los datos introducidos por el usuario y “,
tags$strong(“no constituyen en ningún caso una confirmación oficial”),
“ de que un alumno obtendrá o no plaza en un centro determinado. “,
“La adjudicación definitiva de plazas escolares corresponde exclusivamente a la “,
tags$strong(“Comunidad de Madrid a través del Sistema Integral de Gestión Educativa Raíces.”),
“ Ante cualquier duda, consulte directamente con el centro educativo o con la Dirección de Área Territorial correspondiente.”
)
),

div(style = “padding: 0 30px 30px 30px;”,

```
fluidRow(
  column(5,
    div(class = "panel-custom",
      h4("📋 Datos de entrada"),

      numericInput("sorteo_num", "Número del sorteo oficial:", value = 0.1283,
                   min = 0, max = 0.9999, step = 0.0001),

      numericInput("corte", "Posición de corte (último que entra):", value = 36,
                   min = 1, max = 500, step = 1),

      tags$label("Tabla de alumnos (CSV)"),
      tags$p(style = "font-size:11px; color:#888; margin-bottom:6px; text-transform:none; letter-spacing:0;",
        "Columnas requeridas: Nombre, Puntuacion (separadas por coma o punto y coma)"),
      textAreaInput("csv_input", NULL,
        value = "Nombre,Puntuacion
```

Aldana Vega Sofia,0
Bermejo Rios Carlos,0
Crespo Luna Marta,0
Delgado Fuentes Pedro,4
Espinosa Vargas Lucia,6
Fonseca Prado Miguel,8
Guerrero Salas Ana,12
Herrera Campos Tomas,12
Ibañez Molina Carmen,13
Jimenez Valls Marcos,13
Lago Pinto Elena,13
Moreno Bueno Alexia,13
Navarro Ruiz Daniel,13
Ortega Soto Vera,13
Pando Leal Bruno,13
Quintero Mas Nadia,13
Roca Blanco Sergio,13
Sierra Deza Paula,13
Tovar Nieto Ines,13
Urraca Peña Leo,13
Valdes Cano Mireia,13
Wamba Ortiz Gael,13
Xuarez Vela Carla,13
Yuste Duro Hector,13
Zabala Rios Celia,13
Alonso Vera Ruben,13
Bernal Checa Irene,13
Castro Mingo Diego,13
Duran Llop Noa,13
Esteban Poza Victor,13
Flores Ramos Julia,13
Giron Laso Mateo,13
Hidalgo Pons Blanca,13
Iturbe Sanz Ivan,13
Jarabo Mena Sara,15
Kepa Lara Unai,15
Lemos Diaz Aroa,16
Magan Torres Hugo,25
Nuñez Peral Leire,25
Olea Vasco Pau,25
Prieto Cano Alma,27
Quesada Rial Biel,28
Recio Mazo Laia,28
Solano Vega Nico,28
Tena Busto Lara,28
Uribe Cela Marc,28
Varela Pino Neus,28
Wenceslao Daza Iker,28
Ximenez Roda Ona,28
Yepes Caro Kike,35
Zapata Leal Noa,38
Arevalo Moya Luca,38
Benito Vidal Omar,72”,
rows = 10, width = “100%”),

```
      tags$label("Buscar alumno/a:"),
      textInput("nombre_buscar", NULL, placeholder = "Ej: Sofia"),

      actionButton("calcular", "Calcular", class = "btn btn-primary", width = "100%")
    )
  ),

  column(7,
    # Resultado del alumno buscado
    uiOutput("resultado_alumno"),

    # Razonamiento
    uiOutput("razonamiento_ui"),

    # Tabla completa
    div(class = "panel-custom",
      h4("📊 Lista completa ordenada"),
      DTOutput("tabla_resultado")
    )
  )
)
```

)
)

# ── Server ───────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

datos <- eventReactive(input$calcular, {
req(input$csv_input)

```
# Leer CSV (acepta coma o punto y coma)
sep <- if (grepl(";", input$csv_input)) ";" else ","
df <- tryCatch(
  read.csv(text = input$csv_input, sep = sep, stringsAsFactors = FALSE,
           strip.white = TRUE, encoding = "UTF-8"),
  error = function(e) NULL
)

if (is.null(df)) return(NULL)

# Normalizar nombres de columnas
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
```

})

output$resultado_alumno <- renderUI({
res <- datos()
if (is.null(res)) return(NULL)
buscar <- str_trim(input$nombre_buscar)
if (buscar == “”) return(NULL)

```
df_res <- res$resultado
# Búsqueda flexible (insensible a mayúsculas)
fila <- df_res %>%
  filter(str_detect(tolower(Nombre), tolower(buscar)))

if (nrow(fila) == 0) {
  return(div(class = "panel-custom",
    p(style = "color:#b03030;", paste0("⚠️ No se encontró ningún alumno con '", buscar, "' en su nombre."))))
}

fila <- fila[1, ]
corte <- input$corte
entra <- fila$Posicion <= corte

div(class = "resultado-box",
  div(class = "nombre", fila$Nombre),
  div(style = "display:flex; gap:32px; align-items:flex-start; margin-top:12px;",
    div(
      div(class = "puesto", fila$Posicion),
      div(class = "puesto-label", "Posición final")
    ),
    div(
      div(style = "font-size:13px; opacity:0.7; margin-bottom:4px;", "Puntuación"),
      div(style = "font-size:28px; font-weight:700;", fila$Puntuacion),
      div(
        if (entra)
          tags$span(class = "entra-si", paste0("✓ ENTRA (puesto ", fila$Posicion, " de ", corte, ")"))
        else
          tags$span(class = "entra-no", paste0("✗ NO ENTRA (puesto ", fila$Posicion, ", corte en ", corte, ")"))
      )
    )
  )
)
```

})

output$razonamiento_ui <- renderUI({
res <- datos()
if (is.null(res)) return(NULL)
buscar <- str_trim(input$nombre_buscar)
if (buscar == “”) return(NULL)

```
df_res <- res$resultado
fila <- df_res %>% filter(str_detect(tolower(Nombre), tolower(buscar)))
if (nrow(fila) == 0) return(NULL)
fila <- fila[1, ]

punt_str <- as.character(fila$Puntuacion)
razon <- res$razonamientos[[punt_str]]
corte <- input$corte

# Cuántas plazas hay antes de este grupo
plazas_antes <- df_res %>%
  filter(Puntuacion > fila$Puntuacion) %>%
  nrow()

plazas_grupo <- max(0, corte - plazas_antes)

div(class = "panel-custom",
  h4("🔍 Razonamiento paso a paso"),

  # Paso 1
  div(class = "razonamiento-step",
    tags$strong("Paso 1 · Puntuación del alumno/a"),
    paste0(fila$Nombre, " tiene ", fila$Puntuacion, " puntos.")
  ),

  # Paso 2
  div(class = "razonamiento-step",
    tags$strong("Paso 2 · Alumnos con mayor puntuación"),
    paste0(plazas_antes, " alumnos tienen más de ", fila$Puntuacion,
           " puntos y ocupan los primeros ", plazas_antes, " puestos.")
  ),

  if (razon$n > 1) {
    tagList(
      # Paso 3
      div(class = "razonamiento-step",
        tags$strong("Paso 3 · Grupo de empate"),
        paste0("Hay ", razon$n, " alumnos empatados a ", fila$Puntuacion, " puntos. ",
               "Quedan ", plazas_grupo, " plaza(s) disponibles para este grupo (", corte, " - ", plazas_antes, " = ", plazas_grupo, ").")
      ),

      # Paso 4
      div(class = "razonamiento-step",
        tags$strong("Paso 4 · Cálculo del punto de inicio (Instrucción 9ª BOCM)"),
        tags$code(class = "formula-code", razon$calculo),
        paste0("→ Se empieza a adjudicar desde el alumno nº ", razon$p, " de la lista alfabética.")
      ),

      # Paso 5
      div(class = "razonamiento-step",
        tags$strong("Paso 5 · Lista alfabética del grupo"),
        tags$p(style = "font-size:12px; margin-bottom:6px;",
          paste0("🟡 = inicio del sorteo (nº ", razon$p, ")  |  🔵 = alumno buscado")),
        div(class = "lista-alfa",
          lapply(seq_along(razon$lista_alfa), function(i) {
            nombre_i <- razon$lista_alfa[i]
            es_inicio  <- i == razon$p
            es_buscado <- str_detect(tolower(nombre_i), tolower(buscar))
            clase <- if (es_inicio && es_buscado) "alfa-item buscado"
                     else if (es_inicio) "alfa-item inicio"
                     else if (es_buscado) "alfa-item buscado"
                     else "alfa-item"
            icono <- if (es_inicio && es_buscado) "🟡🔵"
                     else if (es_inicio) "🟡"
                     else if (es_buscado) "🔵"
                     else ""
            tags$div(class = clase, paste0(i, ". ", icono, " ", nombre_i))
          })
        )
      ),

      # Paso 6
      div(class = "razonamiento-step",
        tags$strong("Paso 6 · Orden final tras rotación"),
        paste0(fila$Nombre, " ocupa el puesto ", razon$n - which(razon$lista_alfa == fila$Nombre) + 1,
               " ... "),
        tags$em(style = "color:#1a3a5c;",
          paste0("Dentro del grupo ocupa la posición ", fila$Orden_grupo,
                 " → posición global: ", fila$Posicion, "."))
      )
    )
  } else {
    div(class = "razonamiento-step",
      tags$strong("Sin empate"),
      "Este alumno no comparte puntuación con nadie, por lo que su posición se determina directamente."
    )
  },

  # Conclusión
  div(style = paste0(
        "background:", if (fila$Posicion <= corte) "#2d7d46" else "#b03030",
        "; color:white; padding:14px 18px; border-radius:4px; margin-top:8px;"),
    tags$strong(style = "font-size:15px;",
      if (fila$Posicion <= corte)
        paste0("✓ ", fila$Nombre, " entra en el top ", corte, " (posición ", fila$Posicion, ")")
      else
        paste0("✗ ", fila$Nombre, " no entra. Queda en la posición ", fila$Posicion,
               " (corte en ", corte, ")")
    )
  )
)
```

})

output$tabla_resultado <- renderDT({
res <- datos()
if (is.null(res)) return(NULL)

```
buscar <- str_trim(input$nombre_buscar)
corte  <- input$corte
df_show <- res$resultado %>%
  mutate(
    Estado = ifelse(Posicion <= corte, "✓ Entra", "✗ No entra")
  ) %>%
  select(Posicion, Nombre, Puntuacion, Estado)

# Índices para resaltar
idx_corte  <- corte
idx_buscar <- if (buscar != "") {
  which(str_detect(tolower(df_show$Nombre), tolower(buscar)))
} else integer(0)

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
```

})
}

shinyApp(ui, server)
