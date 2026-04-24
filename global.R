library(shiny)
library(dplyr)
library(stringr)
library(DT)

SORTEO_OFICIAL <- 0.1283

calcular_orden_grupo <- function(nombres_alfa, sorteo) {
  n <- length(nombres_alfa)
  p <- floor(sorteo * n + 1)
  indices <- ((seq_len(n) - 1 + (p - 1)) %% n) + 1
  nombres_alfa[indices]
}

procesar_lista <- function(df, sorteo) {
  df <- df %>% arrange(desc(Puntuacion))

  grupos <- df %>%
    group_by(Puntuacion) %>%
    summarise(n = n(), .groups = "drop") %>%
    arrange(desc(Puntuacion))

  resultado <- data.frame()
  razonamientos <- list()
  pos_global <- 0

  for (punt in grupos$Puntuacion) {
    grupo_df <- df %>%
      filter(Puntuacion == punt) %>%
      arrange(Nombre)

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
      producto <- sorteo * n
      suma <- producto + 1
      p <- floor(suma)

      calculo_str <- sprintf(
        "%.4f × %d = %.4f → + 1 = %.4f → parte entera = %d",
        sorteo, n, producto, suma, p
      )

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
  }

  list(resultado = resultado, razonamientos = razonamientos)
}
