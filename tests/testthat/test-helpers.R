library(testthat)
library(dplyr)
library(stringr)

source("../../global.R")

# ── calcular_orden_grupo ──────────────────────────────────────────────────────

test_that("preserves all names without duplicates", {
  nombres <- c("Ana", "Bruno", "Carla", "David", "Elena")
  resultado <- calcular_orden_grupo(nombres, 0.1283)
  expect_equal(sort(resultado), sort(nombres))
  expect_equal(length(resultado), length(nombres))
})

test_that("single element returns itself", {
  expect_equal(calcular_orden_grupo("Solo", 0.1283), "Solo")
})

test_that("starting position is floor(sorteo * n + 1)", {
  nombres <- letters[1:10]
  sorteo <- 0.1283
  p_expected <- floor(sorteo * 10 + 1)  # floor(2.283) = 2
  resultado <- calcular_orden_grupo(nombres, sorteo)
  expect_equal(resultado[1], nombres[p_expected])
})

test_that("circular rotation wraps correctly", {
  # p = floor(0.5 * 4 + 1) = floor(3) = 3 → starts at index 3
  nombres <- c("A", "B", "C", "D")
  resultado <- calcular_orden_grupo(nombres, 0.5)
  p <- floor(0.5 * 4 + 1)
  expected <- nombres[((seq_len(4) - 1 + (p - 1)) %% 4) + 1]
  expect_equal(resultado, expected)
})

test_that("sorteo=0 always starts at position 1", {
  nombres <- c("Ana", "Bruno", "Carla")
  resultado <- calcular_orden_grupo(nombres, 0)
  expect_equal(resultado[1], nombres[1])
})

test_that("known output matches for BOCM sorteo with 26 names", {
  nombres <- paste0("Alumno_", sprintf("%02d", 1:26))
  sorteo <- 0.1283
  p <- floor(sorteo * 26 + 1)  # floor(4.3358) = 4
  resultado <- calcular_orden_grupo(nombres, sorteo)
  expect_equal(resultado[1], nombres[p])
  expect_equal(resultado[length(resultado)], nombres[p - 1])
})

# ── procesar_lista ────────────────────────────────────────────────────────────

make_df <- function(nombres, puntuaciones) {
  data.frame(Nombre = nombres, Puntuacion = puntuaciones, stringsAsFactors = FALSE)
}

test_that("positions are sequential from 1 to n", {
  df <- make_df(c("Ana", "Bruno", "Carla"), c(10, 8, 6))
  res <- procesar_lista(df, 0.1283)
  expect_equal(sort(res$resultado$Posicion), seq_len(3))
})

test_that("result has required columns", {
  df <- make_df(c("Ana", "Bruno"), c(10, 8))
  res <- procesar_lista(df, 0.1283)
  expect_true(all(c("Nombre", "Puntuacion", "Posicion", "Orden_grupo") %in%
                    names(res$resultado)))
})

test_that("no ties: scores are ordered descending by position", {
  df <- make_df(c("Carla", "Ana", "Bruno"), c(5, 15, 10))
  res <- procesar_lista(df, 0.1283)
  punts_ordenadas <- res$resultado$Puntuacion
  expect_equal(punts_ordenadas, sort(punts_ordenadas, decreasing = TRUE))
})

test_that("single student gets Posicion=1 and Orden_grupo=1", {
  df <- make_df("Solo", 42)
  res <- procesar_lista(df, 0.1283)
  expect_equal(res$resultado$Posicion, 1)
  expect_equal(res$resultado$Orden_grupo, 1)
})

test_that("tied students all appear in resultado", {
  df <- make_df(c("Ana", "Bruno", "Carla"), c(10, 10, 10))
  res <- procesar_lista(df, 0.1283)
  expect_equal(nrow(res$resultado), 3)
  expect_setequal(res$resultado$Nombre, c("Ana", "Bruno", "Carla"))
})

test_that("razonamientos has entry for each distinct score", {
  df <- make_df(c("Ana", "Bruno", "Carla", "David"), c(10, 10, 8, 5))
  res <- procesar_lista(df, 0.1283)
  expect_true("10" %in% names(res$razonamientos))
  expect_true("8" %in% names(res$razonamientos))
  expect_true("5" %in% names(res$razonamientos))
})

test_that("razonamiento for tied group records correct n and p", {
  df <- make_df(c("Ana", "Bruno", "Carla"), c(10, 10, 10))
  sorteo <- 0.1283
  res <- procesar_lista(df, sorteo)
  razon <- res$razonamientos[["10"]]
  expect_equal(razon$n, 3)
  expect_equal(razon$p, floor(sorteo * 3 + 1))
})

test_that("razonamiento for lone student has calculo='Sin empate'", {
  df <- make_df("Solo", 99)
  res <- procesar_lista(df, 0.1283)
  expect_equal(res$razonamientos[["99"]]$calculo, "Sin empate")
})

test_that("changing sorteo changes order within tied group", {
  df <- make_df(c("Ana", "Bruno", "Carla", "David"), c(10, 10, 10, 10))
  res1 <- procesar_lista(df, 0.1)
  res2 <- procesar_lista(df, 0.9)
  # Different sorteo values should give different starting positions
  p1 <- floor(0.1 * 4 + 1)
  p2 <- floor(0.9 * 4 + 1)
  if (p1 != p2) {
    expect_false(identical(res1$resultado$Nombre, res2$resultado$Nombre))
  }
})

test_that("mixed scores: students above cutoff all have Posicion <= corte", {
  df <- make_df(
    c("Ana", "Bruno", "Carla", "David", "Elena"),
    c(20, 15, 15, 10, 5)
  )
  res <- procesar_lista(df, 0.1283)
  df_res <- res$resultado
  # Student with score 20 should always be position 1
  expect_equal(df_res$Posicion[df_res$Nombre == "Ana"], 1)
})

# ── CSV column detection ──────────────────────────────────────────────────────

test_that("column detection matches 'Nombre' and 'Puntuacion' variants", {
  detect_cols <- function(col_names) {
    col_nombre <- col_names[str_detect(tolower(col_names), "nombre|name|alumno")]
    col_punt   <- col_names[str_detect(tolower(col_names), "punt|score|nota")]
    list(nombre = col_nombre, punt = col_punt)
  }

  cols1 <- detect_cols(c("Nombre", "Puntuacion"))
  expect_length(cols1$nombre, 1)
  expect_length(cols1$punt, 1)

  cols2 <- detect_cols(c("name", "score"))
  expect_length(cols2$nombre, 1)
  expect_length(cols2$punt, 1)

  cols3 <- detect_cols(c("alumno", "nota"))
  expect_length(cols3$nombre, 1)
  expect_length(cols3$punt, 1)

  cols_bad <- detect_cols(c("id", "valor"))
  expect_length(cols_bad$nombre, 0)
  expect_length(cols_bad$punt, 0)
})
