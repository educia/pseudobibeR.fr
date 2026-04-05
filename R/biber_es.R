#' Biber features para textos en español (udpipe)
#'
#' Extrae los 67 rasgos de Biber adaptados al español a partir de la
#' salida de `udpipe::udpipe_annotate()` usando el modelo UD `spanish-gsd`.
#'
#' @param tokens Objeto devuelto por `udpipe::udpipe_annotate()`
#' @param measure Medida de TTR: "MATTR", "TTR", "CTTR", "MSTTR" o "none"
#' @param normalize Si TRUE (por defecto), normaliza los recuentos a
#'   frecuencia por 1.000 tokens
#'
#' @return Un data.frame con una fila por documento y columnas f_01–f_67
#'   más las métricas léxicas (f_43, f_44)
#' @export
biber_es <- function(tokens,
                     measure   = c("MATTR", "TTR", "CTTR", "MSTTR", "none"),
                     normalize = TRUE) {

  # Requiere udpipe, igual que biber.udpipe_connlu()[cite:47]
  if (!requireNamespace("udpipe", quietly = TRUE)) {
    stop("El paquete 'udpipe' debe estar instalado para usar biber_es().\n",
         "Instálalo con: install.packages('udpipe')",
         call. = FALSE)
  }

  if (is.null(tokens)) {
    stop("'tokens' no puede ser NULL. Pasa el resultado de udpipe_annotate().",
         call. = FALSE)
  }

  # Conversión al formato interno tipo spacyr_parsed,
  # copiando la lógica de biber.udpipe_connlu()[cite:47]
  udpipe_tks <- as.data.frame(tokens, stringsAsFactors = FALSE)

  if (nrow(udpipe_tks) == 0) {
    stop("'tokens' está vacío (0 filas). Asegúrate de que udpipe_annotate() devolvió algo.",
         call. = FALSE)
  }

  required_cols <- c("doc_id", "token", "lemma", "upos", "xpos", "dep_rel")
  missing_cols  <- setdiff(required_cols, colnames(udpipe_tks))
  if (length(missing_cols) > 0) {
    stop("Faltan columnas requeridas en 'tokens': ",
         paste(missing_cols, collapse = ", " ),
         ". Asegúrate de llamar a udpipe_annotate() con tagger='default' y parser='default'.",
         call. = FALSE)
  }

  udpipe_tks <- udpipe_tks |>
    dplyr::select(
      "doc_id", "sentence_id", "token_id", "token", "lemma",
      "upos", "xpos", "feats", "head_token_id", "dep_rel"
    ) |>
    dplyr::rename(pos = "upos", tag = "xpos") |>
    dplyr::mutate(
      tag = dplyr::if_else(
        is.na(.data$tag) | .data$tag == "",
        .data$pos,
        .data$tag
      )
    )

  # La clase "spacyr_parsed" es la que espera parse_biber_features()[cite:47]
  udpipe_tks <- structure(
    udpipe_tks,
    class = c("spacyr_parsed", "data.frame")
  )

  measure <- match.arg(measure)

  # Aquí fijamos language = "es" para usar todos los bloques *_es[cite:48]
  parse_biber_features(
    tokens    = udpipe_tks,
    measure   = measure,
    normalize = normalize,
    engine    = "udpipe",
    language  = "es"
  )
}
