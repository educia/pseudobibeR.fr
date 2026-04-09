# features_lexical_complexity.R
# Lexical complexity and nominalization features for Spanish (f_60–f_64)
#
# EQUIVALENCIAS CON BIBER (1988):
#   f_60  nominalization          -> sustantivos en sufijos de nominalización
#   f_61  type/token ratio (TTR) -> diversidad léxica (tipos / tokens)
#   f_62  mean word length        -> longitud media de tokens léxicos
#   f_63  -ly adverbs             -> adverbios en -mente (derivados productivos)
#   f_64  words >= 6 letters      -> proporción de palabras "largas"
#
# NOTA METODOLÓGICA — normalización:
#   Biber normaliza los rasgos por 1.000 palabras para hacerlos
#   comparables entre textos de distinta longitud. Esta función devuelve
#   TANTO los conteos brutos COMO las tasas normalizadas (sufijo _rate)
#   para que el usuario pueda elegir. Las tasas se calculan dividiendo
#   entre el número de tokens léxicos del documento.
#
#   Tokens léxicos = NOUN + VERB + ADJ + ADV (UPOS).
#   Tokens totales = todos los tokens excepto puntuación (PUNCT) y espacios.
#
# NOTA — TTR y longitud media:
#   El TTR clásico es sensible a la longitud del texto. Para corpus de
#   tamaños desiguales conviene complementar con MATTR o MTLD (paquete
#   `koRpus`), pero esa lógica pertenece al flujo de análisis, no aquí.
#   Devolvemos el TTR simple para mantener la fidelidad a Biber (1988).

# ─────────────────────────────────────────────────────────────────────────────
# 0.  Helper — vectores léxicos
# ─────────────────────────────────────────────────────────────────────────────

# UPOS que se consideran "tokens léxicos" para normalización y TTR
LEXICAL_UPOS <- c("NOUN", "VERB", "ADJ", "ADV", "PROPN")

# UPOS que se excluyen del conteo de tokens totales
PUNCT_UPOS <- c("PUNCT", "SYM", "SPACE", "X")


# ─────────────────────────────────────────────────────────────────────────────
# 1.  block_lexical_complexity_es
# ─────────────────────────────────────────────────────────────────────────────

#' Lexical complexity and nominalization features (Spanish)
#'
#' Computes Biber (1988) features f_60 to f_64 for Spanish:
#' nominalizations, type-token ratio, mean word length,
#' -mente adverbs, and long words (>= 6 letters).
#'
#' @param tokens Annotated token data frame (UD format). Must contain
#'   columns: doc_id, token, lemma, pos (UPOS), feats.
#' @param doc_ids One-column data frame with column `doc_id`.
#' @param nominalization_suffixes Character vector of suffix strings
#'   (lower-case, without leading dot/caret). Example: c("cion","ción",...)
#' @param nominalization_stoplist Character vector of lexicalized nouns to
#'   exclude even if they match a suffix.
#' @param mente_stoplist Character vector of -mente adverbs to exclude from
#'   f_63 (highly lexicalized items that are not productive derivations).
#' @return Data frame: one row per doc, columns:
#'   n_tokens, n_lex_tokens,
#'   f_60_nominalization, f_60_nominalization_rate,
#'   f_61_ttr,
#'   f_62_mean_word_length,
#'   f_63_mente_adverbs, f_63_mente_adverbs_rate,
#'   f_64_long_words, f_64_long_words_rate
#' @keywords internal
block_lexical_complexity_es <- function(
    tokens,
    doc_ids,
    nominalization_suffixes,
    nominalization_stoplist,
    mente_stoplist = character(0)
) {

  # ── Base: filtrar puntuación ─────────────────────────────────────────────
  toks <- tokens %>%
    dplyr::filter(!.data$pos %in% PUNCT_UPOS)

  # ── Tokens léxicos (para normalización y TTR) ───────────────────────────
  lex_toks <- toks %>%
    dplyr::filter(.data$pos %in% LEXICAL_UPOS)

  # ── Conteos base por documento ───────────────────────────────────────────
  doc_n <- toks %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::summarise(
      n_tokens     = dplyr::n(),
      .groups = "drop"
    )

  doc_n_lex <- lex_toks %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::summarise(
      n_lex_tokens = dplyr::n(),
      .groups = "drop"
    )

  # ── f_60  Nominalizaciones ────────────────────────────────────────────────
  # Algoritmo:
  #   1. Seleccionar tokens NOUN (o PROPN en casos excepcionales).
  #   2. Convertir form a minúsculas.
  #   3. Aplicar regex: endsWith con sufijos de nominalización.
  #   4. Excluir items en nominalization_stoplist.
  #
  # El regex construye un patrón del tipo:
  #   "(cion|ción|ciones|...|ismo|ismos)$"
  # aplicado sobre la forma en minúsculas del token.

  suffix_pattern <- paste0(
    "(",
    paste(nominalization_suffixes, collapse = "|"),
    ")$"
  )

  f60 <- toks %>%
    dplyr::filter(.data$pos == "NOUN") %>%
    dplyr::mutate(
      token_lower = stringr::str_to_lower(.data$token)
    ) %>%
    dplyr::filter(
      stringr::str_detect(.data$token_lower, suffix_pattern),
      !.data$token_lower %in% nominalization_stoplist,
      !.data$lemma       %in% nominalization_stoplist
    ) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id,
                    .data$token_id_int) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_60_nominalization = "n")

  # ── f_61  Type-Token Ratio (TTR) ──────────────────────────────────────────
  # TTR = n tipos léxicos únicos / n tokens léxicos totales.
  # Se calcula sobre lemmas en minúsculas de tokens léxicos.
  ttr_tbl <- lex_toks %>%
    dplyr::mutate(
      lemma_lower = stringr::str_to_lower(.data$lemma)
    ) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::summarise(
      n_types   = dplyr::n_distinct(.data$lemma_lower),
      n_lex_tok = dplyr::n(),
      .groups   = "drop"
    ) %>%
    dplyr::mutate(
      f_61_ttr = dplyr::if_else(
        .data$n_lex_tok > 0,
        round(.data$n_types / .data$n_lex_tok, 4),
        NA_real_
      )
    ) %>%
    dplyr::select("doc_id", "f_61_ttr")

  # ── f_62  Longitud media de palabra ──────────────────────────────────────
  # Sobre tokens léxicos; se mide en nchar() de la forma superficial.
  # Se excluyen tokens de un solo carácter (artículos, preposiciones
  # monosilábicas que quedaron en UPOS léxico por error de parseo).
  word_len_tbl <- lex_toks %>%
    dplyr::mutate(
      tok_len = nchar(as.character(.data$token))
    ) %>%
    dplyr::filter(.data$tok_len >= 2) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::summarise(
      f_62_mean_word_length = round(mean(.data$tok_len, na.rm = TRUE), 3),
      .groups = "drop"
    )

  # ── f_63  Adverbios en -mente ─────────────────────────────────────────────
  # Equivalente de Biber's -ly adverbs.
  # Condiciones:
  #   1. UPOS = ADV
  #   2. token (en minúsculas) termina en "-mente"
  #   3. No está en mente_stoplist
  # Justificación de la stoplist: adverbios muy frecuentes como
  # "actualmente", "anteriormente", "finalmente" están tan lexicalizados
  # que no aportan información de derivación productiva; su inclusión
  # inflaría f_63 en textos expositivos de manera no diferencial.

  f63 <- toks %>%
    dplyr::filter(.data$pos == "ADV") %>%
    dplyr::mutate(
      token_lower = stringr::str_to_lower(.data$token)
    ) %>%
    dplyr::filter(
      stringr::str_ends(.data$token_lower, "mente"),
      !.data$token_lower %in% mente_stoplist
    ) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id,
                    .data$token_id_int) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_63_mente_adverbs = "n")

  # ── f_64  Palabras largas (>= 6 caracteres) ───────────────────────────────
  # Biber (1988) usa >= 6 letras sobre tokens ortográficos.
  # Aplicamos sobre tokens léxicos en minúsculas.
  f64 <- lex_toks %>%
    dplyr::mutate(
      tok_len = nchar(as.character(.data$token))
    ) %>%
    dplyr::filter(.data$tok_len >= 6) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id,
                    .data$token_id_int) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_64_long_words = "n")

  # ── Ensamblar + tasas normalizadas ────────────────────────────────────────
  doc_ids %>%
    dplyr::left_join(doc_n,      by = "doc_id") %>%
    dplyr::left_join(doc_n_lex,  by = "doc_id") %>%
    dplyr::left_join(f60,        by = "doc_id") %>%
    dplyr::left_join(ttr_tbl,    by = "doc_id") %>%
    dplyr::left_join(word_len_tbl, by = "doc_id") %>%
    dplyr::left_join(f63,        by = "doc_id") %>%
    dplyr::left_join(f64,        by = "doc_id") %>%
    dplyr::mutate(
      n_tokens              = dplyr::coalesce(.data$n_tokens,     0L),
      n_lex_tokens          = dplyr::coalesce(.data$n_lex_tokens, 0L),
      f_60_nominalization   = dplyr::coalesce(.data$f_60_nominalization, 0L),
      f_63_mente_adverbs    = dplyr::coalesce(.data$f_63_mente_adverbs,  0L),
      f_64_long_words       = dplyr::coalesce(.data$f_64_long_words,     0L),
      # Tasas por 1000 tokens léxicos
      f_60_nominalization_rate = dplyr::if_else(
        .data$n_lex_tokens > 0,
        round(.data$f_60_nominalization / .data$n_lex_tokens * 1000, 3),
        NA_real_
      ),
      f_63_mente_adverbs_rate = dplyr::if_else(
        .data$n_lex_tokens > 0,
        round(.data$f_63_mente_adverbs / .data$n_lex_tokens * 1000, 3),
        NA_real_
      ),
      f_64_long_words_rate = dplyr::if_else(
        .data$n_lex_tokens > 0,
        round(.data$f_64_long_words / .data$n_lex_tokens * 1000, 3),
        NA_real_
      )
    )
}
