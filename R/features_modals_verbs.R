# Adjective, preposition, adverb, verb class, and modal features for Spanish
#
# NOTA LINGÜÍSTICA — modales españoles:
#
#   A diferencia del francés (pouvoir, devoir, vouloir como auxiliares
#   monoléxicos), en español los modales son PERÍFRASIS VERBALES:
#
#     f_52  Posibilidad:  poder + INF, caber + INF
#     f_53  Necesidad:    deber (de) + INF, tener que + INF,
#                         haber de + INF, haber que + INF
#     f_54  Predictivo:   futuro sintético (dict), ir a + INF (parse_functions)
#
#   Para evitar falsos positivos ("el poder", "el deber"), exigimos:
#     - POS AUX o VERB
#     - dep_rel que NO sea nsubj/obj/root con morph_verbform != Fin
#     - al menos un dependiente con morph_verbform = Inf
#       enlazado por xcomp / ccomp / advcl / aux / obj (según parser)
#
#   tener_que y haber_de son multiword y ya se manejan por el diccionario
#   via quanteda::tokens_compound en parse_functions. Aquí solo contamos
#   por el verbo-cabeza para el caso en que el compound no se detecte.

# ─────────────────────────────────────────────────────────────────────────────
# Helper interno: verbo modal con infinitivo dependiente
# ─────────────────────────────────────────────────────────────────────────────

# Tabla de infinitivos que cuelgan de algún token
# Dep_rel válidas: xcomp, ccomp, advcl, aux, obj (spaCy a veces usa obj)
count_modal_periphrasis <- function(tokens, lemmas) {
  if (length(lemmas) == 0)
    return(tibble::tibble(doc_id = character(), n = integer()))

  # Dependientes infinitivos
  inf_deps <- tokens %>%
    dplyr::filter(
      .data$morph_verbform == "Inf",
      .data$pos %in% c("VERB", "AUX"),
      stringr::str_detect(
        dplyr::coalesce(.data$dep_rel, ""),
        "^(xcomp|ccomp|advcl|aux|obj)"
      ),
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::transmute(.data$doc_id, .data$sentence_id,
                     head_token_id_int = .data$head_token_id_int,
                     has_inf_dep = TRUE) %>%
    dplyr::distinct()

  tokens %>%
    dplyr::filter(
      .data$lemma %in% lemmas,
      .data$pos %in% c("VERB", "AUX"),
      # excluir cuando es nominalización (el poder, el deber)
      !stringr::str_detect(
        dplyr::coalesce(.data$dep_rel, ""),
        "^(nsubj|obj|iobj|nmod|det|appos)$"
      )
    ) %>%
    dplyr::left_join(
      inf_deps,
      by = c("doc_id", "sentence_id", "token_id_int" = "head_token_id_int")
    ) %>%
    dplyr::filter(!is.na(.data$has_inf_dep)) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$token_id_int) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally()
}

# ─────────────────────────────────────────────────────────────────────────────
# block_adj_prep_adv_es
# ─────────────────────────────────────────────────────────────────────────────

#' Extract adjective, preposition, and adverb features (Spanish)
#'
#' @param tokens Annotated token data frame
#' @param doc_ids Document IDs
#' @param dict_lookup Dictionary lookup
#' @param word_lists_lookup Word lists lookup
#' @param negation_adverbs Vector of negation adverbs
#' @return Data frame with f_39_prepositions through f_42_adverbs
#' @keywords internal
block_adj_prep_adv_es <- function(tokens, doc_ids, dict_lookup,
                                   word_lists_lookup, negation_adverbs) {

  # f_39  Preposiciones (dep_rel = case | fixed)
  f39 <- tokens %>%
    dplyr::filter(
      .data$pos == "ADP",
      dplyr::coalesce(.data$dep_rel, "") %in% c("case", "fixed")
    ) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_39_prepositions = "n")

  # f_40  Adjetivo ATRIBUTIVO (prenominal o coordinado)
  #   Usamos dep_rel = amod (modificador adjetival de sustantivo) para
  #   evitar cruces entre oraciones con dplyr::lead.
  #   Fallback: si el parser no asigna amod, usamos lead con group_by
  #   restringido a sentence_id.
  f40_by_dep <- tokens %>%
    dplyr::filter(
      .data$pos == "ADJ",
      .data$dep_rel == "amod",
      !stringr::str_detect(.data$token, "-")
    ) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_40_adj_attr = "n")

  # Para parsers que no usen amod de manera consistente, guardamos
  # conteo por lead dentro de oración como fallback.
  f40_by_lead <- tokens %>%
    dplyr::group_by(.data$doc_id, .data$sentence_id) %>%
    dplyr::filter(
      .data$pos == "ADJ",
      !stringr::str_detect(.data$token, "-"),
      (
        dplyr::lead(.data$pos) == "NOUN" |
          dplyr::lead(.data$pos) == "ADJ" |
          (dplyr::lead(.data$token) == "," &
             dplyr::lead(.data$pos, 2) == "ADJ")
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_40_adj_attr = "n")

  # Usar amod si disponible (más preciso), fallback a lead
  has_amod <- any(
    tokens$dep_rel == "amod" & tokens$pos == "ADJ", na.rm = TRUE
  )
  f40 <- if (has_amod) f40_by_dep else f40_by_lead

  # f_41  Adjetivo PREDICATIVO (verbo copulativo + ADJ)
  linking_verbs <- get_word_list(word_lists_lookup, "linking_matchlist")

  f41 <- tokens %>%
    dplyr::group_by(.data$doc_id, .data$sentence_id) %>%
    dplyr::filter(
      .data$pos == "ADJ",
      .data$dep_rel %in% c("xcomp", "acomp", "cop") |
        (dplyr::lag(.data$pos %in% c("VERB", "AUX")) &
           dplyr::lag(.data$lemma %in% linking_verbs) &
           !dplyr::lead(.data$pos %in% c("NOUN", "ADJ", "ADV")))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_41_adj_pred = "n")

  # f_42  Adverbios (excluye los clasificados en f_46-f_50 y negación)
  adverb_exclusions <- unique(c(
    dictionary_to_lemmas(dict_lookup, "f_46_downtoners"),
    dictionary_to_lemmas(dict_lookup, "f_47_hedges"),
    dictionary_to_lemmas(dict_lookup, "f_48_amplifiers"),
    dictionary_to_lemmas(dict_lookup, "f_49_emphatics"),
    dictionary_to_lemmas(dict_lookup, "f_50_discourse_particles"),
    negation_adverbs
  ))

  f42 <- tokens %>%
    dplyr::filter(
      .data$pos == "ADV",
      !.data$lemma %in% adverb_exclusions
    ) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_42_adverbs = "n")

  doc_ids %>%
    dplyr::left_join(f39, by = "doc_id") %>%
    dplyr::left_join(f40, by = "doc_id") %>%
    dplyr::left_join(f41, by = "doc_id") %>%
    dplyr::left_join(f42, by = "doc_id") %>%
    dplyr::mutate(
      dplyr::across(-dplyr::any_of("doc_id"), ~ dplyr::coalesce(., 0L))
    )
}

# ─────────────────────────────────────────────────────────────────────────────
# block_specialized_verbs_es
# ─────────────────────────────────────────────────────────────────────────────

#' Extract specialized verb class features (Spanish)
#'
#' Counts f_55 (public verbs), f_56 (private verbs), f_57 (suasive verbs),
#' f_58 (seem-type verbs) using lema lists from dict.yaml.
#'
#' @param tokens Annotated token data frame
#' @param doc_ids Document IDs
#' @param dict_lookup Dictionary lookup
#' @return Data frame with f_55_verb_public through f_58_verb_seem
#' @keywords internal
block_specialized_verbs_es <- function(tokens, doc_ids, dict_lookup) {

  count_verb_class <- function(lemmas, col_name) {
    tokens %>%
      dplyr::filter(.data$lemma %in% lemmas,
                    .data$pos %in% c("VERB", "AUX")) %>%
      dplyr::group_by(.data$doc_id) %>%
      dplyr::tally() %>%
      dplyr::rename(!!col_name := "n")
  }

  f55 <- count_verb_class(dictionary_to_lemmas(dict_lookup, "f_55_verb_public"),
                          "f_55_verb_public")
  f56 <- count_verb_class(dictionary_to_lemmas(dict_lookup, "f_56_verb_private"),
                          "f_56_verb_private")
  f57 <- count_verb_class(dictionary_to_lemmas(dict_lookup, "f_57_verb_suasive"),
                          "f_57_verb_suasive")
  f58 <- count_verb_class(dictionary_to_lemmas(dict_lookup, "f_58_verb_seem"),
                          "f_58_verb_seem")

  doc_ids %>%
    dplyr::left_join(f55, by = "doc_id") %>%
    dplyr::left_join(f56, by = "doc_id") %>%
    dplyr::left_join(f57, by = "doc_id") %>%
    dplyr::left_join(f58, by = "doc_id") %>%
    dplyr::mutate(
      dplyr::across(-dplyr::any_of("doc_id"), ~ dplyr::coalesce(., 0L))
    )
}

# ─────────────────────────────────────────────────────────────────────────────
# block_modals_es
# ─────────────────────────────────────────────────────────────────────────────

#' Extract modal verb features (Spanish)
#'
#' Detecta perífrasis modales españolas exigiendo que el verbo modal
#' tenga al menos un dependiente con VerbForm=Inf, para evitar contar
#' nominalizaciones ("el poder", "el deber") y usos plenos no modales.
#'
#' f_52  Posibilidad: poder, caber (+ INF)
#' f_53  Necesidad:   deber, tener_que, haber_de, haber_que (+ INF)
#' f_54  Predictivo:  futuro sintético (via dict) + ir_a+INF (parse_functions)
#'        — NOTE: f_54 via ir_a ya se computa en parse_functions.R y se
#'          fusiona allí. Aquí sólo contamos el futuro sintético del dict.
#'
#' @param tokens Annotated token data frame with context columns
#' @param doc_ids Document IDs
#' @param dict_lookup Dictionary lookup
#' @return Data frame with f_52_modal_possibility through f_54_modal_predictive
#' @keywords internal
block_modals_es <- function(tokens, doc_ids, dict_lookup) {

  # f_52  Posibilidad
  possibility_lemmas <- dictionary_to_lemmas(dict_lookup, "f_52_modal_possibility")
  f52 <- count_modal_periphrasis(tokens, possibility_lemmas) %>%
    dplyr::rename(f_52_modal_possibility = "n")

  # f_53  Necesidad
  #   deber + INF     (sin "de" = necesidad; con "de" = probabilidad epistmica)
  #   deber_de + INF  (probabilidad: algunos lo clasifican en f_52; aquí unido)
  #   tener_que, haber_de, haber_que ya vienen compuestos del dict
  necessity_lemmas <- dictionary_to_lemmas(dict_lookup, "f_53_modal_necessity")
  f53 <- count_modal_periphrasis(tokens, necessity_lemmas) %>%
    dplyr::rename(f_53_modal_necessity = "n")

  # f_54  Predictivo (solo futuro sintético del dict; ir_a se maneja en parse_functions)
  #   El futuro sintético (hablaré, hablarás…) viene del diccionario como
  #   tokens con Tense=Fut, VerbForm=Fin.  Contamos directamente por morph.
  f54 <- tokens %>%
    dplyr::filter(
      .data$pos %in% c("VERB", "AUX"),
      .data$morph_tense == "Fut",
      .data$morph_verbform == "Fin"
    ) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_54_modal_predictive = "n")

  doc_ids %>%
    dplyr::left_join(f52, by = "doc_id") %>%
    dplyr::left_join(f53, by = "doc_id") %>%
    dplyr::left_join(f54, by = "doc_id") %>%
    dplyr::mutate(
      dplyr::across(-dplyr::any_of("doc_id"), ~ dplyr::coalesce(., 0L))
    )
}
