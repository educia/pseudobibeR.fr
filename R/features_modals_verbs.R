# features_modals_verbs.R
# Adjective, preposition, adverb, modal, and specialized-verb features (Spanish)
# f_39–f_42, f_52–f_58
#
# NOTA LINGÜÍSTICA — modales españoles:
#   El español no tiene auxiliares modales monoléxicos como el inglés.
#   Los modales son PERÍFRASIS VERBALES:
#     f_52  Posibilidad:  poder + INF, caber + INF
#     f_53  Necesidad:    deber + INF, tener_que + INF,
#                         haber_de + INF, haber_que + INF
#     f_54  Predictivo:   futuro sintético (Tense=Fut,VerbForm=Fin)
#                         + ir_a + INF (perífrase progresivo-futuro)
#
#   Para evitar falsos positivos ("el poder", "el deber" como sustantivos)
#   exigimos que el verbo modal tenga al menos un dependiente con
#   VerbForm=Inf enlazado por xcomp / ccomp / advcl / aux / obj.
#
# NOTA — extract_feat() y count_feature() se definen en
#   features_tense_pronouns.R y son visibles en el mismo namespace del paquete.

# ─────────────────────────────────────────────────────────────────────────────
# 0.  Helper: perífrasis modal (verbo-cabeza + infinitivo-dependiente)
# ─────────────────────────────────────────────────────────────────────────────

count_modal_periphrasis <- function(tokens, lemmas) {
  if (length(lemmas) == 0)
    return(tibble::tibble(doc_id = character(), n = integer()))

  modal_toks <- tokens %>%
    dplyr::filter(
      .data$lemma %in% lemmas,
      .data$pos %in% c("VERB", "AUX"),
      !stringr::str_detect(
        dplyr::coalesce(.data$dep_rel, ""),
        "^(nsubj|obj|iobj|nmod|det|appos)$"
      )
    )

  # Case 1: modal is the head, INF is a dependent (dep_rel = xcomp/ccomp/...)
  inf_deps <- tokens %>%
    dplyr::filter(
      .data$pos %in% c("VERB", "AUX"),
      dplyr::coalesce(extract_feat(.data$feats, "VerbForm"), "") == "Inf",
      stringr::str_detect(
        dplyr::coalesce(.data$dep_rel, ""),
        "^(xcomp|ccomp|advcl|obj)"
      ),
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::transmute(
      .data$doc_id, .data$sentence_id,
      head_token_id_int = .data$head_token_id_int,
      has_inf_dep = TRUE
    ) %>%
    dplyr::distinct()

  case1 <- modal_toks %>%
    dplyr::left_join(
      inf_deps,
      by = c("doc_id", "sentence_id", "token_id_int" = "head_token_id_int")
    ) %>%
    dplyr::filter(!is.na(.data$has_inf_dep)) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$token_id_int)

  # Case 2: modal is AUX dependent whose head has VerbForm=Inf
  # (canonical UD Spanish structure: INF is root, modal is aux)
  inf_heads <- tokens %>%
    dplyr::transmute(
      .data$doc_id, .data$sentence_id,
      head_token_id_int = .data$token_id_int,
      head_verbform = extract_feat(.data$feats, "VerbForm")
    )

  case2 <- modal_toks %>%
    dplyr::filter(
      stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^aux"),
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::left_join(
      inf_heads,
      by = c("doc_id", "sentence_id", "head_token_id_int")
    ) %>%
    dplyr::filter(dplyr::coalesce(.data$head_verbform, "") == "Inf") %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$token_id_int)

  dplyr::bind_rows(case1, case2) %>%
    dplyr::distinct() %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally()
}

# ─────────────────────────────────────────────────────────────────────────────
# 1.  block_adj_prep_adv_es   f_39–f_42
# ─────────────────────────────────────────────────────────────────────────────

#' Adjective, preposition, and adverb features (Spanish)
#'
#' f_39  Prepositions (dep_rel = case | fixed)
#' f_40  Attributive adjectives (dep_rel = amod; fallback: ADJ before NOUN)
#' f_41  Predicative adjectives (dep_rel = xcomp | acomp | cop target)
#' f_42  General adverbs (excludes stance/hedge/negation adverbs)
#'
#' @param tokens Annotated token data frame
#' @param doc_ids One-column data frame with column `doc_id`
#' @param dict_lookup Dictionary lookup (yaml list loaded by parse_functions)
#' @param word_lists_lookup Word lists lookup
#' @param negation_adverbs Character vector of negation adverb lemmas
#' @return Data frame: one row per doc, columns f_39 – f_42
#' @keywords internal
block_adj_prep_adv_es <- function(tokens, doc_ids, dict_lookup,
                                   word_lists_lookup, negation_adverbs) {

  # f_39  Preposiciones
  f39 <- tokens %>%
    dplyr::filter(
      .data$pos == "ADP",
      dplyr::coalesce(.data$dep_rel, "") %in% c("case", "fixed")
    ) %>%
    count_feature("f_39_prepositions")

  # f_40  Adjetivo atributivo
  has_amod <- any(
    !is.na(tokens$dep_rel) & tokens$dep_rel == "amod" & tokens$pos == "ADJ"
  )

  if (has_amod) {
    f40 <- tokens %>%
      dplyr::filter(
        .data$pos == "ADJ",
        dplyr::coalesce(.data$dep_rel, "") == "amod",
        !stringr::str_detect(.data$token, "-")
      ) %>%
      count_feature("f_40_adj_attr")
  } else {
    # Fallback: ADJ inmediatamente antes de NOUN dentro de la misma oración
    f40 <- tokens %>%
      dplyr::group_by(.data$doc_id, .data$sentence_id) %>%
      dplyr::arrange(.data$token_id_int, .by_group = TRUE) %>%
      dplyr::filter(
        .data$pos == "ADJ",
        !stringr::str_detect(.data$token, "-"),
        dplyr::lead(.data$pos, default = "") %in% c("NOUN", "PROPN", "ADJ")
      ) %>%
      dplyr::ungroup() %>%
      count_feature("f_40_adj_attr")
  }

  # f_41  Adjetivo predicativo
  #   dep_rel = xcomp o acomp, o ADJ cuyo head directo es SER/ESTAR
  linking_verbs <- c("ser", "estar", "parecer", "resultar", "quedar",
                     "volverse", "hacerse", "ponerse", "tornarse")

  f41_dep <- tokens %>%
    dplyr::filter(
      .data$pos == "ADJ",
      dplyr::coalesce(.data$dep_rel, "") %in% c("xcomp", "acomp")
    )

  f41_cop <- tokens %>%
    dplyr::filter(
      .data$pos == "ADJ",
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::left_join(
      tokens %>%
        dplyr::transmute(
          .data$doc_id, .data$sentence_id,
          head_token_id_int = .data$token_id_int,
          head_lemma = .data$lemma
        ),
      by = c("doc_id", "sentence_id", "head_token_id_int")
    ) %>%
    dplyr::filter(.data$head_lemma %in% linking_verbs)

  f41 <- dplyr::bind_rows(f41_dep, f41_cop) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$token_id_int) %>%
    count_feature("f_41_adj_pred")

  # f_42  Adverbios generales (excluye stance, hedges, negación)
  stance_lemmas <- unique(c(
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
      !.data$lemma %in% stance_lemmas
    ) %>%
    count_feature("f_42_adverbs")

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
# 2.  block_modals_es   f_52–f_54
# ─────────────────────────────────────────────────────────────────────────────

#' Modal verb perifrastic features (Spanish)
#'
#' f_52  Posibilidad:  poder + INF, caber + INF
#' f_53  Necesidad:    deber + INF, tener_que + INF,
#'                     haber_de + INF, haber_que + INF
#' f_54  Predictivo:   futuro sintético (Tense=Fut,VerbForm=Fin)
#'                     + ir_a + INF
#'
#' @param tokens Annotated token data frame
#' @param doc_ids One-column data frame with column `doc_id`
#' @param dict_lookup Dictionary lookup
#' @return Data frame: one row per doc, columns f_52 – f_54
#' @keywords internal
block_modals_es <- function(tokens, doc_ids, dict_lookup) {

  # f_52  Posibilidad
  poss_lemmas <- dictionary_to_lemmas(dict_lookup, "f_52_modal_possibility")
  f52 <- count_modal_periphrasis(tokens, poss_lemmas) %>%
    dplyr::rename(f_52_modal_possibility = "n")

  # f_53  Necesidad
  #   deber + INF (sin "de" = necesidad deóntica)
  #   deber_de + INF (probabilidad epistémica; incluido por convención)
  #   tener_que, haber_de, haber_que vienen compuestos del tokenizer
  nec_lemmas <- dictionary_to_lemmas(dict_lookup, "f_53_modal_necessity")
  f53 <- count_modal_periphrasis(tokens, nec_lemmas) %>%
    dplyr::rename(f_53_modal_necessity = "n")

  # f_54  Predictivo
  #   (a) futuro sintético: Tense=Fut + VerbForm=Fin (via extract_feat)
  f54_fut <- tokens %>%
    dplyr::filter(
      .data$pos %in% c("VERB", "AUX"),
      dplyr::coalesce(extract_feat(.data$feats, "Tense"),    "") == "Fut",
      dplyr::coalesce(extract_feat(.data$feats, "VerbForm"), "") == "Fin"
    ) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$token_id_int)

  #   (b) ir_a + INF  (ir con dep_rel aux cuyo head tiene VerbForm=Inf)
  ir_a_inf <- tokens %>%
    dplyr::filter(
      .data$lemma == "ir",
      .data$pos   %in% c("AUX", "VERB"),
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::left_join(
      tokens %>%
        dplyr::transmute(
          .data$doc_id, .data$sentence_id,
          head_token_id_int = .data$token_id_int,
          head_verbform = extract_feat(.data$feats, "VerbForm")
        ),
      by = c("doc_id", "sentence_id", "head_token_id_int")
    ) %>%
    dplyr::filter(
      dplyr::coalesce(.data$head_verbform, "") == "Inf",
      stringr::str_detect(
        dplyr::coalesce(.data$dep_rel, ""), "^aux"
      )
    ) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$token_id_int)

  f54 <- dplyr::bind_rows(f54_fut, ir_a_inf) %>%
    dplyr::distinct() %>%
    count_feature("f_54_modal_predictive")

  doc_ids %>%
    dplyr::left_join(f52, by = "doc_id") %>%
    dplyr::left_join(f53, by = "doc_id") %>%
    dplyr::left_join(f54, by = "doc_id") %>%
    dplyr::mutate(
      dplyr::across(-dplyr::any_of("doc_id"), ~ dplyr::coalesce(., 0L))
    )
}

# ─────────────────────────────────────────────────────────────────────────────
# 3.  block_specialized_verbs_es   f_55–f_58
# ─────────────────────────────────────────────────────────────────────────────

#' Specialized verb class features (Spanish)
#'
#' f_55  Verbos públicos  (decir, afirmar, señalar, indicar, declarar…)
#' f_56  Verbos privados  (creer, pensar, saber, sentir, suponer…)
#' f_57  Verbos suasivos  (pedir, exigir, recomendar, sugerir, ordenar…)
#' f_58  Verbos "seem"    (parecer, resultar, aparecer, semejarse…)
#'
#' Las listas de lemas se leen de dict.yaml (secciones f_55_verb_public,
#' f_56_verb_private, f_57_verb_suasive, f_58_verb_seem).
#'
#' @param tokens Annotated token data frame
#' @param doc_ids One-column data frame with column `doc_id`
#' @param dict_lookup Dictionary lookup
#' @return Data frame: one row per doc, columns f_55 – f_58
#' @keywords internal
block_specialized_verbs_es <- function(tokens, doc_ids, dict_lookup) {

  count_verb_class <- function(lemmas, col_name) {
    if (length(lemmas) == 0)
      return(tibble::tibble(doc_id = character(), !!col_name := integer()))
    tokens %>%
      dplyr::filter(
        .data$lemma %in% lemmas,
        .data$pos   %in% c("VERB", "AUX")
      ) %>%
      count_feature(col_name)
  }

  f55 <- count_verb_class(
    dictionary_to_lemmas(dict_lookup, "f_55_verb_public"),  "f_55_verb_public")
  f56 <- count_verb_class(
    dictionary_to_lemmas(dict_lookup, "f_56_verb_private"), "f_56_verb_private")
  f57 <- count_verb_class(
    dictionary_to_lemmas(dict_lookup, "f_57_verb_suasive"), "f_57_verb_suasive")
  f58 <- count_verb_class(
    dictionary_to_lemmas(dict_lookup, "f_58_verb_seem"),    "f_58_verb_seem")

  doc_ids %>%
    dplyr::left_join(f55, by = "doc_id") %>%
    dplyr::left_join(f56, by = "doc_id") %>%
    dplyr::left_join(f57, by = "doc_id") %>%
    dplyr::left_join(f58, by = "doc_id") %>%
    dplyr::mutate(
      dplyr::across(-dplyr::any_of("doc_id"), ~ dplyr::coalesce(., 0L))
    )
}
