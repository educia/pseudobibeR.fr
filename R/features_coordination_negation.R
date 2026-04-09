# features_coordination_negation.R
# Coordination, split structures, negation, and lexical features (Spanish)
# f_59, f_61–f_67 + f_10 (demonstratives), f_14–f_16 (nouns)
#
# NOTA LINGÜÍSTICA — negación española:
#
#   Español: morfema ÚNICO "no" preverbal + pronombres/adverbios negativos.
#
#   f_66  NEGACIÓN SINTÉTICA = pronombre/adverbio negativo ocupa posición
#         preverbal SIN co-ocurrencia de "no" en la misma cláusula.
#         Ejemplos: "Nadie llegó", "Nada importa", "Nunca viene".
#
#   f_67  NEGACIÓN ANALÍTICA = "no" (o variantes: ni, tampoco) en posición
#         preverbal ligado al verbo por dep_rel=advmod.
#         Ejemplos: "no llegó", "no lo veo nunca".
#
# BUG CORREGIDO respecto a la versión anterior:
#   El left_join de analytic_neg_heads usaba .data$ dentro del vector
#   by = c(...) lo que produce error en dplyr. Corregido con rename previo.

# ─────────────────────────────────────────────────────────────────────────────
# 1.  block_contractions_es   f_59
# ─────────────────────────────────────────────────────────────────────────────

#' Contraction features (Spanish)
#'
#' El español moderno carece de contracciones ortográficas productivas
#' equivalentes a las del francés. Esta función devuelve siempre 0 para
#' mantener paridad estructural con pseudobibeR.fr.
#' Excepción: las contracciones gramaticales *del* (de + el) y *al*
#' (a + el) se cuentan como indicadores de registro informal.
#'
#' @param tokens Annotated token data frame
#' @param doc_ids One-column data frame with column `doc_id`
#' @return Data frame with f_59_contractions
#' @keywords internal
block_contractions_es <- function(tokens, doc_ids) {
  # "del" y "al" son las únicas contracciones gramaticales del español.
  # Los parsers UD las tratan como tokens multi-word (MWT) o como un
  # token único; contamos el token superficial.
  f59 <- tokens %>%
    dplyr::filter(
      stringr::str_to_lower(.data$token) %in% c("del", "al")
    ) %>%
    count_feature("f_59_contractions")

  doc_ids %>%
    dplyr::left_join(f59, by = "doc_id") %>%
    dplyr::mutate(f_59_contractions = dplyr::coalesce(.data$f_59_contractions, 0L))
}

# ─────────────────────────────────────────────────────────────────────────────
# 2.  block_stranded_split_es   f_61–f_62
# ─────────────────────────────────────────────────────────────────────────────

#' Stranded preposition and split infinitive features (Spanish)
#'
#' f_61  Preposición varada: ADP precede inmediatamente a pronombre
#'       relativo/interrogativo ("a quien", "de lo que").
#'       En español la preposición varada en relativas es marginal y
#'       muy marcada; este rasgo discrimina registros informales orales.
#'
#' f_62  Infinitivo escindido: preposición + adverbio(s) + infinitivo
#'       ("para realmente entender", "al nunca poder salir").
#'       Patrón raro pero documentado en registros cultos y periodísticos.
#'
#' @param tokens Annotated token data frame
#' @param doc_ids One-column data frame with column `doc_id`
#' @return Data frame with f_61_stranded_preposition, f_62_split_infinitive
#' @keywords internal
block_stranded_split_es <- function(tokens, doc_ids) {

  tokens_ctx <- tokens %>%
    dplyr::group_by(.data$doc_id, .data$sentence_id) %>%
    dplyr::arrange(.data$token_id_int, .by_group = TRUE) %>%
    dplyr::mutate(
      lead1_pos      = dplyr::lead(.data$pos,   default = NA_character_),
      lead1_lemma    = dplyr::lead(.data$lemma, default = NA_character_),
      lead1_sent     = dplyr::lead(.data$sentence_id, default = NA_integer_),
      lead1_prontype = dplyr::lead(
        extract_feat(.data$feats, "PronType"), default = NA_character_
      ),
      lag1_pos    = dplyr::lag(.data$pos,          default = NA_character_),
      lag1_lemma  = dplyr::lag(.data$lemma,        default = NA_character_),
      lag1_sent   = dplyr::lag(.data$sentence_id,  default = NA_integer_),
      lag2_pos    = dplyr::lag(.data$pos,    2,    default = NA_character_),
      lag2_lemma  = dplyr::lag(.data$lemma,  2,    default = NA_character_),
      lag2_sent   = dplyr::lag(.data$sentence_id, 2, default = NA_integer_),
      lag3_pos    = dplyr::lag(.data$pos,    3,    default = NA_character_),
      lag3_lemma  = dplyr::lag(.data$lemma, 3,    default = NA_character_),
      lag3_sent   = dplyr::lag(.data$sentence_id, 3, default = NA_integer_),
      lag4_pos    = dplyr::lag(.data$pos,    4,    default = NA_character_),
      lag4_lemma  = dplyr::lag(.data$lemma,  4,    default = NA_character_),
      lag4_sent   = dplyr::lag(.data$sentence_id, 4, default = NA_integer_)
    ) %>%
    dplyr::ungroup()

  stranded_rel <- c("quien", "quienes", "que", "cual", "cuales", "donde")

  # f_61
  f61 <- tokens_ctx %>%
    dplyr::filter(
      .data$pos == "ADP",
      !is.na(.data$lead1_sent),
      .data$lead1_sent == .data$sentence_id,
      .data$lead1_pos  %in% c("PRON", "DET", "ADV"),
      .data$lead1_lemma %in% stranded_rel,
      stringr::str_detect(
        dplyr::coalesce(.data$lead1_prontype, ""), "Rel|Int"
      )
    ) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$token_id_int) %>%
    count_feature("f_61_stranded_preposition")

  inf_prepositions <- c("a", "al", "de", "del", "por", "para")
  filler_pos       <- c("ADV", "PART", "PRON", "DET")

  # f_62: patrón ADP (+filler*) + ADV + INF
  f62_candidates <- tokens_ctx %>%
    dplyr::filter(
      .data$pos %in% c("VERB", "AUX"),
      dplyr::coalesce(extract_feat(.data$feats, "VerbForm"), "") == "Inf"
    ) %>%
    dplyr::mutate(
      same1 = !is.na(.data$lag1_sent) & .data$lag1_sent == .data$sentence_id,
      same2 = !is.na(.data$lag2_sent) & .data$lag2_sent == .data$sentence_id,
      same3 = !is.na(.data$lag3_sent) & .data$lag3_sent == .data$sentence_id,
      same4 = !is.na(.data$lag4_sent) & .data$lag4_sent == .data$sentence_id,
      # patrón de 2: ADP ADV INF
      split2 = .data$same2 &
        .data$lag2_pos   == "ADP" &
        .data$lag2_lemma %in% inf_prepositions &
        .data$same1 & .data$lag1_pos == "ADV",
      # patrón de 3: ADP filler ADV INF
      split3 = .data$same3 &
        .data$lag3_pos   == "ADP" &
        .data$lag3_lemma %in% inf_prepositions &
        .data$same2 & .data$lag2_pos %in% filler_pos &
        .data$same1 & .data$lag1_pos == "ADV",
      # patrón de 4: ADP filler filler ADV INF
      split4 = .data$same4 &
        .data$lag4_pos   == "ADP" &
        .data$lag4_lemma %in% inf_prepositions &
        .data$same3 & .data$lag3_pos %in% filler_pos &
        .data$same2 & .data$lag2_pos %in% filler_pos &
        .data$same1 & .data$lag1_pos == "ADV"
    ) %>%
    dplyr::filter(.data$split2 | .data$split3 | .data$split4)

  f62 <- f62_candidates %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$token_id_int) %>%
    count_feature("f_62_split_infinitive")

  doc_ids %>%
    dplyr::left_join(f61, by = "doc_id") %>%
    dplyr::left_join(f62, by = "doc_id") %>%
    dplyr::mutate(
      dplyr::across(-dplyr::any_of("doc_id"), ~ dplyr::coalesce(., 0L))
    )
}

# ─────────────────────────────────────────────────────────────────────────────
# 3.  block_split_coordination_es   f_63–f_65
# ─────────────────────────────────────────────────────────────────────────────

#' Split auxiliary and coordination features (Spanish)
#'
#' f_63  Auxiliar escindido: ADV interviene entre auxiliar y verbo principal
#' f_64  Coordinación sintagmática: CCONJ entre sintagmas no clausales
#' f_65  Coordinación clausal:      CCONJ entre cláusulas con sujeto
#'
#' @param tokens Annotated token data frame
#' @param doc_ids One-column data frame with column `doc_id`
#' @param token_lookup Token-level attribute lookup table
#' @param subject_heads Table of clause heads that have an explicit subject
#' @param head_lookup Head-token attribute table
#' @param negation_part_lemmas Negation particle lemmas (no, ni, tampoco)
#' @return Data frame with f_63, f_64, f_65
#' @keywords internal
block_split_coordination_es <- function(tokens, doc_ids, token_lookup,
                                         subject_heads, head_lookup,
                                         negation_part_lemmas) {

  adv_interveners <- tokens %>%
    dplyr::filter(
      .data$pos == "ADV" |
        (.data$pos == "PART" & .data$lemma %in% negation_part_lemmas)
    ) %>%
    dplyr::transmute(
      .data$doc_id, .data$sentence_id,
      adv_tok = .data$token_id_int
    )

  aux_deps <- tokens %>%
    dplyr::filter(
      stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^aux")
    ) %>%
    dplyr::left_join(
      head_lookup,
      by = c("doc_id", "sentence_id", "head_token_id_int" = "token_id_int")
    ) %>%
    dplyr::filter(
      dplyr::coalesce(.data$head_pos, "") %in% c("VERB", "AUX"),
      !is.na(.data$token_id_int),
      !is.na(.data$head_token_id_int),
      .data$token_id_int != .data$head_token_id_int
    ) %>%
    dplyr::mutate(
      span_min = pmin(.data$token_id_int, .data$head_token_id_int),
      span_max = pmax(.data$token_id_int, .data$head_token_id_int)
    )

  # f_63
  f63 <- aux_deps %>%
    dplyr::left_join(adv_interveners, by = c("doc_id", "sentence_id")) %>%
    dplyr::filter(
      !is.na(.data$adv_tok),
      .data$adv_tok > .data$span_min,
      .data$adv_tok < .data$span_max
    ) %>%
    dplyr::distinct(.data$doc_id, .data$token_id_int) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::summarise(f_63_split_auxiliary = dplyr::n(), .groups = "drop")

  # cc_tokens: CCONJ con dep_rel=cc + atributos de su conjunción hermana
  cc_tokens <- tokens %>%
    dplyr::filter(
      .data$pos == "CCONJ",
      dplyr::coalesce(.data$dep_rel, "") == "cc"
    ) %>%
    # atributos del head del CCONJ (= segundo conjunto)
    dplyr::left_join(
      token_lookup,
      by = c("doc_id", "sentence_id", "head_token_id_int" = "token_id_int")
    ) %>%
    dplyr::rename(
      conj_pos            = "token_pos",
      conj_dep_rel        = "token_dep_rel",
      conj_head_id        = "token_head_token_id_int",
      conj_verbform       = "token_morph_verbform"
    ) %>%
    # atributos del primer conjunto (head del head)
    dplyr::left_join(
      token_lookup %>%
        dplyr::select(
          "doc_id", "sentence_id", "token_id_int",
          first_conj_pos = "token_pos"
        ),
      by = c("doc_id", "sentence_id", "conj_head_id" = "token_id_int")
    ) %>%
    # sujeto explícito en la cláusula
    dplyr::left_join(
      subject_heads,
      by = c("doc_id", "sentence_id",
             "head_token_id_int" = "clause_head_token_id_int")
    ) %>%
    dplyr::mutate(
      has_subject = dplyr::coalesce(.data$has_subject, FALSE)
    )

  # f_64  Coordinación sintagmática
  f64 <- cc_tokens %>%
    dplyr::filter(
      dplyr::coalesce(.data$conj_dep_rel, "") == "conj",
      dplyr::coalesce(.data$conj_pos,     "") %in%
        c("NOUN", "PROPN", "ADJ", "ADV"),
      !is.na(.data$first_conj_pos),
      .data$first_conj_pos == .data$conj_pos,
      !.data$has_subject
    ) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::summarise(f_64_phrasal_coordination = dplyr::n(), .groups = "drop")

  # f_65  Coordinación clausal
  f65 <- cc_tokens %>%
    dplyr::filter(
      dplyr::coalesce(.data$conj_dep_rel, "") == "conj",
      dplyr::coalesce(.data$conj_pos,     "") %in% c("VERB", "AUX"),
      .data$has_subject,
      is.na(.data$conj_verbform) |
        !.data$conj_verbform %in% c("Inf", "Ger", "Part")
    ) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::summarise(f_65_clausal_coordination = dplyr::n(), .groups = "drop")

  doc_ids %>%
    dplyr::left_join(f63, by = "doc_id") %>%
    dplyr::left_join(f64, by = "doc_id") %>%
    dplyr::left_join(f65, by = "doc_id") %>%
    dplyr::mutate(
      dplyr::across(-dplyr::any_of("doc_id"), ~ dplyr::coalesce(., 0L))
    )
}

# ─────────────────────────────────────────────────────────────────────────────
# 4.  block_negation_es   f_66–f_67
# ─────────────────────────────────────────────────────────────────────────────

#' Negation features (Spanish)
#'
#' f_66  Negación sintética — pronombre/adverbio negativo preverbal SIN «no»
#' f_67  Negación analítica  — «no» (o ni/tampoco) preverbal + verbo
#'
#' @param tokens Annotated token data frame
#' @param doc_ids One-column data frame with column `doc_id`
#' @param neg_synthetic_terms Lemmas de pronombres/adverbios negativos
#'   (nadie, nada, ninguno, ninguna, nunca, jamás)
#' @param negation_part_lemmas Lemmas de partículas negativas (no, ni, tampoco)
#' @return Data frame with f_66_neg_synthetic, f_67_neg_analytic
#' @keywords internal
block_negation_es <- function(tokens, doc_ids,
                               neg_synthetic_terms,
                               negation_part_lemmas,
                               negation_adverbs = NULL) {

  # ── Tabla auxiliar de heads verbales negados analíticamente ───────────────
  # Columnas: doc_id, sentence_id, neg_head_id
  verb_pos <- c("VERB", "AUX")

  analytic_heads <- tokens %>%
    dplyr::filter(
      .data$lemma %in% negation_part_lemmas,
      dplyr::coalesce(.data$dep_rel, "") == "advmod",
      !is.na(.data$head_token_id_int)
    ) %>%
    # join para verificar que el head es VERB/AUX
    dplyr::rename(neg_head_id = "head_token_id_int") %>%
    dplyr::left_join(
      tokens %>%
        dplyr::transmute(
          doc_id2      = .data$doc_id,
          sentence_id2 = .data$sentence_id,
          neg_head_id  = .data$token_id_int,
          head_pos     = .data$pos
        ),
      by = c(
        "doc_id"      = "doc_id2",
        "sentence_id" = "sentence_id2",
        "neg_head_id"
      )
    ) %>%
    dplyr::filter(dplyr::coalesce(.data$head_pos, "") %in% verb_pos) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$neg_head_id)

  # ── f_67  Negación analítica ──────────────────────────────────────────────
  f67 <- tokens %>%
    dplyr::filter(
      .data$lemma %in% negation_part_lemmas,
      dplyr::coalesce(.data$dep_rel, "") == "advmod",
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::rename(neg_head_id = "head_token_id_int") %>%
    dplyr::inner_join(
      analytic_heads,
      by = c("doc_id", "sentence_id", "neg_head_id")
    ) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$token_id_int) %>%
    count_feature("f_67_neg_analytic")

  # ── f_66  Negación sintética ──────────────────────────────────────────────
  # Pronombre/adverbio negativo con función sintáctica real (nsubj, advmod,
  # obj, obl…) cuyo head verbal NO aparece en analytic_heads.
  synth_dep_rels <- c(
    "nsubj", "nsubj:pass", "advmod", "obj", "iobj",
    "nmod", "obl", "obl:agent"
  )

  f66 <- tokens %>%
    dplyr::filter(
      .data$lemma %in% neg_synthetic_terms,
      .data$pos   %in% c("PRON", "ADV", "DET"),
      dplyr::coalesce(.data$dep_rel, "") %in% synth_dep_rels,
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::rename(neg_head_id = "head_token_id_int") %>%
    dplyr::anti_join(
      analytic_heads,
      by = c("doc_id", "sentence_id", "neg_head_id")
    ) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$token_id_int) %>%
    count_feature("f_66_neg_synthetic")

  doc_ids %>%
    dplyr::left_join(f66, by = "doc_id") %>%
    dplyr::left_join(f67, by = "doc_id") %>%
    dplyr::mutate(
      dplyr::across(-dplyr::any_of("doc_id"), ~ dplyr::coalesce(., 0L))
    )
}

# ─────────────────────────────────────────────────────────────────────────────
# 5.  block_lexical_membership_es   f_10 (dem.), f_14–f_16 (nouns)
# ─────────────────────────────────────────────────────────────────────────────

#' Demonstrative pronoun, nominalization, gerund, and noun features (Spanish)
#'
#' f_10  Pronombres demostrativos (este, ese, aquel + formas)
#' f_14  Nominalizaciones (sustantivos con sufijos productivos)
#' f_15  Gerundios
#' f_16  Otros sustantivos (= total NOUN/PROPN - nominalizaciones - gerundios-NOUN)
#' f_51  Demostrativos determinantes (mismo este/ese/aquel en función DET)
#'
#' @param tokens Annotated token data frame
#' @param doc_ids One-column data frame with column `doc_id`
#' @param word_lists_lookup Word lists lookup
#' @return Data frame with f_10, f_14, f_15, f_16, f_51
#' @keywords internal
block_lexical_membership_es <- function(tokens, doc_ids, word_lists_lookup) {

  pronoun_terms         <- get_word_list(word_lists_lookup, "pronoun_matchlist")
  nominalization_sfx    <- get_word_list(word_lists_lookup, "nominalization_suffixes")
  nominalization_stop   <- normalize_terms(
    get_word_list(word_lists_lookup, "nominalization_stoplist")
  )
  gerund_stop           <- normalize_terms(
    get_word_list(word_lists_lookup, "gerund_stoplist")
  )

  # ── f_10  Pronombres demostrativos ────────────────────────────────────────
  f10 <- tokens %>%
    dplyr::filter(
      stringr::str_to_lower(.data$token) %in%
        stringr::str_to_lower(pronoun_terms),
      .data$pos == "PRON"
    ) %>%
    count_feature("f_10_demonstrative_pronoun")

  # ── f_51  Demostrativos determinantes ─────────────────────────────────────
  f51 <- tokens %>%
    dplyr::filter(
      stringr::str_to_lower(.data$token) %in%
        stringr::str_to_lower(pronoun_terms),
      .data$pos == "DET",
      dplyr::coalesce(.data$dep_rel, "") == "det"
    ) %>%
    count_feature("f_51_demonstratives")

  # ── f_14  Nominalizaciones ────────────────────────────────────────────────
  if (length(nominalization_sfx) > 0) {
    sfx_pat <- paste0(
      "(", paste(nominalization_sfx, collapse = "|"), ")$"
    )
  } else {
    sfx_pat <- "^$"
  }

  f14 <- tokens %>%
    dplyr::filter(.data$pos == "NOUN") %>%
    dplyr::mutate(lem_lower = stringr::str_to_lower(.data$lemma)) %>%
    dplyr::filter(
      stringr::str_detect(.data$lem_lower, sfx_pat),
      !.data$lem_lower %in% nominalization_stop
    ) %>%
    count_feature("f_14_nominalizations")

  # ── f_15  Gerundios ───────────────────────────────────────────────────────
  # Por morfología UD (VerbForm=Ger)
  f15_morph <- tokens %>%
    dplyr::filter(
      .data$pos %in% c("VERB", "AUX"),
      dplyr::coalesce(extract_feat(.data$feats, "VerbForm"), "") == "Ger",
      !.data$lemma %in% gerund_stop
    )

  # Fallback: NOUN con lema en -ando/-iendo precedido de preposición «en»
  f15_fallback <- tokens %>%
    dplyr::group_by(.data$doc_id, .data$sentence_id) %>%
    dplyr::arrange(.data$token_id_int, .by_group = TRUE) %>%
    dplyr::filter(
      .data$pos %in% c("NOUN", "PROPN"),
      stringr::str_detect(
        stringr::str_to_lower(.data$lemma), "(ando|iendo)$"
      ),
      dplyr::lag(.data$token, default = "") == "en",
      dplyr::lag(.data$pos,   default = "") == "ADP",
      !.data$lemma %in% gerund_stop
    ) %>%
    dplyr::ungroup()

  f15_tokens <- dplyr::bind_rows(f15_morph, f15_fallback) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$token_id_int,
                    .keep_all = TRUE)

  gerunds_noun_n <- f15_tokens %>%
    dplyr::filter(.data$pos %in% c("NOUN", "PROPN")) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(gerunds_noun_n = "n")

  f15 <- f15_tokens %>%
    count_feature("f_15_gerunds")

  # ── f_16  Otros sustantivos ───────────────────────────────────────────────
  f16_raw <- tokens %>%
    dplyr::filter(
      .data$pos %in% c("NOUN", "PROPN"),
      !stringr::str_detect(.data$token, "-")
    ) %>%
    count_feature("f_16_other_nouns")

  f16 <- f16_raw %>%
    dplyr::left_join(
      f14 %>% dplyr::rename(n_nom = "f_14_nominalizations"),
      by = "doc_id"
    ) %>%
    dplyr::left_join(gerunds_noun_n, by = "doc_id") %>%
    dplyr::mutate(
      n_nom        = dplyr::coalesce(.data$n_nom, 0L),
      gerunds_noun_n = dplyr::coalesce(.data$gerunds_noun_n, 0L),
      f_16_other_nouns = pmax(
        0L,
        .data$f_16_other_nouns - .data$n_nom - .data$gerunds_noun_n
      )
    ) %>%
    dplyr::select("doc_id", "f_16_other_nouns")

  doc_ids %>%
    dplyr::left_join(f10,  by = "doc_id") %>%
    dplyr::left_join(f51,  by = "doc_id") %>%
    dplyr::left_join(f14,  by = "doc_id") %>%
    dplyr::left_join(f15,  by = "doc_id") %>%
    dplyr::left_join(f16,  by = "doc_id") %>%
    dplyr::mutate(
      dplyr::across(-dplyr::any_of("doc_id"), ~ dplyr::coalesce(., 0L))
    )
}
