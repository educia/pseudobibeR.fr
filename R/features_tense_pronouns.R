# Tense, aspect, and pronoun features

#' Extract tense and auxiliary-related features
#'
#' @param tokens Annotated token data frame
#' @param doc_ids Document IDs
#' @param head_lookup Head token lookup table
#' @param proverb_pronouns Vector of proverb pronoun lemmas
#' @return Data frame with f_02_perfect_aspect and f_12_proverb_do
#' @keywords internal
block_aux_tense_fr <- function(tokens, doc_ids, head_lookup, proverb_pronouns) {
  perfect_candidates <- tokens %>%
    dplyr::filter(
      .data$pos %in% c("AUX", "VERB"),
      .data$lemma %in% c("avoir", "\u00eatre"),
      stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^(aux|cop)"),
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::left_join(
      head_lookup,
      by = c("doc_id", "sentence_id", "head_token_id_int" = "token_id_int")
    ) %>%
    dplyr::mutate(
      head_is_participle = (
        .data$head_pos %in% c("VERB", "AUX") &
          .data$head_morph_verbform == "Part" &
          dplyr::coalesce(.data$head_morph_voice, "") != "Pass"
      ) |
        (
          .data$head_pos %in% c("ADJ", "NOUN") &
            stringr::str_detect(
              dplyr::coalesce(.data$head_feats, ""),
              "VerbForm=Part"
            ) &
            dplyr::coalesce(.data$head_morph_voice, "") != "Pass"
        ) |
        (
          .data$head_pos == "NOUN" &
            stringr::str_detect(
              stringr::str_to_lower(dplyr::coalesce(.data$head_token, "")),
              "(\u00e9|\u00e9e|\u00e9s|\u00e9es|i|ie|is|ies|u|ue|us|ues)$"
            )
        )
    ) %>%
    dplyr::filter(.data$head_is_participle) %>%
    dplyr::distinct(.data$doc_id, .data$head_token_id_int, .keep_all = TRUE)

  f02 <- perfect_candidates %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_02_perfect_aspect = "n")

  proverb_objects <- tokens %>%
    dplyr::filter(
      .data$pos == "PRON",
      .data$lemma %in% proverb_pronouns,
      stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^obj"),
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::transmute(
      .data$doc_id,
      .data$sentence_id,
      head_token_id_int = .data$head_token_id_int,
      has_proverb_object = TRUE
    ) %>%
    dplyr::distinct()

  proverb_candidates <- tokens %>%
    dplyr::filter(
      .data$lemma == "faire",
      .data$pos %in% c("VERB", "AUX"),
      !stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^aux"),
      !is.na(.data$token_id_int)
    )

  f12 <- proverb_candidates %>%
    dplyr::left_join(
      proverb_objects,
      by = c("doc_id", "sentence_id", "token_id_int" = "head_token_id_int")
    ) %>%
    dplyr::filter(.data$has_proverb_object) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$token_id_int) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_12_proverb_do = "n")

  doc_ids %>%
    dplyr::left_join(f02, by = "doc_id") %>%
    dplyr::left_join(f12, by = "doc_id") %>%
    dplyr::mutate(
      f_02_perfect_aspect = dplyr::coalesce(.data$f_02_perfect_aspect, 0L),
      f_12_proverb_do = dplyr::coalesce(.data$f_12_proverb_do, 0L)
    )
}

#' Extract personal pronoun and question features
#'
#' @param tokens Annotated token data frame
#' @param doc_ids Document IDs
#' @param head_lookup Head token lookup table
#' @param de_markers De marker lookup table
#' @param que_markers Que marker lookup table
#' @param clause_complements Clause complement lookup table
#' @param weather_lemmas Weather verb lemmas
#' @param raising_verbs Raising verb lemmas
#' @param wh_question_lemmas WH question word lemmas
#' @return Data frame with f_09_pronoun_it and f_13_wh_question
#' @keywords internal
block_personal_pronouns_fr <- function(
    tokens,
    doc_ids,
    head_lookup,
    de_markers,
    que_markers,
    clause_complements,
    weather_lemmas,
    raising_verbs,
    wh_question_lemmas) {
  pronoun_it_candidates <- tokens %>%
    dplyr::filter(
      .data$pos == "PRON",
      .data$lemma %in% c("il", "ce", "cela", "\u00e7a"),
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::left_join(
      head_lookup,
      by = c("doc_id", "sentence_id", "head_token_id_int" = "token_id_int")
    ) %>%
    dplyr::left_join(de_markers, by = c("doc_id", "sentence_id", "head_token_id_int")) %>%
    dplyr::left_join(que_markers, by = c("doc_id", "sentence_id", "head_token_id_int")) %>%
    dplyr::left_join(clause_complements, by = c("doc_id", "sentence_id", "head_token_id_int")) %>%
    dplyr::mutate(
      has_de_marker = dplyr::coalesce(.data$has_de_marker, FALSE),
      has_que_marker = dplyr::coalesce(.data$has_que_marker, FALSE),
      has_clause_comp = dplyr::coalesce(.data$has_clause_comp, FALSE),
      is_weather = .data$head_lemma %in% weather_lemmas,
      is_raising_verb = .data$head_lemma %in% raising_verbs,
      has_control_marker = .data$has_de_marker | .data$has_que_marker | .data$has_clause_comp
    ) %>%
    dplyr::filter(
      stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^expl") |
        (
          stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^nsubj") &
            (
              .data$is_weather |
                (
                  .data$head_pos == "ADJ" &
                    .data$has_control_marker
                ) |
                (
                  .data$head_pos %in% c("VERB", "AUX") &
                    (.data$is_raising_verb | .data$has_control_marker)
                )
            )
        )
    )

  f09 <- pronoun_it_candidates %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$token_id_int) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_09_pronoun_it = "n")

  question_sentences <- tokens %>%
    dplyr::filter(.data$token == "?") %>%
    dplyr::transmute(
      .data$doc_id,
      .data$sentence_id,
      has_question = TRUE
    ) %>%
    dplyr::distinct()

  f13 <- tokens %>%
    dplyr::filter(
      .data$lemma %in% wh_question_lemmas,
      .data$pos %in% c("ADV", "PRON", "DET", "ADJ")
    ) %>%
    dplyr::left_join(
      question_sentences,
      by = c("doc_id", "sentence_id")
    ) %>%
    dplyr::filter(!is.na(.data$has_question)) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$token_id_int) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_13_wh_question = "n")

  doc_ids %>%
    dplyr::left_join(f09, by = "doc_id") %>%
    dplyr::left_join(f13, by = "doc_id") %>%
    dplyr::mutate(
      dplyr::across(-dplyr::any_of("doc_id"), ~ dplyr::coalesce(., 0L))
    )
}
