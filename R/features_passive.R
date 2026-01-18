# Passive voice and stative features

#' Extract passive voice and stative verb features
#'
#' @param tokens Annotated token data frame
#' @param doc_ids Document IDs
#' @param head_lookup Head token lookup table
#' @param passive_rel_values Dependency relation values indicating passive voice
#' @return Data frame with f_17_agentless_passives, f_18_by_passives, f_19_be_main_verb, f_20_existential_there
#' @keywords internal
block_passive_voice_fr <- function(tokens, doc_ids, head_lookup, passive_rel_values) {
  passive_candidates <- tokens %>%
    dplyr::filter(.data$dep_rel %in% passive_rel_values) %>%
    dplyr::left_join(
      head_lookup,
      by = c("doc_id", "sentence_id", "head_token_id_int" = "token_id_int")
    )

  f17 <- passive_candidates %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      .data$head_pos == "VERB",
      !.data$passive_agent_next2,
      !.data$passive_agent_next3
    ) %>%
    dplyr::distinct(.data$doc_id, .data$head_token_id_int, .keep_all = TRUE) %>%
    dplyr::tally() %>%
    dplyr::rename(f_17_agentless_passives = "n")

  f18 <- passive_candidates %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      .data$head_pos == "VERB",
      (.data$passive_agent_next2 | .data$passive_agent_next3)
    ) %>%
    dplyr::distinct(.data$doc_id, .data$head_token_id_int, .keep_all = TRUE) %>%
    dplyr::tally() %>%
    dplyr::rename(f_18_by_passives = "n")

  f19 <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      .data$lemma == "\u00eatre",
      !stringr::str_detect(.data$dep_rel, "aux")
    ) %>%
    dplyr::tally() %>%
    dplyr::rename(f_19_be_main_verb = "n")

  f20 <- tokens %>%
    dplyr::group_by(.data$doc_id, .data$sentence_id) %>%
    dplyr::filter(
      .data$lemma == "avoir",
      .data$pos %in% c("VERB", "AUX"),
      dplyr::lag(.data$lemma == "y", default = FALSE),
      dplyr::lag(.data$lemma == "il", 2, default = FALSE),
      dplyr::lag(.data$pos == "PRON", 2, default = FALSE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .keep_all = TRUE) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_20_existential_there = "n")

  doc_ids %>%
    dplyr::left_join(f17, by = "doc_id") %>%
    dplyr::left_join(f18, by = "doc_id") %>%
    dplyr::left_join(f19, by = "doc_id") %>%
    dplyr::left_join(f20, by = "doc_id") %>%
    dplyr::mutate(
      dplyr::across(-dplyr::any_of("doc_id"), ~ dplyr::coalesce(., 0L))
    )
}
