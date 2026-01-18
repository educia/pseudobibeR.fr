# Subordination and clause embedding features

#' Extract participial clause features
#'
#' @param tokens Annotated token data frame with participle markers
#' @param doc_ids Document IDs
#' @return Data frame with f_24_infinitives through f_28_present_participle_whiz
#' @keywords internal
block_participial_clauses_fr <- function(tokens, doc_ids) {
  f24 <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(.data$is_infinitive) %>%
    dplyr::tally() %>%
    dplyr::rename(f_24_infinitives = "n")

  f25 <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      .data$is_present_participle,
      .data$dep_rel %in% c("advcl", "ccomp"),
      (
        dplyr::lag(.data$dep_rel == "punct", default = TRUE) |
          (
            dplyr::lag(.data$token %in% c("en"), default = FALSE) &
              dplyr::lag(.data$dep_rel %in% c("mark", "case"), default = FALSE)
          )
      )
    ) %>%
    dplyr::tally() %>%
    dplyr::rename(f_25_present_participle = "n")

  f26 <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      .data$is_past_participle,
      (
        .data$dep_rel %in% c("advcl", "ccomp") |
          (
            .data$dep_rel == "acl" &
              dplyr::lag(.data$dep_rel == "punct", default = TRUE)
          )
      ),
      (
        dplyr::lag(.data$dep_rel == "punct", default = TRUE) |
          (
            dplyr::lag(.data$token %in% c("en"), default = FALSE) &
              dplyr::lag(.data$dep_rel %in% c("mark", "case"), default = FALSE)
          )
      )
    ) %>%
    dplyr::tally() %>%
    dplyr::rename(f_26_past_participle = "n")

  f27 <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      .data$is_past_participle,
      dplyr::lag(.data$pos == "NOUN"),
      (
        stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^acl") |
          .data$dep_rel == "root"
      )
    ) %>%
    dplyr::tally() %>%
    dplyr::rename(f_27_past_participle_whiz = "n")

  f28 <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      .data$is_present_participle,
      dplyr::lag(.data$pos == "NOUN"),
      .data$dep_rel == "acl"
    ) %>%
    dplyr::tally() %>%
    dplyr::rename(f_28_present_participle_whiz = "n")

  doc_ids %>%
    dplyr::left_join(f24, by = "doc_id") %>%
    dplyr::left_join(f25, by = "doc_id") %>%
    dplyr::left_join(f26, by = "doc_id") %>%
    dplyr::left_join(f27, by = "doc_id") %>%
    dplyr::left_join(f28, by = "doc_id") %>%
    dplyr::mutate(dplyr::across(-dplyr::any_of("doc_id"), ~ dplyr::coalesce(., 0L)))
}

#' Extract relative clause features
#'
#' @param tokens Annotated token data frame with relative pronouns marked
#' @param doc_ids Document IDs
#' @return Data frame with f_29_that_subj through f_34_sentence_relatives
#' @keywords internal
block_relatives_fr <- function(tokens, doc_ids) {
  relative_subject_that_lemmas <- c("qui")
  relative_object_that_lemmas <- c("que")
  wh_subject_relative_lemmas <- c("lequel", "laquelle", "lesquel", "lesquelle", "lesquels", "lesquelles")
  wh_object_relative_lemmas <- c(
    "dont",
    "lequel", "laquelle", "lesquel", "lesquelle", "lesquels", "lesquelles",
    "duquel", "desquels", "desquelles",
    "auquel", "auxquels", "auxquelles"
  )
  pied_piping_relative_lemmas <- c(
    "dont",
    "lequel", "laquelle", "lesquels", "lesquelles",
    "auquel", "auxquels", "auxquelles",
    "duquel", "desquels", "desquelles"
  )
  sentence_relative_anchors <- c("ce", "cela", "ceci", "celui", "celle", "ceux", "celles")

  f29 <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      .data$is_relative_subject,
      .data$lemma %in% relative_subject_that_lemmas,
      (
        dplyr::lag(.data$pos %in% c("NOUN", "PROPN"), default = FALSE) |
          (
            dplyr::lag(.data$pos == "ADJ", default = FALSE) &
              dplyr::lag(.data$pos %in% c("NOUN", "PROPN"), 2, default = FALSE)
          )
      )
    ) %>%
    dplyr::tally() %>%
    dplyr::rename(f_29_that_subj = "n")

  f30 <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      .data$is_relative_object,
      .data$lemma %in% relative_object_that_lemmas,
      dplyr::lag(.data$pos %in% c("NOUN", "PROPN"), default = FALSE)
    ) %>%
    dplyr::tally() %>%
    dplyr::rename(f_30_that_obj = "n")

  f31 <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      .data$is_relative_subject,
      .data$lemma %in% wh_subject_relative_lemmas,
      (
        dplyr::lag(.data$pos %in% c("NOUN", "PROPN"), default = FALSE) |
          (
            dplyr::lag(.data$pos == "PUNCT", default = FALSE) &
              dplyr::lag(.data$pos %in% c("NOUN", "PROPN"), 2, default = FALSE)
          )
      )
    ) %>%
    dplyr::tally() %>%
    dplyr::rename(f_31_wh_subj = "n")

  f32 <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      .data$is_relative_object,
      .data$lemma %in% wh_object_relative_lemmas,
      (
        dplyr::lag(.data$pos %in% c("NOUN", "PROPN"), default = FALSE) |
          (
            dplyr::lag(.data$pos == "PUNCT", default = FALSE) &
              dplyr::lag(.data$pos %in% c("NOUN", "PROPN"), 2, default = FALSE)
          )
      )
    ) %>%
    dplyr::tally() %>%
    dplyr::rename(f_32_wh_obj = "n")

  f33 <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      .data$is_relative_pronoun,
      .data$lemma %in% pied_piping_relative_lemmas,
      (
        .data$lemma == "dont" |
          dplyr::lag(.data$pos == "ADP", default = FALSE)
      )
    ) %>%
    dplyr::tally() %>%
    dplyr::rename(f_33_pied_piping = "n")

  f34 <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      .data$is_relative_pronoun,
      dplyr::lag(.data$token %in% sentence_relative_anchors, default = FALSE),
      (
        dplyr::lag(.data$pos == "PUNCT", 2, default = TRUE) |
          is.na(dplyr::lag(.data$token, 2))
      )
    ) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$token_id_int, .keep_all = TRUE) %>%
    dplyr::tally() %>%
    dplyr::rename(f_34_sentence_relatives = "n")

  doc_ids %>%
    dplyr::left_join(f29, by = "doc_id") %>%
    dplyr::left_join(f30, by = "doc_id") %>%
    dplyr::left_join(f31, by = "doc_id") %>%
    dplyr::left_join(f32, by = "doc_id") %>%
    dplyr::left_join(f33, by = "doc_id") %>%
    dplyr::left_join(f34, by = "doc_id") %>%
    dplyr::mutate(dplyr::across(-dplyr::any_of("doc_id"), ~ dplyr::coalesce(., 0L)))
}

#' Extract clause embedding and complementizer features
#'
#' @param tokens Annotated token data frame
#' @param doc_ids Document IDs
#' @param head_lookup Head token lookup table
#' @return Data frame with subordinator features f_21 through f_38, plus f_60
#' @keywords internal
block_clause_embedding_fr <- function(tokens, doc_ids, head_lookup) {
  complementizers <- c("que", "qu'", "qu\u2019")
  wh_lemmas <- c(
    "qui", "que", "quoi", "dont",
    "o\u00f9", "ou", "quand", "comment", "pourquoi", "combien",
    "lequel", "laquelle", "lesquels", "lesquelles",
    "auquel", "auxquels", "auxquelles",
    "duquel", "desquels", "desquelles"
  )
  parce_follow_tokens <- c("que", "qu'", "qu\u2019")
  because_single_tokens <- c("car", "puisque", "puisqu'", "puisqu\u2019", "comme")

  f21 <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      .data$token %in% complementizers,
      .data$pos == "SCONJ",
      dplyr::lag(.data$pos) %in% c("VERB", "AUX")
    ) %>%
    dplyr::tally() %>%
    dplyr::rename(f_21_that_verb_comp = "n")

  f22 <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      .data$token %in% complementizers,
      .data$pos == "SCONJ",
      dplyr::lag(.data$pos) == "ADJ"
    ) %>%
    dplyr::tally() %>%
    dplyr::rename(f_22_that_adj_comp = "n")

  f23 <- tokens %>%
    dplyr::filter(
      .data$lemma %in% wh_lemmas,
      .data$pos %in% c("PRON", "ADV", "DET", "ADJ", "NOUN", "PROPN"),
      stringr::str_detect(
        dplyr::coalesce(.data$dep_rel, ""),
        "^(obj|obl|nsubj|iobj|expl|mark)"
      ),
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::left_join(
      head_lookup,
      by = c("doc_id", "sentence_id", "head_token_id_int" = "token_id_int")
    ) %>%
    dplyr::filter(.data$head_pos %in% c("VERB", "AUX", "ADJ")) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$token_id_int, .keep_all = TRUE) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_23_wh_clause = "n")

  f35 <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      (
        .data$token %in% because_single_tokens &
          .data$pos %in% c("SCONJ", "CCONJ") &
          stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^(mark|cc)")
      ) |
        (
          .data$token == "parce" &
            .data$next_token %in% parce_follow_tokens &
            .data$pos %in% c("SCONJ", "ADV")
        )
    ) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$token_id_int, .keep_all = TRUE) %>%
    dplyr::tally() %>%
    dplyr::rename(f_35_because = "n")

  f36 <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      (
        .data$token == "quoique" & .data$pos %in% c("SCONJ")
      ) |
        (
          .data$token == "bien" &
            .data$next_token %in% parce_follow_tokens &
            .data$next_pos %in% c("SCONJ")
        ) |
        (
          .data$token == "m\u00eame" &
            .data$next_token == "si" &
            .data$next_pos %in% c("SCONJ")
        )
    ) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$token_id_int, .keep_all = TRUE) %>%
    dplyr::tally() %>%
    dplyr::rename(f_36_though = "n")

  f37 <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      (
        (.data$token == "si" | .data$token == "s'") &
          .data$pos %in% c("SCONJ") &
          stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^mark")
      ) |
        (
          .data$token == "moins" &
            .data$prev_token %in% c("\u00e0", "au") &
            .data$next_token %in% parce_follow_tokens
        ) |
        (
          .data$token == "condition" &
            .data$prev_token == "\u00e0" &
            .data$next_token %in% parce_follow_tokens
        )
    ) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$token_id_int, .keep_all = TRUE) %>%
    dplyr::tally() %>%
    dplyr::rename(f_37_if = "n")

  counted_subordinators <- unique(c(
    complementizers,
    parce_follow_tokens,
    because_single_tokens,
    "parce",
    "quoique",
    "si"
  ))

  f38 <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      .data$pos %in% c("SCONJ", "ADP", "ADV"),
      stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^mark"),
      !.data$token %in% counted_subordinators
    ) %>%
    dplyr::tally() %>%
    dplyr::rename(f_38_other_adv_sub = "n")

  clause_marks <- tokens %>%
    dplyr::filter(
      .data$dep_rel == "mark",
      .data$lemma %in% complementizers
    ) %>%
    dplyr::transmute(
      doc_id = .data$doc_id,
      sentence_id = .data$sentence_id,
      token_id_int = .data$head_token_id_int,
      has_mark = TRUE
    ) %>%
    dplyr::distinct()

  clause_deletions <- tokens %>%
    dplyr::filter(.data$dep_rel %in% c("ccomp", "xcomp")) %>%
    dplyr::left_join(
      head_lookup,
      by = c("doc_id", "sentence_id", "head_token_id_int" = "token_id_int")
    ) %>%
    dplyr::filter(.data$head_pos %in% c("VERB", "AUX", "ADJ")) %>%
    dplyr::left_join(
      clause_marks,
      by = c("doc_id", "sentence_id", "token_id_int")
    ) %>%
    dplyr::filter(is.na(.data$has_mark)) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$token_id_int)

  f60 <- clause_deletions %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_60_that_deletion = "n")

  doc_ids %>%
    dplyr::left_join(f21, by = "doc_id") %>%
    dplyr::left_join(f22, by = "doc_id") %>%
    dplyr::left_join(f23, by = "doc_id") %>%
    dplyr::left_join(f35, by = "doc_id") %>%
    dplyr::left_join(f36, by = "doc_id") %>%
    dplyr::left_join(f37, by = "doc_id") %>%
    dplyr::left_join(f38, by = "doc_id") %>%
    dplyr::left_join(f60, by = "doc_id") %>%
    dplyr::mutate(dplyr::across(-dplyr::any_of("doc_id"), ~ dplyr::coalesce(., 0L)))
}
