# Stance feature disambiguation
# Handles overlapping lexical items between feature categories

#' Disambiguate 'donc' between conjuncts and discourse particles
#'
#' Identifies discourse particle uses of 'donc' based on syntactic position.
#' Discourse particles typically appear sentence-initially, sentence-finally,
#' or in questions. Conjuncts appear mid-sentence after punctuation.
#'
#' @param tokens Annotated token data frame
#' @param doc_ids Document IDs tibble
#' @return Data frame with donc_as_discourse_particle count per doc
#' @keywords internal
count_donc_discourse_fr <- function(tokens, doc_ids) {
  
  # Calculate sentence position for each token
  sentence_info <- tokens %>%
    dplyr::group_by(.data$doc_id, .data$sentence_id) %>%
    dplyr::mutate(
      sentence_length = dplyr::n(),
      position_in_sentence = dplyr::row_number(),
      is_sentence_initial = .data$position_in_sentence <= 2,
      is_sentence_final = .data$position_in_sentence > (.data$sentence_length - 2),
      sentence_has_question = any(.data$token == "?" & .data$pos == "PUNCT")
    ) %>%
    dplyr::ungroup()
  
  # Identify 'donc' tokens that function as discourse particles
  donc_discourse <- sentence_info %>%
    dplyr::filter(.data$lemma == "donc") %>%
    dplyr::filter(
      .data$is_sentence_initial |   # Sentence-initial: "Donc, ..."
      .data$is_sentence_final |     # Sentence-final: "... donc."
      .data$sentence_has_question   # In questions: "C'est quoi donc?"
    ) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(donc_as_discourse_particle = "n")
  
  # Join with all doc_ids to ensure complete coverage
  doc_ids %>%
    dplyr::left_join(donc_discourse, by = "doc_id") %>%
    dplyr::mutate(donc_as_discourse_particle = dplyr::coalesce(.data$donc_as_discourse_particle, 0L)) %>%
    dplyr::select("doc_id", "donc_as_discourse_particle")
}

#' Disambiguate 'ensuite' between conjuncts and time adverbials
#'
#' Identifies conjunct uses of 'ensuite' based on syntactic position.
#' Conjuncts typically appear after commas in mid-sentence position,
#' marking logical sequence. Time adverbials mark temporal sequence.
#'
#' @param tokens Annotated token data frame
#' @param doc_ids Document IDs tibble
#' @return Data frame with ensuite_as_conjunct count per doc
#' @keywords internal
count_ensuite_conjunct_fr <- function(tokens, doc_ids) {
  
  # Calculate sentence position for each token
  sentence_info <- tokens %>%
    dplyr::group_by(.data$doc_id, .data$sentence_id) %>%
    dplyr::mutate(
      position_in_sentence = dplyr::row_number(),
      is_sentence_initial = .data$position_in_sentence <= 2,
      prev_is_comma = dplyr::lag(.data$token, default = "") %in% c(",", ";")
    ) %>%
    dplyr::ungroup()
  
  # Identify 'ensuite' tokens that function as conjuncts
  ensuite_conjunct <- sentence_info %>%
    dplyr::filter(.data$lemma == "ensuite") %>%
    dplyr::filter(
      .data$prev_is_comma,        # After comma
      !.data$is_sentence_initial  # Not sentence-initial
    ) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(ensuite_as_conjunct = "n")
  
  # Join with all doc_ids to ensure complete coverage
  doc_ids %>%
    dplyr::left_join(ensuite_conjunct, by = "doc_id") %>%
    dplyr::mutate(ensuite_as_conjunct = dplyr::coalesce(.data$ensuite_as_conjunct, 0L)) %>%
    dplyr::select("doc_id", "ensuite_as_conjunct")
}

#' Disambiguate 'vraiment' between amplifiers and emphatics
#'
#' Identifies amplifier uses of 'vraiment' based on adjacency to adjectives/adverbs.
#' Amplifiers modify degree ("vraiment grand" = "really big").
#' Emphatics provide standalone emphasis ("Je l'ai vraiment fait" = "I really did it").
#'
#' @param tokens Annotated token data frame
#' @param doc_ids Document IDs tibble
#' @return Data frame with vraiment_as_amplifier count per doc
#' @keywords internal
count_vraiment_amplifier_fr <- function(tokens, doc_ids) {
  
  # Add adjacent POS tags for context
  tokens_with_context <- tokens %>%
    dplyr::group_by(.data$doc_id, .data$sentence_id) %>%
    dplyr::mutate(
      next_pos = dplyr::lead(.data$pos, default = ""),
      prev_pos = dplyr::lag(.data$pos, default = "")
    ) %>%
    dplyr::ungroup()
  
  # Identify 'vraiment' tokens that function as amplifiers
  vraiment_amplifier <- tokens_with_context %>%
    dplyr::filter(.data$lemma == "vraiment") %>%
    dplyr::filter(
      .data$next_pos %in% c("ADJ", "ADV") |  # Modifies following ADJ/ADV
      .data$prev_pos %in% c("ADJ", "ADV")    # Modifies preceding ADJ/ADV
    ) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(vraiment_as_amplifier = "n")
  
  # Join with all doc_ids to ensure complete coverage
  doc_ids %>%
    dplyr::left_join(vraiment_amplifier, by = "doc_id") %>%
    dplyr::mutate(vraiment_as_amplifier = dplyr::coalesce(.data$vraiment_as_amplifier, 0L)) %>%
    dplyr::select("doc_id", "vraiment_as_amplifier")
}

#' Supplement pronoun counts with morphological features
#'
#' Dictionary-based pronoun extraction may miss tokens when udpipe fails to
#' provide full morphological features. This function supplements dictionary
#' counts by using the Person morphological feature as a fallback, similar
#' to the f_10 demonstrative pronoun fix.
#'
#' @param tokens Annotated token data frame
#' @param doc_ids Document IDs tibble
#' @return Data frame with supplementary counts for f_06, f_07, f_08
#' @keywords internal
supplement_pronouns_morphological_fr <- function(tokens, doc_ids) {
  
  # Extract pronouns using morphological Person feature as fallback
  # Accept tokens that are tagged as PRON with Person feature, even if not in dictionary
  pronoun_morph <- tokens %>%
    dplyr::filter(
      .data$pos == "PRON",
      !is.na(.data$morph_person)
    )
  
  # First person: Person=1
  f06_morph <- pronoun_morph %>%
    dplyr::filter(.data$morph_person == "1") %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_06_morph_supplement = "n")
  
  # Second person: Person=2
  f07_morph <- pronoun_morph %>%
    dplyr::filter(.data$morph_person == "2") %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_07_morph_supplement = "n")
  
  # Third person: Person=3
  f08_morph <- pronoun_morph %>%
    dplyr::filter(.data$morph_person == "3") %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_08_morph_supplement = "n")
  
  # Combine all supplements
  doc_ids %>%
    dplyr::left_join(f06_morph, by = "doc_id") %>%
    dplyr::left_join(f07_morph, by = "doc_id") %>%
    dplyr::left_join(f08_morph, by = "doc_id") %>%
    dplyr::mutate(
      f_06_morph_supplement = dplyr::coalesce(.data$f_06_morph_supplement, 0L),
      f_07_morph_supplement = dplyr::coalesce(.data$f_07_morph_supplement, 0L),
      f_08_morph_supplement = dplyr::coalesce(.data$f_08_morph_supplement, 0L)
    ) %>%
    dplyr::select("doc_id", "f_06_morph_supplement", "f_07_morph_supplement", "f_08_morph_supplement")
}
