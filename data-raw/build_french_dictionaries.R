"%||%" <- function(x, y) {
  if (is.null(x) || (is.vector(x) && length(x) == 0)) {
    return(y)
  }
  x
}

# Build dictionary and word list objects for pseudobibeR.fr
# Run via: source("data-raw/build_french_dictionaries.R") from the package root.

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(stringr)
  library(tibble)
  library(quanteda)
  library(yaml)
})

normalize_terms <- function(values, replace_spaces = TRUE) {
  if (is.null(values) || length(values) == 0) {
    return(character())
  }

  cleaned <- values %>%
    purrr::map_chr(as.character) %>%
    stringr::str_to_lower() %>%
    stringr::str_squish()

  cleaned <- cleaned[nzchar(cleaned)]

  if (replace_spaces) {
    cleaned <- stringr::str_replace_all(cleaned, "\\s+", "_")
  }

  sort(unique(cleaned))
}

normalize_multiword_patterns <- function(values) {
  if (is.null(values) || length(values) == 0) {
    return(character())
  }

  normalized <- normalize_terms(values, replace_spaces = FALSE)
  normalized <- normalized[stringr::str_detect(normalized, "_|\\s")]

  if (length(normalized) == 0) {
    return(character())
  }

  normalized %>%
    stringr::str_replace_all("_", " ") %>%
    stringr::str_squish() %>%
    unique() %>%
    sort()
}

dict_path <- file.path("data-raw", "dict.yaml")
if (!file.exists(dict_path)) {
  stop("Cannot find dictionary specification at ", dict_path)
}

dict_spec <- yaml::read_yaml(dict_path)
if (!is.list(dict_spec)) {
  stop("Dictionary specification must be a named list")
}

dict_list <- dict_spec %>%
  purrr::imap(function(values, feature) {
    normalize_terms(values, replace_spaces = TRUE)
  }) %>%
  purrr::compact()

dict_list <- dict_list[order(names(dict_list))]
dict <- quanteda::dictionary(dict_list)

dict_multiword_patterns <- normalize_multiword_patterns(unlist(dict_list, use.names = FALSE))

word_lists_path <- file.path("data-raw", "word_lists.yaml")
if (!file.exists(word_lists_path)) {
  stop("Cannot find word list specification at ", word_lists_path)
}

word_lists_spec <- yaml::read_yaml(word_lists_path)
if (!is.list(word_lists_spec)) {
  stop("Word list specification must be a named list")
}

manual_multiword_patterns <- normalize_multiword_patterns(word_lists_spec$multiword_patterns)
multiword_patterns <- sort(unique(c(dict_multiword_patterns, manual_multiword_patterns)))

word_lists <- list(
  pronoun_matchlist = normalize_terms(word_lists_spec$pronoun_matchlist, replace_spaces = TRUE),
  proverb_object_pronouns = normalize_terms(word_lists_spec$proverb_object_pronouns, replace_spaces = TRUE),
  impersonal_verbs = normalize_terms(word_lists_spec$impersonal_verbs, replace_spaces = TRUE),
  linking_matchlist = normalize_terms(word_lists_spec$linking_matchlist, replace_spaces = TRUE),
  verb_matchlist = normalize_terms(word_lists_spec$verb_matchlist, replace_spaces = TRUE),
  nominalization_stoplist = normalize_terms(word_lists_spec$nominalization_stoplist, replace_spaces = TRUE),
  gerund_stoplist = normalize_terms(word_lists_spec$gerund_stoplist, replace_spaces = TRUE),
  nominalization_suffixes = normalize_terms(word_lists_spec$nominalization_suffixes, replace_spaces = FALSE),
  multiword_patterns = multiword_patterns,
  neg_synthetic_determiners = normalize_terms(word_lists_spec$neg_synthetic_determiners, replace_spaces = TRUE),
  negation_particles = normalize_terms(word_lists_spec$negation_particles, replace_spaces = TRUE),
  neg_analytic_adverbs = normalize_terms(word_lists_spec$neg_analytic_adverbs, replace_spaces = TRUE)
)

examples_path <- file.path("data-raw", "french_examples.yaml")
french_examples <- tibble::tibble(feature = character(), example = character(), count = double())

if (file.exists(examples_path)) {
  examples_spec <- yaml::read_yaml(examples_path)
  if (!is.null(examples_spec)) {
    french_examples <- examples_spec %>%
      purrr::map_dfr(function(entry) {
        tibble::tibble(
          feature = entry$feature %||% NA_character_,
          example = entry$example %||% NA_character_,
          count = as.numeric(entry$count %||% NA_real_)
        )
      }) %>%
      dplyr::mutate(
        feature = stringr::str_squish(stringr::str_to_lower(.data$feature)),
        example = stringr::str_squish(.data$example)
      ) %>%
      dplyr::filter(!is.na(.data$feature), nzchar(.data$feature))

    french_examples <- french_examples %>%
      dplyr::arrange(.data$feature, .data$example) %>%
      dplyr::mutate(count = dplyr::coalesce(.data$count, 1))
  }
}

data_dir <- "data"
if (!dir.exists(data_dir)) {
  dir.create(data_dir, recursive = TRUE)
}

save(dict, file = file.path(data_dir, "dict.rda"), compress = "xz")
save(word_lists, file = file.path(data_dir, "word_lists.rda"), compress = "xz")
save(french_examples, file = file.path(data_dir, "french_examples.rda"), compress = "xz")

message("French dictionary resources saved to data/ directory.")
