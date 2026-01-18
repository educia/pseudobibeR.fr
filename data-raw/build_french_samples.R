# Build French parsing samples from the Chambers & LeBaron corpus
# Run via: source("data-raw/build_french_samples.R") from the package root.

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readr)
  library(stringr)
  library(reticulate)
})

source_root <- file.path("..", "ChambersLeBaron", "2527_Corpus_text_files")

discipline_dirs <- c(
  "Economics",
  "Linguistics",
  "Medicine",
  "Physics",
  "Sociology",
  "Social_anthropology"
)

set.seed(20231110)

collect_samples <- function(dir_name) {
  target_dir <- file.path(source_root, dir_name)
  if (!dir.exists(target_dir)) {
    return(character())
  }
  files <- list.files(target_dir, pattern = "\\.txt$", full.names = TRUE)
  if (length(files) <= 2) {
    return(files)
  }
  sample(files, size = 2)
}

candidate_files <- discipline_dirs %>%
  purrr::map(collect_samples) %>%
  unlist(use.names = FALSE) %>%
  unique()

read_utf8 <- function(path) {
  raw_bytes <- read_file_raw(path)
  guess <- suppressWarnings(guess_encoding(raw_bytes, n_max = 2000))
  chosen <- dplyr::coalesce(guess$encoding[1], "UTF-8")
  text <- read_file(path, locale = locale(encoding = chosen))
  text <- str_replace_all(text, "\r\n?", "\n")
  text <- str_replace_all(text, "\n+", "\n")
  text <- str_squish(text)
  str_trunc(text, 4000, ellipsis = "")
}

samples_tbl <- tibble::tibble(
  file = candidate_files,
  text = purrr::map_chr(candidate_files, read_utf8)
) %>%
  mutate(
    doc_id = file %>%
      tools::file_path_sans_ext() %>%
      basename()
  ) %>%
  select(doc_id, text) %>%
  mutate(text = str_squish(text))

samples_path <- file.path("data-raw", "french_raw_text.tsv")
write_tsv(samples_tbl, samples_path)

if (!requireNamespace("udpipe", quietly = TRUE)) {
  stop("Package 'udpipe' must be installed to build udpipe samples")
}

model_path <- file.path("..", "tests", "french-gsd-ud-2.5-191206.udpipe")
if (!file.exists(model_path)) {
  stop("Could not find UD model at ", normalizePath(model_path, mustWork = FALSE))
}

ud_model <- udpipe::udpipe_load_model(model_path)
udpipe_samples <- udpipe::udpipe_annotate(
  ud_model,
  x = samples_tbl$text,
  doc_id = samples_tbl$doc_id,
  parser = "default"
)

if (!requireNamespace("usethis", quietly = TRUE)) {
  stop("Package 'usethis' must be installed to save sample data")
}

usethis::use_data(udpipe_samples, overwrite = TRUE)

if (!requireNamespace("spacyr", quietly = TRUE)) {
  stop("Package 'spacyr' must be installed to build spaCy samples")
}

spacyr::spacy_initialize(model = "fr_core_news_sm")
corpus <- samples_tbl$text
names(corpus) <- samples_tbl$doc_id
spacy_samples <- spacyr::spacy_parse(
  corpus,
  pos = TRUE,
  tag = TRUE,
  dependency = TRUE,
  entity = FALSE,
  morph = TRUE,
  additional_attributes = "morph"
)

normalize_morph <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NA_character_)
  }
  if (py_is_null_xptr(x)) {
    return(NA_character_)
  }
  if (py_has_attr(x, "to_dict")) {
    values <- py_to_r(x$to_dict())
    if (length(values) == 0) {
      return(NA_character_)
    }
    pieces <- purrr::imap_chr(values, function(val, key) {
      if (length(val) == 0 || is.na(val)) {
        return(NA_character_)
      }
      paste0(key, "=", val)
    })
    pieces <- pieces[!is.na(pieces) & pieces != ""]
    if (length(pieces) == 0) {
      return(NA_character_)
    }
    return(paste(pieces, collapse = "|"))
  }
  as_char <- tryCatch(py_to_r(x), error = function(e) NULL)
  if (is.character(as_char) && length(as_char) > 0) {
    return(as_char[[1]])
  }
  NA_character_
}

if ("morph" %in% colnames(spacy_samples)) {
  spacy_samples <- spacy_samples %>%
    dplyr::mutate(morph = purrr::map_chr(.data$morph, normalize_morph))
}

usethis::use_data(spacy_samples, overwrite = TRUE)
spacyr::spacy_finalize()

message("French sample datasets updated: data/udpipe_samples.rda and data/spacy_samples.rda")
