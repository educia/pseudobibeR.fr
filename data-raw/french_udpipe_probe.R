# Quick probe of UDpipe annotations for French sentences.
# Run via: source("data-raw/french_udpipe_probe.R") from the package root.

if (!requireNamespace("udpipe", quietly = TRUE)) {
  stop("Package 'udpipe' must be installed to run this probe.")
}

needed_pkgs <- c("dplyr", "tibble", "stringr", "tidyr", "readr")
missing_pkgs <- needed_pkgs[!vapply(needed_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop("Install required packages first: ", paste(missing_pkgs, collapse = ", "))
}

model_path <- file.path("..", "tests", "french-gsd-ud-2.5-191206.udpipe")
if (!file.exists(model_path)) {
  stop("Could not find UD model at ", model_path)
}

sample_text <- c(
  "Hier, les enfants ont mangé le gâteau.",
  "Il sera intéressant de voir ce que tu fais.",
  "Il y a trois pommes sur la table.",
  "La maison construite en 1890 se trouve au centre-ville.",
  "Si tu viens, nous pourrons commencer.",
  "Le chat est poursuivi par le chien.",
  "En travaillant dur, elle a réussi.",
  "C'est l'homme dont je t'ai parlé.",
  "Bien que fatigués, ils continuent.",
  "Il ne vient pas."
)

docs <- tibble::tibble(
  doc_id = sprintf("fr_probe_%02d", seq_along(sample_text)),
  text = sample_text
)

model <- udpipe::udpipe_load_model(model_path)
annotation <- udpipe::udpipe_annotate(
  model,
  x = docs$text,
  doc_id = docs$doc_id,
  parser = "default"
)
annotation_df <- as.data.frame(annotation, stringsAsFactors = FALSE)

annotation_tbl <- tibble::as_tibble(annotation_df) |>
  dplyr::select(doc_id, sentence_id, token_id, token, lemma, upos, xpos, feats, head_token_id, dep_rel)

clean_feats <- function(x) {
  out <- x
  out[is.na(out)] <- ""
  out
}

verb_snapshot <- annotation_tbl |>
  dplyr::filter(upos %in% c("VERB", "AUX")) |>
  dplyr::mutate(feats = clean_feats(feats)) |>
  dplyr::mutate(
    verbform = stringr::str_extract(feats, "VerbForm=[A-Za-z]+"),
    tense = stringr::str_extract(feats, "Tense=[A-Za-z]+"),
    mood = stringr::str_extract(feats, "Mood=[A-Za-z]+"),
    voice = stringr::str_extract(feats, "Voice=[A-Za-z]+"),
    aux_pass = dep_rel %in% c("aux:pass", "cop")
  )

rel_pronouns <- annotation_tbl |>
  dplyr::filter(
    stringr::str_to_lower(lemma) %in% c(
      "que", "qui", "dont", "ou",
      "lequel", "laquelle", "lesquels", "lesquelles"
    )
  ) |>
  dplyr::mutate(feats = clean_feats(feats)) |>
  dplyr::select(doc_id, sentence_id, token_id, token, lemma, upos, feats, dep_rel)

participle_phrases <- annotation_tbl |>
  dplyr::filter(stringr::str_detect(clean_feats(feats), "VerbForm=(Part|Ger)")) |>
  dplyr::select(doc_id, sentence_id, token_id, token, lemma, upos, feats, dep_rel)

output_dir <- file.path("data-raw", "probe-output")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

readr::write_csv(annotation_tbl, file.path(output_dir, "french_udpipe_probe_tokens.csv"))
readr::write_csv(verb_snapshot, file.path(output_dir, "french_udpipe_probe_verbs.csv"))
readr::write_csv(rel_pronouns, file.path(output_dir, "french_udpipe_probe_relatives.csv"))
readr::write_csv(participle_phrases, file.path(output_dir, "french_udpipe_probe_participles.csv"))

message("Probe complete. Inspect CSV files in ", normalizePath(output_dir, mustWork = FALSE))
