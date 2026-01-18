#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(yaml)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
})

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("pkgload needed to load package code")
}

if (!requireNamespace("udpipe", quietly = TRUE)) {
  stop("udpipe package required")
}

pkgload::load_all(export_all = FALSE, helpers = FALSE, quiet = TRUE)

edge_path <- file.path("data-raw", "french_edge_cases.yaml")
edge_data <- yaml::read_yaml(edge_path)
edge_cases <- edge_data$edge_cases

edge_df <- purrr::imap_dfr(edge_cases, function(sentences, category) {
  tibble::tibble(
    category = category,
    text = unname(unlist(sentences)),
    doc_id = stringr::str_c(
      category,
      "_",
      stringr::str_pad(seq_along(sentences), width = 2, pad = "0")
    )
  )
}) %>%
  dplyr::arrange(.data$category, .data$doc_id)

model_candidates <- c(
  file.path("tests", "french-gsd-ud-2.5-191206.udpipe"),
  file.path("tests", "models", "french-gsd-ud-2.5-191206.udpipe")
)
existing_models <- model_candidates[file.exists(model_candidates)]
if (length(existing_models) == 0) {
  stop("UDPipe model missing; place it under tests/ or tests/models/")
}
model_path <- existing_models[[1]]

model <- udpipe::udpipe_load_model(model_path)
on.exit({
  if ("udpipe_free_model" %in% getNamespaceExports("udpipe")) {
    udpipe::udpipe_free_model(model)
  }
}, add = TRUE)

annotations <- udpipe::udpipe_annotate(
  model,
  x = edge_df$text,
  doc_id = edge_df$doc_id,
  parser = "default"
)

features <- biber(annotations, measure = "none", normalize = FALSE)
feature_cols <- grep("^f_", names(features), value = TRUE)

long <- tidyr::pivot_longer(
  features,
  cols = dplyr::all_of(feature_cols),
  names_to = "feature",
  values_to = "count"
) %>%
  dplyr::filter(.data$count > 0)

output <- long %>%
  dplyr::left_join(edge_df, by = "doc_id") %>%
  dplyr::select(category, doc_id, feature, count) %>%
  dplyr::arrange(category, doc_id, feature)

output_path <- file.path("tests", "testthat", "fixtures", "french_edge_case_features.csv")
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
write.csv(output, output_path, row.names = FALSE)

message("Wrote ", nrow(output), " rows to ", output_path)
