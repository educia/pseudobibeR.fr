# Diagnostic script to analyze apostrophe patterns in parsed French corpora
# Goal: Find linguistic signals that distinguish elisions from contractions

library(dplyr)
library(stringr)
library(tidyr)
library(udpipe)  # Needed for as.data.frame() method on udpipe_connlu objects

# Known contractions (ground truth)
known_contractions <- c("t'es", "t'as", "p'tit", "p'tite", "p'tits", "p'tites", "m'sieur", "m'dame")

# Known elisions (ground truth)  
known_elisions <- c("c'est", "l'", "d'", "qu'il", "j'ai", "n'", "s'", "jusqu'", "lorsqu'", "aujourd'hui")

# Load parsed corpora
load("data-raw/corpus-data/fc_udpipe_parsed.rda")
fc_tokens <- as.data.frame(fc_udpipe_parsed)

load("data-raw/corpus-data/fr_udpipe_parsed.rda")
fr_tokens <- as.data.frame(fr_udpipe_parsed)

cat("=== ANALYZING APOSTROPHE PATTERNS IN FRENCH CORPORA ===\n\n")

# Function to analyze apostrophe tokens
analyze_apostrophes <- function(tokens, corpus_name) {
  cat("--- Corpus:", corpus_name, "---\n")
  
  apostrophe_pattern <- "[\\u2019']"
  
  # Find all tokens with apostrophes
  apos_tokens <- tokens %>%
    filter(str_detect(token, apostrophe_pattern)) %>%
    mutate(
      lower_token = str_to_lower(token),
      has_suffix = str_detect(lower_token, paste0(apostrophe_pattern, "[[:alpha:]]")),
      prefix = str_extract(lower_token, paste0("^[^", apostrophe_pattern, "]+")),
      suffix = str_extract(lower_token, paste0(apostrophe_pattern, "[[:alpha:]]+$")) %>%
        str_remove(apostrophe_pattern),
      type = case_when(
        lower_token %in% known_contractions ~ "contraction",
        str_detect(lower_token, paste(known_elisions, collapse = "|")) ~ "elision",
        TRUE ~ "unknown"
      ),
      # Normalize column names (udpipe uses 'upos', spacyr uses 'pos')
      pos_tag = if ("upos" %in% names(.)) upos else if ("pos" %in% names(.)) pos else NA_character_
    )
  
  cat("\nTotal apostrophe tokens:", nrow(apos_tokens), "\n")
  cat("With suffix (not terminal apostrophe):", sum(apos_tokens$has_suffix), "\n")
  
  # POS distribution
  cat("\n=== POS Distribution ===\n")
  pos_dist <- apos_tokens %>%
    count(pos_tag, sort = TRUE) %>%
    head(10)
  print(pos_dist)
  
  # Common patterns by POS
  cat("\n=== Common Token Patterns by POS ===\n")
  patterns_by_pos <- apos_tokens %>%
    count(pos_tag, lower_token, sort = TRUE) %>%
    group_by(pos_tag) %>%
    slice_head(n = 5) %>%
    ungroup()
  print(patterns_by_pos, n = 30)
  
  # Lemma patterns for known contractions
  cat("\n=== Known Contractions: Token → Lemma ===\n")
  contractions <- apos_tokens %>%
    filter(type == "contraction") %>%
    select(token, lemma, pos_tag, dep_rel, feats) %>%
    distinct() %>%
    arrange(token)
  if (nrow(contractions) > 0) {
    print(as.data.frame(contractions), row.names = FALSE)
  } else {
    cat("No known contractions found in this corpus.\n")
  }
  
  # Lemma patterns for known elisions
  cat("\n=== Sample Elisions: Token → Lemma ===\n")
  elisions <- apos_tokens %>%
    filter(type == "elision") %>%
    select(token, lemma, pos_tag, dep_rel) %>%
    distinct() %>%
    arrange(token) %>%
    head(20)
  if (nrow(elisions) > 0) {
    print(as.data.frame(elisions), row.names = FALSE)
  } else {
    cat("No known elisions found.\n")
  }
  
  # Prefix analysis
  cat("\n=== Prefix Patterns (before apostrophe) ===\n")
  prefix_patterns <- apos_tokens %>%
    filter(!is.na(prefix)) %>%
    count(prefix, pos_tag, sort = TRUE) %>%
    head(20)
  print(as.data.frame(prefix_patterns), row.names = FALSE)
  
  # Check: Can we distinguish by lemma pattern?
  cat("\n=== Lemma Analysis: Does token prefix appear in lemma? ===\n")
  lemma_check <- apos_tokens %>%
    filter(has_suffix) %>%
    mutate(
      prefix_in_lemma = str_detect(lemma, paste0("^", prefix)),
      lemma_matches_token = (str_remove_all(lower_token, apostrophe_pattern) == lemma)
    ) %>%
    count(prefix_in_lemma, lemma_matches_token) %>%
    arrange(desc(n))
  print(as.data.frame(lemma_check), row.names = FALSE)
  
  cat("\n", rep("=", 70), "\n\n", sep = "")
  
  return(apos_tokens)
}

# Analyze Chambers corpus (academic)
fc_apos <- analyze_apostrophes(fc_tokens, "Chambers Corpus (academic, UDPipe)")

# Analyze French register corpus (twitter, fiction, etc.)
fr_apos <- analyze_apostrophes(fr_tokens, "French Register Corpus (multi-register, UDPipe)")

cat("=== COMPARISON: CONTRACTIONS vs ELISIONS ===\n\n")

# Compare linguistic properties
all_apos <- bind_rows(
  fc_apos %>% mutate(corpus = "chambers"),
  fr_apos %>% mutate(corpus = "examples")
) %>%
  filter(type %in% c("contraction", "elision"))

if (nrow(all_apos) > 0 && any(all_apos$type == "contraction")) {
  cat("POS comparison:\n")
  pos_comparison <- all_apos %>%
    count(type, pos_tag) %>%
    pivot_wider(names_from = type, values_from = n, values_fill = 0) %>%
    arrange(desc(if("contraction" %in% names(.)) elision + contraction else elision))
  print(as.data.frame(pos_comparison), row.names = FALSE)
  
  cat("\nDependency relation comparison:\n")
  dep_comparison <- all_apos %>%
    count(type, dep_rel) %>%
    pivot_wider(names_from = type, values_from = n, values_fill = 0) %>%
    arrange(desc(if("contraction" %in% names(.)) elision + contraction else elision))
  print(as.data.frame(dep_comparison), row.names = FALSE)
} else {
  cat("Note: No contractions found in these corpora for comparison.\n")
  cat("      (This is expected for formal/academic text.)\n\n")
  cat("Elision POS distribution:\n")
  elision_pos <- all_apos %>%
    filter(type == "elision") %>%
    count(pos_tag, sort = TRUE)
  print(as.data.frame(elision_pos), row.names = FALSE)
  
  cat("\nElision dependency relations:\n")
  elision_deps <- all_apos %>%
    filter(type == "elision") %>%
    count(dep_rel, sort = TRUE)
  print(as.data.frame(elision_deps), row.names = FALSE)
}
}

cat("\n=== RECOMMENDATIONS ===\n")
cat("1. Check if certain POS tags are exclusive to elisions\n")
cat("2. Check if dependency relations can distinguish patterns\n")
cat("3. Consider lemma-based rules (e.g., lemma contains full word)\n")
cat("4. Look for morphological features (feats) that might help\n")
