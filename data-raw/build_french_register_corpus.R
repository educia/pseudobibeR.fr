# Build French Register Corpus
# Creates a multi-register corpus of ~1.5-2M words across 6 text types
# Output: data-raw/corpus-data/french_register_corpus.rda

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(purrr)
  library(xml2)
  library(tibble)
})

set.seed(12345)

# Helper function to read file with encoding detection
read_file_smart <- function(file_path) {
  # Guess encoding
  encoding_guess <- guess_encoding(file_path, n_max = 1000)
  
  if (nrow(encoding_guess) > 0) {
    # Try the most confident encoding first
    best_encoding <- encoding_guess$encoding[1]
    
    text <- tryCatch(
      read_lines(file_path, locale = locale(encoding = best_encoding)),
      error = function(e) {
        # Fallback to common French encodings
        tryCatch(
          read_lines(file_path, locale = locale(encoding = "ISO-8859-1")),
          error = function(e2) {
            tryCatch(
              read_lines(file_path, locale = locale(encoding = "windows-1252")),
              error = function(e3) {
                read_lines(file_path, locale = locale(encoding = "UTF-8"))
              }
            )
          }
        )
      }
    )
  } else {
    # If guess fails, try UTF-8 first, then fallbacks
    text <- tryCatch(
      read_lines(file_path, locale = locale(encoding = "UTF-8")),
      error = function(e) {
        tryCatch(
          read_lines(file_path, locale = locale(encoding = "ISO-8859-1")),
          error = function(e2) {
            read_lines(file_path, locale = locale(encoding = "windows-1252"))
          }
        )
      }
    )
  }
  
  return(text)
}

# Helper function to clean text
clean_text <- function(text) {
  text %>%
    # Remove multiple spaces
    str_replace_all("\\s+", " ") %>%
    # Remove leading/trailing whitespace
    str_trim() %>%
    # Remove empty strings
    .[nchar(.) > 0]
}

# Helper function to count words
count_words <- function(text) {
  sum(str_count(text, "\\S+"))
}

# 1. ACADEMIC ----
process_academic <- function(target_words = 300000) {
  cat("Processing ACADEMIC corpus...\n")
  
  parquet_path <- "CORPUS-FR/ACADEMIC/french_corpus.parquet"
  
  if (!file.exists(parquet_path)) {
    warning("Academic corpus not found")
    return(tibble(doc_id = character(), register = character(), text = character()))
  }
  
  academic <- arrow::read_parquet(parquet_path) %>%
    mutate(
      doc_id = paste0("ACAD_", doc_id),
      register = "academic",
      word_count = str_count(text, "\\S+")
    ) %>%
    select(doc_id, register, text, word_count)
  
  # Sample to reach target
  total_words <- sum(academic$word_count)
  if (total_words > target_words) {
    sample_prop <- target_words / total_words
    academic <- academic %>%
      slice_sample(prop = sample_prop) %>%
      select(-word_count)
  }
  
  cat(sprintf("  - %d documents, ~%s words\n", 
              nrow(academic), 
              format(count_words(academic$text), big.mark = ",")))
  
  academic %>% select(doc_id, register, text)
}

# 2. FICTION ----
process_fiction <- function(target_words = 300000, lines_per_file = 150) {
  cat("Processing FICTION corpus...\n")
  
  fiction_dir <- "CORPUS-FR/FICTION"
  files <- list.files(fiction_dir, pattern = "\\.txt$", full.names = TRUE)
  
  fiction <- map_dfr(files, function(f) {
    all_lines <- read_file_smart(f)
    
    # Sample lines from this file to get diverse excerpts
    n_available <- length(all_lines)
    n_sample <- min(lines_per_file, n_available)
    
    if (n_available > lines_per_file) {
      sampled_lines <- sample(all_lines, n_sample)
    } else {
      sampled_lines <- all_lines
    }
    
    text <- paste(sampled_lines, collapse = " ")
    
    tibble(
      doc_id = paste0("FICT_", tools::file_path_sans_ext(basename(f))),
      register = "fiction",
      text = text
    )
  })
  
  # Clean and adjust if needed
  fiction <- fiction %>%
    mutate(
      text = clean_text(text),
      word_count = str_count(text, "\\S+")
    )
  
  # If still over target, remove some documents
  total_words <- sum(fiction$word_count)
  if (total_words > target_words) {
    sample_prop <- target_words / total_words
    fiction <- fiction %>%
      slice_sample(prop = sample_prop)
  }
  
  cat(sprintf("  - %d documents (sampled from %d files), ~%s words\n", 
              nrow(fiction),
              length(files),
              format(count_words(fiction$text), big.mark = ",")))
  
  fiction %>% select(doc_id, register, text)
}

# 3. NEWS ----
process_news <- function(target_words = 300000) {
  cat("Processing NEWS corpus...\n")
  
  news_dir <- "CORPUS-FR/NEWS"
  files <- list.files(news_dir, pattern = "\\.txt$", full.names = TRUE, recursive = TRUE)
  
  news <- map_dfr(files, function(f) {
    text <- read_file_smart(f)
    text <- paste(text, collapse = " ")
    
    # Extract newspaper from path
    newspaper <- str_extract(f, "(L'Humanité|Le Monde|La Dépêche)")
    
    tibble(
      doc_id = paste0("NEWS_", tools::file_path_sans_ext(basename(f))),
      register = "news",
      source = newspaper,
      text = text
    )
  })
  
  # Clean and sample
  news <- news %>%
    mutate(
      text = clean_text(text),
      word_count = str_count(text, "\\S+")
    )
  
  total_words <- sum(news$word_count)
  if (total_words > target_words) {
    sample_prop <- target_words / total_words
    news <- news %>%
      slice_sample(prop = sample_prop)
  }
  
  cat(sprintf("  - %d documents, ~%s words\n", 
              nrow(news), 
              format(count_words(news$text), big.mark = ",")))
  
  news %>% select(doc_id, register, text)
}

# 4. SPOKEN ----
process_spoken <- function(target_words = 300000) {
  cat("Processing SPOKEN corpus...\n")
  
  spoken_dir <- "CORPUS-FR/SPOKEN/TEIP5"
  files <- list.files(spoken_dir, pattern = "\\.xml$", full.names = TRUE)
  
  spoken <- map_dfr(files, function(f) {
    tryCatch({
      doc <- read_xml(f)
      
      # Extract utterances (u elements)
      utterances <- xml_find_all(doc, ".//d1:u", xml_ns(doc))
      
      if (length(utterances) == 0) {
        # Try without namespace
        utterances <- xml_find_all(doc, ".//u")
      }
      
      text <- utterances %>%
        map_chr(xml_text) %>%
        paste(collapse = " ")
      
      # Get title
      title <- xml_find_first(doc, ".//d1:title", xml_ns(doc))
      if (is.na(title)) {
        title <- xml_find_first(doc, ".//title")
      }
      title_text <- if (!is.na(title)) xml_text(title) else basename(f)
      
      tibble(
        doc_id = paste0("SPOK_", tools::file_path_sans_ext(basename(f))),
        register = "spoken",
        title = title_text,
        text = text
      )
    }, error = function(e) {
      warning(sprintf("Error parsing %s: %s", basename(f), e$message))
      tibble(
        doc_id = character(),
        register = character(),
        title = character(),
        text = character()
      )
    })
  })
  
  # Clean and sample
  spoken <- spoken %>%
    filter(nchar(text) > 0) %>%
    mutate(
      text = clean_text(text),
      word_count = str_count(text, "\\S+")
    )
  
  total_words <- sum(spoken$word_count)
  if (total_words > target_words) {
    sample_prop <- target_words / total_words
    spoken <- spoken %>%
      slice_sample(prop = sample_prop)
  }
  
  cat(sprintf("  - %d documents, ~%s words\n", 
              nrow(spoken), 
              format(count_words(spoken$text), big.mark = ",")))
  
  spoken %>% select(doc_id, register, text)
}

# 5. TWITTER ----
process_twitter <- function(target_words = 250000) {
  cat("Processing TWITTER corpus...\n")
  
  twitter_file <- "CORPUS-FR/TWITTER/tweets_unique.csv"
  
  if (!file.exists(twitter_file)) {
    warning("Twitter corpus not found")
    return(tibble(doc_id = character(), register = character(), text = character()))
  }
  
  twitter <- read_csv(twitter_file, show_col_types = FALSE) %>%
    mutate(
      doc_id = paste0("TWTR_", row_number()),
      register = "twitter",
      text = as.character(text)
    ) %>%
    filter(!is.na(text), nchar(text) > 0)
  
  # Clean
  twitter <- twitter %>%
    mutate(
      text = clean_text(text),
      word_count = str_count(text, "\\S+")
    ) %>%
    filter(word_count > 3)  # Remove very short tweets
  
  # Sample to target
  total_words <- sum(twitter$word_count)
  if (total_words > target_words) {
    sample_prop <- target_words / total_words
    twitter <- twitter %>%
      slice_sample(prop = sample_prop)
  }
  
  cat(sprintf("  - %d documents, ~%s words\n", 
              nrow(twitter), 
              format(count_words(twitter$text), big.mark = ",")))
  
  twitter %>% select(doc_id, register, text)
}

# 6. WIKIPEDIA ----
process_wikipedia <- function(target_words = 300000) {
  cat("Processing WIKIPEDIA corpus...\n")
  
  wiki_file <- "CORPUS-FR/WIKIPEDIA/frwiki.txt"
  
  if (!file.exists(wiki_file)) {
    warning("Wikipedia corpus not found")
    return(tibble(doc_id = character(), register = character(), text = character()))
  }
  
  # Read in chunks to avoid memory issues
  wiki_lines <- read_lines(wiki_file, n_max = 100000)
  
  # Split into articles (assuming blank lines separate articles)
  article_breaks <- which(wiki_lines == "" | str_length(wiki_lines) < 5)
  
  # Create article chunks
  articles <- list()
  start_idx <- 1
  
  for (i in seq_along(article_breaks)) {
    end_idx <- article_breaks[i] - 1
    if (end_idx > start_idx) {
      article_text <- paste(wiki_lines[start_idx:end_idx], collapse = " ")
      if (nchar(article_text) > 100) {  # Skip very short chunks
        articles <- c(articles, article_text)
      }
    }
    start_idx <- article_breaks[i] + 1
  }
  
  wikipedia <- tibble(
    doc_id = paste0("WIKI_", seq_along(articles)),
    register = "wikipedia",
    text = unlist(articles)
  ) %>%
    mutate(
      text = clean_text(text),
      word_count = str_count(text, "\\S+")
    )
  
  # Sample to target
  total_words <- sum(wikipedia$word_count)
  if (total_words > target_words) {
    sample_prop <- target_words / total_words
    wikipedia <- wikipedia %>%
      slice_sample(prop = sample_prop)
  }
  
  cat(sprintf("  - %d documents, ~%s words\n", 
              nrow(wikipedia), 
              format(count_words(wikipedia$text), big.mark = ",")))
  
  wikipedia %>% select(doc_id, register, text)
}

# MAIN EXECUTION ----
cat("Building French Register Corpus\n")
cat("================================\n\n")

# Process each register
academic <- process_academic(target_words = 300000)
fiction <- process_fiction(target_words = 300000)
news <- process_news(target_words = 300000)
spoken <- process_spoken(target_words = 300000)
twitter <- process_twitter(target_words = 250000)
wikipedia <- process_wikipedia(target_words = 300000)

# Combine all
french_register_corpus <- bind_rows(
  academic,
  fiction,
  news,
  spoken,
  twitter,
  wikipedia
) %>%
  mutate(register = factor(register, levels = c("academic", "fiction", "news", "spoken", "twitter", "wikipedia")))

# Summary
cat("\n================================\n")
cat("Corpus Summary:\n")
cat("================================\n")

summary_stats <- french_register_corpus %>%
  group_by(register) %>%
  summarise(
    n_docs = n(),
    total_words = count_words(text),
    mean_words = mean(str_count(text, "\\S+")),
    .groups = "drop"
  )

print(summary_stats)

cat("\nTotal documents:", nrow(french_register_corpus), "\n")
cat("Total words:", format(count_words(french_register_corpus$text), big.mark = ","), "\n")

# Save
output_file <- "data-raw/corpus-data/french_register_corpus.rda"
save(french_register_corpus, file = output_file)
cat("\nSaved to:", output_file, "\n")
