#' Main feature extraction orchestrator
#' 
#' This function coordinates all feature extraction by calling the modular
#' block functions and assembling the results.
#' 
#' @param tokens Annotated token data
#' @param measure Type-token ratio measure
#' @param normalize Whether to normalize counts
#' @param engine Parser engine ("spacy" or "udpipe")
#' @param language Language code for feature heuristics ("fr" for French,
#'   "es" for Spanish). Defaults to "fr" to preserve original behaviour.
#' @return Data frame of extracted features
#' 
#' @importFrom rlang .data :=
#' @importFrom utils tail
#' @keywords internal
parse_biber_features <- function(tokens, measure, normalize,
                                 engine = c("spacy", "udpipe"),
                                 language = c("fr", "es")) {
  engine   <- match.arg(engine)
  language <- match.arg(language)

  if (!is.data.frame(tokens)) stop("'tokens' must be a data frame.", call. = FALSE)
  if (nrow(tokens) == 0)      stop("'tokens' is empty.", call. = FALSE)
  if (!"doc_id" %in% colnames(tokens)) stop("'tokens' must have a 'doc_id' column.", call. = FALSE)

  dict_lookup       <- dict
  word_lists_lookup <- word_lists

  df <- list()

  tokens <- tokens %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(token = stringr::str_to_lower(.data$token)) %>%
    dplyr::mutate(pos   = dplyr::if_else(.data$token == "\n", "PUNCT", .data$pos)) %>%
    dplyr::filter(.data$pos != "SPACE")

  if (nrow(tokens) == 0) stop("No valid tokens found after filtering.", call. = FALSE)

  if ("morph" %in% colnames(tokens)) {
    tokens <- tokens %>%
      dplyr::mutate(morph = purrr::map_chr(.data$morph, function(x) {
        if (inherits(x, "python.builtin.object")) {
          if (requireNamespace("reticulate", quietly = TRUE)) {
            value <- reticulate::py_to_r(x)
            if (is.null(value) || length(value) == 0) return("")
            value <- as.character(value)
            return(if (length(value) == 1) value else paste(value, collapse = "|"))
          }
          return(as.character(x))
        }
        if (is.null(x) || length(x) == 0) return("")
        if (length(x) == 1) return(as.character(x))
        paste(as.character(x), collapse = "|")
      })) %>%
      dplyr::mutate(morph = dplyr::na_if(.data$morph, ""))

    tokens <- if ("feats" %in% colnames(tokens)) {
      dplyr::mutate(tokens, feats = dplyr::coalesce(.data$feats, .data$morph))
    } else {
      dplyr::mutate(tokens, feats = .data$morph)
    }
  }

  if (!"feats" %in% colnames(tokens)) tokens <- dplyr::mutate(tokens, feats = NA_character_)

  tokens <- tokens %>%
    dplyr::mutate(
      token_id_int         = suppressWarnings(as.integer(.data$token_id)),
      head_token_id_int    = suppressWarnings(as.integer(.data$head_token_id)),
      morph_tense          = extract_morph_value(.data$feats, "Tense"),
      morph_verbform       = extract_morph_value(.data$feats, "VerbForm"),
      morph_mood           = extract_morph_value(.data$feats, "Mood"),
      morph_prontype       = extract_morph_value(.data$feats, "PronType"),
      morph_voice          = extract_morph_value(.data$feats, "Voice"),
      morph_number         = extract_morph_value(.data$feats, "Number"),
      morph_person         = extract_morph_value(.data$feats, "Person")
    ) %>%
    dplyr::arrange(.data$doc_id, .data$sentence_id, .data$token_id_int)

  doc_ids <- tokens %>% dplyr::distinct(.data$doc_id)

  # --- Shared word-list lookups ------------------------------------------------
  proverb_pronouns      <- normalize_terms(get_word_list(word_lists_lookup, "proverb_object_pronouns"))
  neg_synthetic_terms   <- normalize_terms(get_word_list(word_lists_lookup, "neg_synthetic_determiners"))
  negation_particle_terms <- normalize_terms(get_word_list(word_lists_lookup, "negation_particles"))
  negation_part_lemmas  <- unique(c(negation_particle_terms, "n'", "n\u2019"))
  negation_adverbs      <- normalize_terms(get_word_list(word_lists_lookup, "neg_analytic_adverbs"))
  impersonal_verbs      <- normalize_terms(get_word_list(word_lists_lookup, "impersonal_verbs"))

  # --- Language-specific constants -------------------------------------------
  if (language == "es") {
    weather_lemmas <- unique(c(impersonal_verbs,
                               "llover", "nevar", "granizar", "lloviznar", "tronar",
                               "amanecer", "anochecer", "atardecer"))
    raising_verbs  <- c("parecer", "resultar", "continuar", "seguir",
                        "bastar", "convenir", "quedar")
    wh_question_lemmas <- c(
      "quien", "qui\u00e9n", "que", "qu\u00e9",
      "cual",  "cu\u00e1l",  "cuales", "cu\u00e1les",
      "donde", "d\u00f3nde",
      "cuando", "cu\u00e1ndo",
      "como",  "c\u00f3mo",
      "cuanto", "cu\u00e1nto", "cuanta", "cu\u00e1nta",
      "cuantos", "cu\u00e1ntos", "cuantas", "cu\u00e1ntas",
      "por_que", "por_qu\u00e9"
    )
    relative_pronoun_candidates <- c(
      "que", "quien", "quienes",
      "cual", "cuales",
      "cuyo", "cuya", "cuyos", "cuyas",
      "donde", "cuando", "como"
    )
    # Subject pronoun lemmas for Spanish (pro-drop language; explicit forms)
    subject_pron_lemmas <- c(
      "yo", "t\u00fa", "vos", "\u00e9l", "ella", "ello",
      "nosotros", "nosotras", "vosotros", "vosotras",
      "ellos", "ellas", "usted", "ustedes"
    )
  } else {
    # French defaults (preserved for language = "fr")
    weather_lemmas <- unique(c(impersonal_verbs, "bruiner", "tonner"))
    raising_verbs  <- c("sembler", "para\u00eetre", "demeurer", "rester", "suffire", "convenir")
    wh_question_lemmas <- c(
      "qui", "que", "quoi", "o\u00f9", "quand", "comment", "pourquoi",
      "lequel", "laquelle", "lesquels", "lesquelles",
      "quel", "quelle", "quels", "quelles", "combien"
    )
    relative_pronoun_candidates <- c(
      "qui", "que", "quoi", "o\u00f9", "dont",
      "lequel", "laquelle", "lesquel", "lesquelle", "lesquels", "lesquelles",
      "auquel", "auxquels", "auxquelles",
      "duquel", "desquels", "desquelles"
    )
    subject_pron_lemmas <- c(
      "je", "tu", "il", "elle", "on", "nous", "vous", "ils", "elles", "lui", "leur"
    )
  }

  # --- Shared context builders -----------------------------------------------
  de_markers <- tokens %>%
    dplyr::filter(
      .data$lemma == "de",
      .data$dep_rel %in% c("mark", "advmod", "case"),
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::transmute(.data$doc_id, .data$sentence_id,
                     head_token_id_int = .data$head_token_id_int,
                     has_de_marker = TRUE) %>%
    dplyr::distinct()

  que_markers <- tokens %>%
    dplyr::filter(
      .data$lemma %in% c("que", "qu'", "qu\u2019"),
      .data$dep_rel %in% c("mark", "expl", "obj"),
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::transmute(.data$doc_id, .data$sentence_id,
                     head_token_id_int = .data$head_token_id_int,
                     has_que_marker = TRUE) %>%
    dplyr::distinct()

  clause_complements <- tokens %>%
    dplyr::filter(
      .data$dep_rel %in% c("ccomp", "xcomp", "csubj", "advcl"),
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::transmute(.data$doc_id, .data$sentence_id,
                     head_token_id_int = .data$head_token_id_int,
                     has_clause_comp = TRUE) %>%
    dplyr::distinct()

  lexical_text <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::summarise(text = paste(.data$token, collapse = " "), .groups = "drop")

  if (nrow(lexical_text) > 0) {
    biber_tks <- quanteda::tokens(lexical_text$text, what = "word", remove_punct = FALSE)
    names(biber_tks) <- lexical_text$doc_id

    multiword_patterns <- get_word_list(word_lists_lookup, "multiword_patterns")
    if (length(multiword_patterns) > 0) {
      multi_phrases <- quanteda::phrase(multiword_patterns)
      biber_tks     <- quanteda::tokens_compound(biber_tks, pattern = multi_phrases)
    }

    biber_tks <- quanteda::tokens_tolower(biber_tks)

    biber_1 <- quanteda::tokens_lookup(biber_tks, dictionary = dict_lookup, nomatch = NULL) %>%
      quanteda::dfm() %>%
      quanteda::convert(to = "data.frame") %>%
      dplyr::as_tibble()

    if ("document" %in% colnames(biber_1)) biber_1 <- dplyr::rename(biber_1, doc_id = "document")
  } else {
    biber_1 <- tibble::tibble(doc_id = character())
  }

  # ---------------------------------------------------------------------------
  head_lookup <- tokens %>%
    dplyr::select(
      "doc_id", "sentence_id", "token_id_int",
      head_pos             = "pos",
      head_lemma           = "lemma",
      head_token           = "token",
      head_feats           = "feats",
      head_morph_verbform  = "morph_verbform",
      head_morph_voice     = "morph_voice",
      head_morph_tense     = "morph_tense"
    )

  if (language == "fr") {
    df[["auxiliary_tense"]] <- block_aux_tense_fr(
      tokens = tokens, doc_ids = doc_ids, head_lookup = head_lookup,
      proverb_pronouns = proverb_pronouns)
    df[["personal_pronouns"]] <- block_personal_pronouns_fr(
      tokens = tokens, doc_ids = doc_ids, head_lookup = head_lookup,
      de_markers = de_markers, que_markers = que_markers,
      clause_complements = clause_complements,
      weather_lemmas = weather_lemmas, raising_verbs = raising_verbs,
      wh_question_lemmas = wh_question_lemmas)
    df[["lexical_membership"]] <- block_lexical_membership_fr(
      tokens = tokens, doc_ids = doc_ids, word_lists_lookup = word_lists_lookup)
  } else if (language == "es") {
    place_advs  <- dictionary_to_lemmas(dict_lookup, "f_04_place_adverbials")
    time_advs   <- dictionary_to_lemmas(dict_lookup, "f_05_time_adverbials")
    indef_prons <- dictionary_to_lemmas(dict_lookup, "f_11_indefinite_pronoun")
    df[["tense_features"]] <- block_tense_es(
      tokens = tokens, doc_ids = doc_ids, head_lookup = head_lookup,
      place_adverbials    = place_advs,
      time_adverbials     = time_advs,
      indefinite_pronouns = indef_prons)
    df[["personal_pronouns"]] <- block_personal_pronouns_es(
      tokens = tokens, doc_ids = doc_ids, head_lookup = head_lookup,
      de_markers = de_markers, que_markers = que_markers,
      clause_complements = clause_complements,
      weather_lemmas = weather_lemmas, raising_verbs = raising_verbs,
      wh_question_lemmas = wh_question_lemmas)
    df[["lexical_membership"]] <- block_lexical_membership_es(
      tokens = tokens, doc_ids = doc_ids, word_lists_lookup = word_lists_lookup)
  }

  # --- French-only disambiguation helpers (no-ops for Spanish) ---------------
  df[["donc_disambiguation"]]    <- count_donc_discourse_fr(tokens = tokens, doc_ids = doc_ids)
  df[["ensuite_disambiguation"]] <- count_ensuite_conjunct_fr(tokens = tokens, doc_ids = doc_ids)
  df[["vraiment_disambiguation"]]<- count_vraiment_amplifier_fr(tokens = tokens, doc_ids = doc_ids)
  df[["pronoun_morph_supplement"]]<- supplement_pronouns_morphological_fr(tokens = tokens, doc_ids = doc_ids)

  # --- Passive agents --------------------------------------------------------
  passive_rel_values <- if (engine == "spacy") c("auxpass", "aux:pass") else "aux:pass"
  passive_agents     <- if (language == "fr") c("par") else c("por", "por_medio_de")

  tokens <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::arrange(.data$sentence_id, .data$token_id_int, .by_group = TRUE) %>%
    dplyr::mutate(
      passive_agent_next2 = dplyr::lead(.data$token %in% passive_agents, 2, default = FALSE),
      passive_agent_next3 = dplyr::lead(.data$token %in% passive_agents, 3, default = FALSE),
      is_infinitive = dplyr::if_else(
        .data$pos %in% c("VERB", "AUX") & !is.na(.data$morph_verbform),
        .data$morph_verbform == "Inf", FALSE),
      is_present_participle = dplyr::case_when(
        .data$tag == "VBG" ~ TRUE,
        .data$pos %in% c("VERB", "AUX") & !is.na(.data$morph_verbform) &
          .data$morph_verbform == "Ger" ~ TRUE,
        .data$pos %in% c("VERB", "AUX") & !is.na(.data$morph_verbform) &
          .data$morph_verbform == "Part" & .data$morph_tense %in% c("Pres", "Imp") ~ TRUE,
        TRUE ~ FALSE),
      is_past_participle = dplyr::case_when(
        .data$tag == "VBN" ~ TRUE,
        .data$pos %in% c("VERB", "AUX") & !is.na(.data$morph_verbform) &
          .data$morph_verbform == "Part" & .data$morph_tense %in% c("Past", "Pqp") ~ TRUE,
        .data$pos %in% c("VERB", "ADJ") &
          stringr::str_detect(stringr::str_to_lower(.data$token),
                              "(ado|ada|ados|adas|ido|ida|idos|idas)$") &
          stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""),
                              "^(acl|advcl|xcomp|ccomp|root)") ~ TRUE,
        TRUE ~ FALSE),
      is_relative_pronoun = dplyr::if_else(
        (!is.na(.data$morph_prontype) &
           stringr::str_detect(.data$morph_prontype, "Rel")) |
          (.data$lemma %in% relative_pronoun_candidates &
             stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""),
                                 "^(nsubj|obj|obl|iobj|expl|mark|acl)")),
        TRUE, FALSE),
      is_relative_subject = .data$is_relative_pronoun &
        stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "nsubj"),
      is_relative_object  = .data$is_relative_pronoun &
        (stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "obj") |
           (.data$lemma %in% c("cual", "cuales", "cuyo", "cuya", "cuyos", "cuyas") &
              stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^obl") &
              !dplyr::lag(.data$pos == "ADP", default = FALSE)) |
           (.data$lemma == "que" &
              stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^mark"))),
      prev_token  = dplyr::lag(.data$token),
      prev_lemma  = dplyr::lag(.data$lemma),
      prev_pos    = dplyr::lag(.data$pos),
      prev2_token = dplyr::lag(.data$token, 2),
      prev2_lemma = dplyr::lag(.data$lemma, 2),
      prev3_token = dplyr::lag(.data$token, 3),
      prev3_lemma = dplyr::lag(.data$lemma, 3),
      prev3_pos   = dplyr::lag(.data$pos, 3),
      next_token  = dplyr::lead(.data$token),
      next_lemma  = dplyr::lead(.data$lemma),
      next_pos    = dplyr::lead(.data$pos),
      next_dep_rel = dplyr::lead(.data$dep_rel),
      next2_token = dplyr::lead(.data$token, 2),
      next2_lemma = dplyr::lead(.data$lemma, 2),
      next2_pos   = dplyr::lead(.data$pos, 2),
      next2_morph_verbform = dplyr::lead(.data$morph_verbform, 2),
      prev_morph_verbform  = dplyr::lag(.data$morph_verbform),
      next_morph_verbform  = dplyr::lead(.data$morph_verbform)
    ) %>%
    dplyr::ungroup()

  token_lookup <- tokens %>%
    dplyr::select(
      "doc_id", "sentence_id", "token_id_int",
      token_pos             = "pos",
      token_dep_rel         = "dep_rel",
      token_head_token_id_int = "head_token_id_int",
      token_morph_verbform  = "morph_verbform")

  subject_heads <- tokens %>%
    dplyr::filter(
      stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^nsubj") |
        (stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^(obj|iobj)") &
           .data$pos == "PRON" & .data$lemma %in% subject_pron_lemmas)
    ) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id,
                    clause_head_token_id_int = .data$head_token_id_int) %>%
    dplyr::mutate(has_subject = TRUE)

  if (language == "fr") {
    df[["passive_voice"]]     <- block_passive_voice_fr(tokens, doc_ids, head_lookup, passive_rel_values)
    df[["clause_embedding"]]  <- block_clause_embedding_fr(tokens, doc_ids, head_lookup)
    df[["participial_clauses"]]<- block_participial_clauses_fr(tokens, doc_ids)
    df[["relative_clauses"]]  <- block_relatives_fr(tokens, doc_ids)
    df[["contractions"]]      <- block_contractions_fr(tokens, doc_ids)
    df[["adj_prep_adverbs"]]  <- block_adj_prep_adv_fr(tokens, doc_ids, dict_lookup, word_lists_lookup, negation_adverbs)
    df[["specialized_verbs"]] <- block_specialized_verbs_fr(tokens, doc_ids, dict_lookup)
    df[["modals"]]            <- block_modals_fr(tokens, doc_ids, dict_lookup)
    df[["stranded_split"]]    <- block_stranded_split_fr(tokens, doc_ids)
    df[["split_coordination"]]<- block_split_coordination_fr(tokens, doc_ids, token_lookup, subject_heads, head_lookup, negation_part_lemmas)
    df[["negation"]]          <- block_negation_fr(tokens, doc_ids, neg_synthetic_terms, negation_part_lemmas, negation_adverbs)
  } else if (language == "es") {
    df[["passive_voice"]]     <- block_passive_voice_es(tokens, doc_ids, head_lookup, passive_rel_values)
    df[["clause_embedding"]]  <- block_clause_embedding_es(tokens, doc_ids, head_lookup)
    df[["participial_clauses"]]<- block_participial_clauses_es(tokens, doc_ids, head_lookup)
    df[["relative_clauses"]]  <- block_relatives_es(tokens, doc_ids, head_lookup)
    df[["contractions"]]      <- block_contractions_es(tokens, doc_ids)
    df[["adj_prep_adverbs"]]  <- block_adj_prep_adv_es(tokens, doc_ids, dict_lookup, word_lists_lookup, negation_adverbs)
    df[["specialized_verbs"]] <- block_specialized_verbs_es(tokens, doc_ids, dict_lookup)
    df[["modals"]]            <- block_modals_es(tokens, doc_ids, dict_lookup)
    df[["stranded_split"]]    <- block_stranded_split_es(tokens, doc_ids)
    df[["split_coordination"]]<- block_split_coordination_es(tokens, doc_ids, token_lookup, subject_heads, head_lookup, negation_part_lemmas)
    df[["negation"]]          <- block_negation_es(tokens, doc_ids, neg_synthetic_terms, negation_part_lemmas, negation_adverbs)
    nom_suf   <- get_word_list(word_lists_lookup, "nominalization_suffixes")
    nom_stop  <- get_word_list(word_lists_lookup, "nominalization_stoplist")
    ment_stop <- tryCatch(
      get_word_list(word_lists_lookup, "adverb_mente_stoplist"),
      warning = function(w) character(0)
    )
    df[["lexical_complexity"]] <- block_lexical_complexity_es(
      tokens = tokens, doc_ids = doc_ids,
      nominalization_suffixes = nom_suf,
      nominalization_stoplist = nom_stop,
      mente_stoplist          = ment_stop)
  }

  biber_tks <- biber_tks %>%
    quanteda::tokens_remove("\\d_", valuetype = "regex") %>%
    quanteda::tokens_remove("_punct_", valuetype = "fixed")

  biber_2 <- df %>% purrr::reduce(dplyr::full_join, by = "doc_id")

  biber_counts <- dplyr::full_join(biber_1, biber_2, by = "doc_id") %>%
    replace_nas()

  # Merge duplicate columns that appear from both dict and code paths
  combine_features <- c(
    "f_04_place_adverbials",
    "f_05_time_adverbials",
    "f_06_first_person_pronouns",
    "f_07_second_person_pronouns",
    "f_08_third_person_pronouns",
    "f_11_indefinite_pronoun",
    "f_51_demonstratives",
    "f_52_modal_possibility",
    "f_53_modal_necessity",
    "f_54_modal_predictive",
    "f_55_verb_public",
    "f_56_verb_private",
    "f_57_verb_suasive",
    "f_58_verb_seem"
  )
  for (feature in combine_features) {
    x_col <- paste0(feature, ".x")
    y_col <- paste0(feature, ".y")
    if (all(c(x_col, y_col) %in% colnames(biber_counts))) {
      x_vals <- dplyr::coalesce(biber_counts[[x_col]], 0L)
      y_vals <- dplyr::coalesce(biber_counts[[y_col]], 0L)
      pmax_features <- c("f_04_place_adverbials", "f_05_time_adverbials",
                         "f_06_first_person_pronouns", "f_07_second_person_pronouns",
                         "f_08_third_person_pronouns", "f_11_indefinite_pronoun",
                         "f_51_demonstratives")
      biber_counts[[feature]] <- if (feature %in% pmax_features) pmax(x_vals, y_vals) else x_vals + y_vals
      biber_counts <- dplyr::select(biber_counts, -dplyr::any_of(c(x_col, y_col)))
    }
  }

  if ("f_11_indefinite_pronoun" %in% colnames(biber_counts))
    biber_counts <- dplyr::rename(biber_counts, f_11_indefinite_pronouns = "f_11_indefinite_pronoun")

  # French disambiguation helpers (harmless no-ops when not present)
  if ("donc_as_discourse_particle" %in% colnames(biber_counts)) {
    if ("f_45_conjuncts" %in% colnames(biber_counts))
      biber_counts <- dplyr::mutate(biber_counts, f_45_conjuncts = pmax(.data$f_45_conjuncts - .data$donc_as_discourse_particle, 0))
    if ("f_50_discourse_particles" %in% colnames(biber_counts))
      biber_counts <- dplyr::mutate(biber_counts, f_50_discourse_particles = .data$f_50_discourse_particles + .data$donc_as_discourse_particle)
    biber_counts <- dplyr::select(biber_counts, -"donc_as_discourse_particle")
  }
  if ("ensuite_as_conjunct" %in% colnames(biber_counts)) {
    if ("f_05_time_adverbials" %in% colnames(biber_counts))
      biber_counts <- dplyr::mutate(biber_counts, f_05_time_adverbials = pmax(.data$f_05_time_adverbials - .data$ensuite_as_conjunct, 0))
    if ("f_45_conjuncts" %in% colnames(biber_counts))
      biber_counts <- dplyr::mutate(biber_counts, f_45_conjuncts = .data$f_45_conjuncts + .data$ensuite_as_conjunct)
    biber_counts <- dplyr::select(biber_counts, -"ensuite_as_conjunct")
  }
  if ("vraiment_as_amplifier" %in% colnames(biber_counts)) {
    if ("f_49_emphatics" %in% colnames(biber_counts))
      biber_counts <- dplyr::mutate(biber_counts, f_49_emphatics = pmax(.data$f_49_emphatics - .data$vraiment_as_amplifier, 0))
    if ("f_48_amplifiers" %in% colnames(biber_counts))
      biber_counts <- dplyr::mutate(biber_counts, f_48_amplifiers = .data$f_48_amplifiers + .data$vraiment_as_amplifier)
    biber_counts <- dplyr::select(biber_counts, -"vraiment_as_amplifier")
  }

  # Pronoun morphological supplement
  for (pf in c("f_06", "f_07", "f_08")) {
    supp_col <- paste0(pf, "_morph_supplement")
    main_col <- switch(pf,
      f_06 = "f_06_first_person_pronouns",
      f_07 = "f_07_second_person_pronouns",
      f_08 = "f_08_third_person_pronouns")
    if (supp_col %in% colnames(biber_counts) && main_col %in% colnames(biber_counts)) {
      biber_counts <- dplyr::mutate(biber_counts,
        !!main_col := pmax(.data[[main_col]], .data[[supp_col]], na.rm = TRUE))
    }
    if (supp_col %in% colnames(biber_counts))
      biber_counts <- dplyr::select(biber_counts, -dplyr::any_of(supp_col))
  }

  if (normalize) {
    tot_counts <- data.frame(tot_counts = quanteda::ntoken(biber_tks)) %>%
      tibble::rownames_to_column("doc_id") %>%
      dplyr::as_tibble()
    biber_counts <- dplyr::full_join(biber_counts, tot_counts, by = "doc_id")
    biber_counts <- normalize_counts(biber_counts)
  }

  if (measure != "none") {
    if (min(quanteda::ntoken(biber_tks)) < 200) {
      message("Setting type-to-token ratio to TTR")
      measure <- "TTR"
    }
    f_43_type_token <- quanteda.textstats::textstat_lexdiv(biber_tks, measure = measure) %>%
      dplyr::rename(doc_id = "document", f_43_type_token := !!measure)
    biber_counts <- dplyr::full_join(biber_counts, f_43_type_token, by = "doc_id")
  }

  f_44_mean_word_length <- tokens %>%
    dplyr::filter(stringr::str_detect(.data$token, "^[a-z]+$")) %>%
    dplyr::mutate(mean_word_length = stringr::str_length(.data$token)) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::summarise(f_44_mean_word_length = mean(.data$mean_word_length))

  biber_counts <- dplyr::full_join(biber_counts, f_44_mean_word_length, by = "doc_id") %>%
    replace_nas()

  biber_counts <- biber_counts %>%
    dplyr::select(order(colnames(biber_counts)))

  biber_counts[] <- lapply(biber_counts, as.vector)
  biber_counts
}
