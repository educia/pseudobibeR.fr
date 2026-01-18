#' Extract Biber features from a document parsed and annotated by spacyr or udpipe
#'
#' Takes data that has been part-of-speech tagged and dependency parsed and
#' extracts counts of features that have been used in Douglas Biber's research
#' since the late 1980s.
#'
#' Refer to `spacyr::spacy_parse()` or `udpipe::udpipe_annotate()` for details
#' on parsing texts. These must be configured to do part-of-speech and
#' dependency parsing. For `spacyr::spacy_parse()`, use the `dependency = TRUE`,
#' `tag = TRUE`, and `pos = TRUE` arguments; for `udpipe::udpipe_annotate()`,
#' set the `tagger` and `parser` arguments to `"default"`.
#'
#' Feature extraction relies on the `dict` and `word_lists` datasets to match specific features;
#' see their documentation
#' and values for details on the exact patterns and words matched by each. The
#' function identifies other features based on local cues, which are
#' approximations. Because they rely on probabilistic taggers provided by spaCy
#' or udpipe, the accuracy of the resulting counts are dependent on the accuracy
#' of those models. Thus, texts with irregular spellings, non-normative
#' punctuation, etc. will likely produce unreliable outputs, unless taggers are
#' tuned specifically for those purposes.
#'
#' The following feature families are detected. Each description highlights
#' typical French cues for the feature. The repository file
#' `biber_features_equivalents_french.csv` and the packaged dataset
#' `french_examples` list the exact examples that guide every heuristic.
#'
#' ## Tense and aspect markers
#'
#' \describe{
#' \item{f_01_past_tense}{Verbs in a past-tense form (e.g., \emph{il parlait}).}
#' \item{f_02_perfect_aspect}{
#'   Perfect aspect built with auxiliaries such as \emph{avoir} or \emph{etre}
#'   plus a past participle (e.g., \emph{j'ai fini}).
#' }
#' \item{f_03_present_tense}{Finite verbs in the present tense (e.g., \emph{nous parlons}).}
#' }
#'
#' ## Place and time adverbials
#'
#' \describe{
#' \item{f_04_place_adverbials}{Adverbs of place (e.g., \emph{dehors}, \emph{ailleurs}).}
#' \item{f_05_time_adverbials}{Adverbs of time (e.g., \emph{bientot}, \emph{hier}).}
#' }
#'
#' ## Pronouns and pro-verbs
#'
#' \describe{
#' \item{f_06_first_person_pronouns}{First-person pronouns (e.g., \emph{je}, \emph{nous}).}
#' \item{f_07_second_person_pronouns}{Second-person pronouns (e.g., \emph{tu}, \emph{vous}).}
#' \item{f_08_third_person_pronouns}{
#'   Third-person personal pronouns other than expletives
#'   (e.g., \emph{il}, \emph{elle}, \emph{ils}).
#' }
#' \item{f_09_pronoun_it}{Impersonal or expletive subjects (e.g., \emph{il} in \emph{il faut} or \emph{il pleut}).}
#' \item{f_10_demonstrative_pronoun}{Standalone demonstratives that replace a noun (e.g., \emph{celui-ci}).}
#' \item{f_11_indefinite_pronouns}{Indefinite pronouns (e.g., \emph{quelqu'un}, \emph{personne}).}
#' \item{f_12_proverb_do}{Uses of \emph{faire} as a pro-verb (e.g., \emph{je le fais}).}
#' }
#'
#' ## Questions
#'
#' \describe{
#' \item{f_13_wh_question}{Direct interrogatives beginning with \emph{qui}, \emph{quoi}, \emph{ou}, etc.}
#' }
#'
#' ## Nominal forms
#'
#' \describe{
#' \item{f_14_nominalizations}{Nominalisations such as words ending in \emph{-tion}, \emph{-ment}, \emph{-age}.}
#' \item{f_15_gerunds}{Gerunds or \emph{groupe infinitif} headed by \emph{en} (e.g., \emph{en travaillant}).}
#' \item{f_16_other_nouns}{All other noun tokens once nominalisations and gerunds are excluded.}
#' }
#'
#' ## Passives
#'
#' \describe{
#' \item{f_17_agentless_passives}{Passive clauses without an explicit agent (e.g., \emph{La decision a ete prise}).}
#' \item{f_18_by_passives}{
#'   Passive clauses with an expressed agent
#'   (e.g., \emph{La decision a ete prise par le directeur}).
#' }
#' }
#'
#' ## Stative forms
#'
#' \describe{
#' \item{f_19_be_main_verb}{\emph{etre} used as a lexical verb.}
#' \item{f_20_existential_there}{Existential constructions (e.g., \emph{il y a}).}
#' }
#'
#' ## Subordination features
#'
#' \describe{
#' \item{f_21_that_verb_comp}{Verb complements introduced by \emph{que} (e.g., \emph{je pense qu'il vient}).}
#' \item{f_22_that_adj_comp}{
#'   Adjectival complements introduced by \emph{que}
#'   (e.g., \emph{je suis heureux que tu sois la}).
#' }
#' \item{f_23_wh_clause}{\emph{wh}-clauses such as \emph{ce que}, \emph{ce qui}.}
#' \item{f_24_infinitives}{Infinitival clauses.}
#' \item{f_25_present_participle}{Adverbial clauses headed by a present participle (e.g., \emph{en chantant}).}
#' \item{f_26_past_participle}{Adverbial clauses headed by a past participle (e.g., \emph{construit en un an}).}
#' \item{f_27_past_participle_whiz}{
#'   Post-nominal reduced relatives with past participles
#'   (e.g., \emph{la maison construite en 2020}).
#' }
#' \item{f_28_present_participle_whiz}{
#'   Post-nominal reduced relatives with present participles
#'   (e.g., \emph{les enfants jouant dans le parc}).
#' }
#' \item{f_29_that_subj}{Subject relatives introduced by \emph{qui} following a noun.}
#' \item{f_30_that_obj}{Object relatives introduced by \emph{que}.}
#' \item{f_31_wh_subj}{Subject relatives with \emph{lequel/laquelle/...}.}
#' \item{f_32_wh_obj}{Object relatives with \emph{dont}, \emph{lequel/laquelle/...}.}
#' \item{f_33_pied_piping}{
#'   Pied-piping relatives including the preposition
#'   (e.g., \emph{la maniere dans laquelle il procede}).
#' }
#' \item{f_34_sentence_relatives}{
#'   Sentence relatives such as \emph{ce qui} following a clause
#'   (e.g., \emph{Il a reussi, ce qui est remarquable}).
#' }
#' \item{f_35_because}{Causal subordinates (e.g., \emph{parce que}, \emph{car}).}
#' \item{f_36_though}{Concessive subordinates (e.g., \emph{bien que}, \emph{meme si}).}
#' \item{f_37_if}{Conditional subordinates (e.g., \emph{si}, \emph{a moins que}).}
#' \item{f_38_other_adv_sub}{Other adverbial subordinators (e.g., \emph{tandis que}, \emph{lorsque}).}
#' }
#'
#' ## Prepositional phrases, adjectives, and adverbs
#'
#' \describe{
#' \item{f_39_prepositions}{Prepositions functioning as case markers.}
#' \item{f_40_adj_attr}{Attributive adjectives placed before or alongside a noun (e.g., \emph{une grande maison}).}
#' \item{f_41_adj_pred}{Predicative adjectives following a copula (e.g., \emph{la maison est grande}).}
#' \item{f_42_adverbs}{Adverbs not counted elsewhere (e.g., \emph{vraiment}).}
#' }
#'
#' ## Lexical specificity
#'
#' \describe{
#' \item{f_43_type_token}{Type-token ratio computed with the requested measure.}
#' \item{f_44_mean_word_length}{Average token length (letters only).}
#' }
#'
#' ## Lexical classes
#'
#' \describe{
#' \item{f_45_conjuncts}{Sentence connectors such as \emph{cependant}, \emph{toutefois}.}
#' \item{f_46_downtoners}{Downtoners (e.g., \emph{a peine}, \emph{presque}).}
#' \item{f_47_hedges}{Hedges or approximators (e.g., \emph{environ}, \emph{quelque peu}).}
#' \item{f_48_amplifiers}{Amplifiers (e.g., \emph{tres}, \emph{particulierement}).}
#' \item{f_49_emphatics}{Emphatic markers (e.g., \emph{vraiment}, \emph{bien}).}
#' \item{f_50_discourse_particles}{Discourse particles often sentence-initial (e.g., \emph{eh bien}, \emph{bon}).}
#' \item{f_51_demonstratives}{Demonstrative determiners (e.g., \emph{ce}, \emph{cet}, \emph{cette}, \emph{ces}).}
#' }
#'
#' ## Modals
#'
#' \describe{
#' \item{f_52_modal_possibility}{Verbs expressing possibility (e.g., \emph{pouvoir}, \emph{il se peut}).}
#' \item{f_53_modal_necessity}{Necessity modals (e.g., \emph{devoir}, \emph{falloir}).}
#' \item{f_54_modal_predictive}{Predictive markers (e.g., \emph{aller} + infinitive, \emph{devoir} au futur).}
#' }
#'
#' ## Specialized verb classes
#'
#' \describe{
#' \item{f_55_verb_public}{Public communication verbs (e.g., \emph{declarer}, \emph{annoncer}).}
#' \item{f_56_verb_private}{Private cognition verbs (e.g., \emph{penser}, \emph{croire}).}
#' \item{f_57_verb_suasive}{Suasive verbs (e.g., \emph{ordonner}, \emph{proposer}).}
#' \item{f_58_verb_seem}{Verbs of seeming (e.g., \emph{sembler}, \emph{paraitre}).}
#' }
#'
#' ## Reduced forms and dispreferred structures
#'
#' \describe{
#' \item{f_59_contractions}{Contractions (e.g., \emph{j'ai}, \emph{c'etait}).}
#' \item{f_60_that_deletion}{Subordinator \emph{que} omission in complement clauses.}
#' \item{f_61_stranded_preposition}{Stranded prepositions (e.g., \emph{la personne que je parlais avec}).}
#' \item{f_62_split_infinitive}{Split infinitives (e.g., \emph{de vraiment comprendre}).}
#' \item{f_63_split_auxiliary}{Auxiliary and participle separated by an adverb (e.g., \emph{a probablement ete vu}).}
#' }
#'
#' ## Co-ordination
#'
#' \describe{
#' \item{f_64_phrasal_coordination}{Coordination of like phrases (e.g., \emph{pommes et oranges}).}
#' \item{f_65_clausal_coordination}{Coordination of independent clauses (e.g., sentence-initial \emph{et}).}
#' }
#'
#' ## Negation
#'
#' \describe{
#' \item{f_66_neg_synthetic}{Synthetic negation with determiners or adjectives (e.g., \emph{aucun}, \emph{nul}).}
#' \item{f_67_neg_analytic}{
#'   Analytic negation with \emph{ne} ... adverb
#'   (e.g., \emph{ne ... pas}, \emph{ne ... jamais}).
#' }
#' }
#'
#' @param tokens A dataset of tokens created by `spacyr::spacy_parse()` or
#'   `udpipe::udpipe_annotate()`
#' @param measure Measure to use for type-token ratio. Passed to
#'   `quanteda.textstats::textstat_lexdiv()` to calculate the statistic. Can be
#'   the Moving Average Type-Token Ratio (MATTR), ordinary Type-Token Ratio
#'   (TTR), corrected TTR (CTTR), Mean Segmental Type-Token Ratio (MSTTR), or
#'   `"none"` to skip calculating a type-token ratio. If a statistic is chosen
#'   but there are fewer than 200 token in the smallest document, the TTR is
#'   used instead.
#' @param normalize If `TRUE`, count features are normalized to the rate per
#'   1,000 tokens.
#' @return A `data.frame` of features containing one row per document and one
#'   column per feature. If `normalize` is `TRUE`, count features are normalized
#'   to the rate per 1,000 tokens.
#' @references Biber, Douglas (1985). "Investigating macroscopic textual
#' variation through multifeature/multidimensional analyses." *Linguistics*
#' 23(2), 337-360. \doi{10.1515/ling.1985.23.2.337}
#'
#' Biber, Douglas (1988). *Variation across Speech and Writing*.
#'   Cambridge University Press.
#'
#' Biber, Douglas (1995). *Dimensions of Register Variation: A Cross-Linguistic
#' Comparison.* Cambridge University Press.
#'
#' Covington, M. A., & McFall, J. D. (2010). Cutting the Gordian Knot: The
#' Moving-Average Type-Token Ratio (MATTR). *Journal of Quantitative
#' Linguistics*, 17(2), 94-100. \doi{10.1080/09296171003643098}
#' @examples
#' # Basic usage with pre-parsed samples
#' biber(udpipe_samples)
#' biber(spacy_samples)
#'
#' # Get raw counts instead of normalized rates
#' raw_features <- biber(udpipe_samples, normalize = FALSE)
#' head(raw_features[, 1:5])
#'
#' # Use different type-token ratio measures
#' features_ttr <- biber(udpipe_samples, measure = "TTR")
#' features_mattr <- biber(udpipe_samples, measure = "MATTR")
#'
#' # Skip type-token ratio calculation
#' features_no_ttr <- biber(udpipe_samples, measure = "none")
#'
#' \dontrun{
#' # Parse your own French text with udpipe
#' library(udpipe)
#' udmodel <- udpipe_load_model("french-gsd-ud-2.5-191206.udpipe")
#' my_text <- "Voici un exemple de texte en français."
#' parsed <- udpipe_annotate(udmodel, my_text, parser = "default", tagger = "default")
#' features <- biber(parsed)
#'
#' # Or with spacyr (requires Python and spaCy installation)
#' library(spacyr)
#' spacy_initialize(model = "fr_core_news_sm")
#' parsed_sp <- spacy_parse("Voici un exemple de texte.", dependency = TRUE, tag = TRUE, pos = TRUE)
#' features_sp <- biber(parsed_sp)
#' spacy_finalize()
#' }
#' @importFrom magrittr %>%
#' @seealso [dict], [word_lists]
#' @export
biber <- function(tokens, measure = c("MATTR", "TTR", "CTTR", "MSTTR", "none"),
                  normalize = TRUE) {
  UseMethod("biber")
}

#' @rdname biber
#' @export
biber.spacyr_parsed <- function(tokens, measure = c("MATTR", "TTR", "CTTR", "MSTTR", "none"),
                                normalize = TRUE) {
  # Input validation
  if (is.null(tokens)) {
    stop("'tokens' cannot be NULL. Please provide a valid spacyr_parsed object.", call. = FALSE)
  }
  
  if (!is.data.frame(tokens)) {
    stop("'tokens' must be a data frame. Got object of class: ", paste(class(tokens), collapse = ", "), call. = FALSE)
  }
  
  if (nrow(tokens) == 0) {
    stop("'tokens' is empty (0 rows). Please provide a data frame with parsed tokens.", call. = FALSE)
  }
  
  required_cols <- c("doc_id", "token", "lemma", "pos", "tag", "dep_rel")
  missing_cols <- setdiff(required_cols, colnames(tokens))
  
  if (length(missing_cols) > 0) {
    if ("dep_rel" %in% missing_cols) {
      stop("Column 'dep_rel' not found. Be sure to set 'dependency = TRUE' when using spacy_parse().", call. = FALSE)
    }
    if ("tag" %in% missing_cols) {
      stop("Column 'tag' not found. Be sure to set 'tag = TRUE' when using spacy_parse().", call. = FALSE)
    }
    if ("pos" %in% missing_cols) {
      stop("Column 'pos' not found. Be sure to set 'pos = TRUE' when using spacy_parse().", call. = FALSE)
    }
    stop("Missing required column(s): ", paste(missing_cols, collapse = ", "), 
         ". Ensure 'tokens' is from spacyr::spacy_parse() with dependency=TRUE, tag=TRUE, pos=TRUE.", call. = FALSE)
  }

  measure <- match.arg(measure)

  parse_biber_features(tokens, measure, normalize, "spacy")
}

#' @rdname biber
#' @export
biber.udpipe_connlu <- function(tokens, measure = c("MATTR", "TTR", "CTTR", "MSTTR", "none"),
                                normalize = TRUE) {
  # Check for udpipe package
  if (!requireNamespace("udpipe", quietly = TRUE)) {
    stop("Package 'udpipe' must be installed to extract features from udpipe-tagged text.\n",
         "Install it with: install.packages('udpipe')", call. = FALSE)
  }
  
  # Input validation
  if (is.null(tokens)) {
    stop("'tokens' cannot be NULL. Please provide a valid udpipe_connlu object.", call. = FALSE)
  }

  udpipe_tks <- as.data.frame(tokens, stringsAsFactors = FALSE)
  
  if (nrow(udpipe_tks) == 0) {
    stop("'tokens' is empty (0 rows). Please provide parsed tokens from udpipe_annotate().", call. = FALSE)
  }
  
  required_cols <- c("doc_id", "token", "lemma", "upos", "xpos", "dep_rel")
  missing_cols <- setdiff(required_cols, colnames(udpipe_tks))
  
  if (length(missing_cols) > 0) {
    if ("dep_rel" %in% missing_cols) {
      stop("Column 'dep_rel' not found. Be sure to set parser = 'default' in udpipe_annotate().", call. = FALSE)
    }
    if ("xpos" %in% missing_cols || "upos" %in% missing_cols) {
      stop("Column 'upos' or 'xpos' not found. Be sure to set tagger = 'default' in udpipe_annotate().", call. = FALSE)
    }
    stop("Missing required column(s): ", paste(missing_cols, collapse = ", "),
         ". Ensure 'tokens' is from udpipe_annotate() with tagger='default' and parser='default'.", call. = FALSE)
  }

  measure <- match.arg(measure)

  udpipe_tks <- udpipe_tks %>%
    dplyr::select("doc_id", "sentence_id", "token_id", "token", "lemma", "upos",
                  "xpos", "feats", "head_token_id", "dep_rel") %>%
    dplyr::rename(pos = "upos", tag = "xpos") %>%
    dplyr::mutate(tag = dplyr::if_else(is.na(.data$tag) | .data$tag == "", .data$pos, .data$tag))

  udpipe_tks <- structure(udpipe_tks, class = c("spacyr_parsed", "data.frame"))

  parse_biber_features(udpipe_tks, measure, normalize, "udpipe")
}

#' Main feature extraction orchestrator
#' 
#' This function coordinates all feature extraction by calling the modular
#' block functions and assembling the results.
#' 
#' @param tokens Annotated token data
#' @param measure Type-token ratio measure
#' @param normalize Whether to normalize counts
#' @param engine Parser engine ("spacy" or "udpipe")
#' @return Data frame of extracted features
#' 
#' @importFrom rlang .data :=
#' @importFrom utils tail
#' @keywords internal
parse_biber_features <- function(tokens, measure, normalize, engine = c("spacy", "udpipe")) {
  engine <- match.arg(engine)
  
  # Validate tokens data frame has required structure
  if (!is.data.frame(tokens)) {
    stop("'tokens' must be a data frame after processing.", call. = FALSE)
  }
  
  if (nrow(tokens) == 0) {
    stop("'tokens' is empty after processing. Cannot extract features from 0 tokens.", call. = FALSE)
  }
  
  if (!"doc_id" %in% colnames(tokens)) {
    stop("'tokens' must have a 'doc_id' column to identify documents.", call. = FALSE)
  }

  dict_lookup <- dict
  word_lists_lookup <- word_lists

  df <- list()

  tokens <- tokens %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(token = stringr::str_to_lower(.data$token)) %>%
    dplyr::mutate(pos = dplyr::if_else(.data$token == "\n", "PUNCT", .data$pos)) %>%
    dplyr::filter(.data$pos != "SPACE")
  
  # Check after filtering SPACE tokens
  if (nrow(tokens) == 0) {
    stop("No valid tokens found after filtering. Document may contain only whitespace.", call. = FALSE)
  }

  if ("morph" %in% colnames(tokens)) {
    tokens <- tokens %>%
      dplyr::mutate(morph = purrr::map_chr(.data$morph, function(x) {
        if (inherits(x, "python.builtin.object")) {
          if (requireNamespace("reticulate", quietly = TRUE)) {
            value <- reticulate::py_to_r(x)
            if (is.null(value) || length(value) == 0) {
              return("")
            }
            value <- as.character(value)
            if (length(value) == 1) {
              return(value)
            }
            return(paste(value, collapse = "|"))
          }
          return(as.character(x))
        }
        if (is.null(x) || length(x) == 0) {
          return("")
        }
        if (length(x) == 1) {
          return(as.character(x))
        }
        paste(as.character(x), collapse = "|")
      })) %>%
      dplyr::mutate(morph = dplyr::na_if(.data$morph, ""))

    if ("feats" %in% colnames(tokens)) {
      tokens <- tokens %>%
        dplyr::mutate(feats = dplyr::coalesce(.data$feats, .data$morph))
    } else {
      tokens <- tokens %>%
        dplyr::mutate(feats = .data$morph)
    }
  }

  if (!"feats" %in% colnames(tokens)) {
    tokens <- dplyr::mutate(tokens, feats = NA_character_)
  }

  tokens <- tokens %>%
    dplyr::mutate(
      token_id_int = suppressWarnings(as.integer(.data$token_id)),
      head_token_id_int = suppressWarnings(as.integer(.data$head_token_id)),
      morph_tense = extract_morph_value(.data$feats, "Tense"),
      morph_verbform = extract_morph_value(.data$feats, "VerbForm"),
      morph_mood = extract_morph_value(.data$feats, "Mood"),
      morph_prontype = extract_morph_value(.data$feats, "PronType"),
      morph_voice = extract_morph_value(.data$feats, "Voice"),
      morph_number = extract_morph_value(.data$feats, "Number"),
      morph_person = extract_morph_value(.data$feats, "Person")
    )

  tokens <- tokens %>%
    dplyr::arrange(.data$doc_id, .data$sentence_id, .data$token_id_int)

  doc_ids <- tokens %>% dplyr::distinct(.data$doc_id)

  proverb_pronouns <- normalize_terms(
    get_word_list(word_lists_lookup, "proverb_object_pronouns")
  )

  neg_synthetic_terms <- normalize_terms(
    get_word_list(word_lists_lookup, "neg_synthetic_determiners")
  )

  negation_particle_terms <- normalize_terms(get_word_list(word_lists_lookup, "negation_particles"))
  negation_part_lemmas <- unique(c(negation_particle_terms, "n'", "n\u2019"))

  negation_adverbs <- normalize_terms(
    get_word_list(
      word_lists_lookup,
      "neg_analytic_adverbs"
    )
  )

  impersonal_verbs <- normalize_terms(
    get_word_list(
      word_lists_lookup,
      "impersonal_verbs"
    )
  )

  weather_lemmas <- unique(c(impersonal_verbs, "bruiner", "tonner"))
  raising_verbs <- c("sembler", "para\u00eetre", "demeurer", "rester", "suffire", "convenir")
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

  de_markers <- tokens %>%
    dplyr::filter(
      .data$lemma == "de",
      .data$dep_rel %in% c("mark", "advmod", "case"),
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::transmute(
      .data$doc_id,
      .data$sentence_id,
      head_token_id_int = .data$head_token_id_int,
      has_de_marker = TRUE
    ) %>%
    dplyr::distinct()

  que_markers <- tokens %>%
    dplyr::filter(
      .data$lemma %in% c("que", "qu'", "qu\u2019"),
      .data$dep_rel %in% c("mark", "expl", "obj"),
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::transmute(
      .data$doc_id,
      .data$sentence_id,
      head_token_id_int = .data$head_token_id_int,
      has_que_marker = TRUE
    ) %>%
    dplyr::distinct()

  clause_complements <- tokens %>%
    dplyr::filter(
      .data$dep_rel %in% c("ccomp", "xcomp", "csubj", "advcl"),
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::transmute(
      .data$doc_id,
      .data$sentence_id,
      head_token_id_int = .data$head_token_id_int,
      has_clause_comp = TRUE
    ) %>%
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
      biber_tks <- quanteda::tokens_compound(biber_tks, pattern = multi_phrases)
    }

    biber_tks <- quanteda::tokens_tolower(biber_tks)

    biber_1 <- quanteda::tokens_lookup(biber_tks, dictionary = dict_lookup, nomatch = NULL) %>%
      quanteda::dfm() %>%
      quanteda::convert(to = "data.frame") %>%
      dplyr::as_tibble()

    if ("document" %in% colnames(biber_1)) {
      biber_1 <- biber_1 %>% dplyr::rename(doc_id = "document")
    }
  } else {
    biber_1 <- tibble::tibble(doc_id = character())
  }

  df[["f_01_past_tense"]] <- tokens %>%
    dplyr::filter(
      .data$pos == "VERB",
      .data$morph_tense %in% c("Past", "Imp", "Pqp"),
      is.na(.data$morph_verbform) | .data$morph_verbform %in% c("Fin")
    ) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_01_past_tense = "n")

  df[["f_03_present_tense"]] <- tokens %>%
    dplyr::filter(
      .data$pos == "VERB",
      .data$morph_tense %in% c("Pres"),
      is.na(.data$morph_verbform) | .data$morph_verbform %in% c("Fin")
    ) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_03_present_tense = "n")

  head_lookup <- tokens %>%
    dplyr::select(
      "doc_id", "sentence_id", "token_id_int",
      head_pos = "pos",
      head_lemma = "lemma",
      head_token = "token",
      head_feats = "feats",
      head_morph_verbform = "morph_verbform",
      head_morph_voice = "morph_voice",
      head_morph_tense = "morph_tense"
    )

  df[["auxiliary_tense"]] <- block_aux_tense_fr(
    tokens = tokens,
    doc_ids = doc_ids,
    head_lookup = head_lookup,
    proverb_pronouns = proverb_pronouns
  )

  df[["personal_pronouns"]] <- block_personal_pronouns_fr(
    tokens = tokens,
    doc_ids = doc_ids,
    head_lookup = head_lookup,
    de_markers = de_markers,
    que_markers = que_markers,
    clause_complements = clause_complements,
    weather_lemmas = weather_lemmas,
    raising_verbs = raising_verbs,
    wh_question_lemmas = wh_question_lemmas
  )

  df[["lexical_membership"]] <- block_lexical_membership_fr(
    tokens = tokens,
    doc_ids = doc_ids,
    word_lists_lookup = word_lists_lookup
  )

  df[["donc_disambiguation"]] <- count_donc_discourse_fr(
    tokens = tokens,
    doc_ids = doc_ids
  )

  df[["ensuite_disambiguation"]] <- count_ensuite_conjunct_fr(
    tokens = tokens,
    doc_ids = doc_ids
  )

  df[["vraiment_disambiguation"]] <- count_vraiment_amplifier_fr(
    tokens = tokens,
    doc_ids = doc_ids
  )

  df[["pronoun_morph_supplement"]] <- supplement_pronouns_morphological_fr(
    tokens = tokens,
    doc_ids = doc_ids
  )

  passive_rel_values <- if (engine == "spacy") c("auxpass", "aux:pass") else "aux:pass"
  passive_agents <- c("par")

  tokens <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::arrange(.data$sentence_id, .data$token_id_int, .by_group = TRUE) %>%
    dplyr::mutate(
      passive_agent_next2 = dplyr::lead(.data$token %in% passive_agents, 2, default = FALSE),
      passive_agent_next3 = dplyr::lead(.data$token %in% passive_agents, 3, default = FALSE),
      is_infinitive = dplyr::if_else(
        .data$pos %in% c("VERB", "AUX") & !is.na(.data$morph_verbform),
        .data$morph_verbform == "Inf",
        FALSE
      ),
      is_present_participle = dplyr::case_when(
        .data$tag == "VBG" ~ TRUE,
        .data$pos %in% c("VERB", "AUX") & !is.na(.data$morph_verbform) & .data$morph_verbform == "Ger" ~ TRUE,
        .data$pos %in% c("VERB", "AUX") & !is.na(.data$morph_verbform) & .data$morph_verbform == "Part" &
          .data$morph_tense %in% c("Pres", "Imp") ~ TRUE,
        TRUE ~ FALSE
      ),
      is_past_participle = dplyr::case_when(
        .data$tag == "VBN" ~ TRUE,
        .data$pos %in% c("VERB", "AUX") & !is.na(.data$morph_verbform) &
          .data$morph_verbform == "Part" &
          .data$morph_tense %in% c("Past", "Pqp") ~ TRUE,
        .data$pos %in% c("VERB", "ADJ") &
          stringr::str_detect(
            stringr::str_to_lower(.data$token),
            "(\u00e9|\u00e9e|\u00e9s|\u00e9es|i|ie|is|ies|u|ue|us|ues|it|ite|its|ites)$"
          ) &
          stringr::str_detect(
            dplyr::coalesce(.data$dep_rel, ""),
            "^(acl|advcl|xcomp|ccomp|root)"
          ) ~ TRUE,
        TRUE ~ FALSE
      ),
      is_relative_pronoun = dplyr::if_else(
        (!is.na(.data$morph_prontype) & stringr::str_detect(.data$morph_prontype, "Rel")) |
          (
            .data$lemma %in% relative_pronoun_candidates &
              stringr::str_detect(
                dplyr::coalesce(.data$dep_rel, ""),
                "^(nsubj|obj|obl|iobj|expl|mark|acl)"
              )
          ),
        TRUE,
        FALSE
      ),
      is_relative_subject = .data$is_relative_pronoun &
        stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "nsubj"),
      is_relative_object = .data$is_relative_pronoun &
        (
          stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "obj") |
            (
              stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^obl") &
                .data$lemma %in% c(
                  "lequel", "laquelle", "lesquel", "lesquelle", "lesquels", "lesquelles",
                  "auquel", "auxquels", "auxquelles",
                  "duquel", "desquels", "desquelles",
                  "dont"
                ) &
                !dplyr::lag(.data$pos == "ADP", default = FALSE)
            ) |
            (
              .data$lemma %in% c("que") &
                stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^mark")
            )
        ),
      prev_token = dplyr::lag(.data$token),
      prev_lemma = dplyr::lag(.data$lemma),
      prev_pos = dplyr::lag(.data$pos),
      prev2_token = dplyr::lag(.data$token, 2),
      prev2_lemma = dplyr::lag(.data$lemma, 2),
      prev3_token = dplyr::lag(.data$token, 3),
      prev3_lemma = dplyr::lag(.data$lemma, 3),
      prev3_pos = dplyr::lag(.data$pos, 3),
      next_token = dplyr::lead(.data$token),
      next_lemma = dplyr::lead(.data$lemma),
      next_pos = dplyr::lead(.data$pos),
      next_dep_rel = dplyr::lead(.data$dep_rel),
      next2_token = dplyr::lead(.data$token, 2),
      next2_lemma = dplyr::lead(.data$lemma, 2),
      next2_pos = dplyr::lead(.data$pos, 2),
      next2_morph_verbform = dplyr::lead(.data$morph_verbform, 2),
      prev_morph_verbform = dplyr::lag(.data$morph_verbform),
      next_morph_verbform = dplyr::lead(.data$morph_verbform)
    ) %>%
    dplyr::ungroup()

  token_lookup <- tokens %>%
    dplyr::select(
      "doc_id", "sentence_id", "token_id_int",
      token_pos = "pos",
      token_dep_rel = "dep_rel",
      token_head_token_id_int = "head_token_id_int",
      token_morph_verbform = "morph_verbform"
    )

  subject_pron_lemmas <- c("je", "tu", "il", "elle", "on", "nous", "vous", "ils", "elles", "lui", "leur")

  subject_heads <- tokens %>%
    dplyr::filter(
      stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^nsubj") |
        (
          stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^(obj|iobj)") &
            .data$pos == "PRON" &
            .data$lemma %in% subject_pron_lemmas
        )
    ) %>%
    dplyr::distinct(
      .data$doc_id,
      .data$sentence_id,
      clause_head_token_id_int = .data$head_token_id_int
    ) %>%
    dplyr::mutate(has_subject = TRUE)

  df[["passive_voice"]] <- block_passive_voice_fr(
    tokens = tokens,
    doc_ids = doc_ids,
    head_lookup = head_lookup,
    passive_rel_values = passive_rel_values
  )

  df[["clause_embedding"]] <- block_clause_embedding_fr(
    tokens = tokens,
    doc_ids = doc_ids,
    head_lookup = head_lookup
  )

  df[["participial_clauses"]] <- block_participial_clauses_fr(
    tokens = tokens,
    doc_ids = doc_ids
  )

  df[["relative_clauses"]] <- block_relatives_fr(
    tokens = tokens,
    doc_ids = doc_ids
  )
  
  df[["contractions"]] <- block_contractions_fr(
    tokens = tokens,
    doc_ids = doc_ids
  )

  df[["adj_prep_adverbs"]] <- block_adj_prep_adv_fr(
    tokens = tokens,
    doc_ids = doc_ids,
    dict_lookup = dict_lookup,
    word_lists_lookup = word_lists_lookup,
    negation_adverbs = negation_adverbs
  )

  df[["specialized_verbs"]] <- block_specialized_verbs_fr(
    tokens = tokens,
    doc_ids = doc_ids,
    dict_lookup = dict_lookup
  )

  df[["modals"]] <- block_modals_fr(
    tokens = tokens,
    doc_ids = doc_ids,
    dict_lookup = dict_lookup
  )
  
  df[["stranded_split"]] <- block_stranded_split_fr(
    tokens = tokens,
    doc_ids = doc_ids
  )

  df[["split_coordination"]] <- block_split_coordination_fr(
    tokens = tokens,
    doc_ids = doc_ids,
    token_lookup = token_lookup,
    subject_heads = subject_heads,
    head_lookup = head_lookup,
    negation_part_lemmas = negation_part_lemmas
  )

  df[["negation"]] <- block_negation_fr(
    tokens = tokens,
    doc_ids = doc_ids,
    neg_synthetic_terms = neg_synthetic_terms,
    negation_part_lemmas = negation_part_lemmas,
    negation_adverbs = negation_adverbs
  )

  biber_tks <- biber_tks %>%
    quanteda::tokens_remove("\\d_", valuetype = "regex") %>%
    quanteda::tokens_remove("_punct_", valuetype = "fixed")

  biber_2 <- df %>% purrr::reduce(dplyr::full_join, by = "doc_id")

  biber_counts <- dplyr::full_join(biber_1, biber_2, by = "doc_id") %>%
    replace_nas()

  combine_features <- c(
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
      if (feature == "f_51_demonstratives") {
        biber_counts[[feature]] <- pmax(x_vals, y_vals)
      } else {
        biber_counts[[feature]] <- x_vals + y_vals
      }
      biber_counts <- biber_counts %>%
        dplyr::select(-dplyr::any_of(c(x_col, y_col)))
    }
  }

  if ("f_11_indefinite_pronoun" %in% colnames(biber_counts)) {
    biber_counts <- biber_counts %>%
      dplyr::rename(f_11_indefinite_pronouns = "f_11_indefinite_pronoun")
  }

  # Disambiguate 'donc' between conjuncts and discourse particles
  if ("donc_as_discourse_particle" %in% colnames(biber_counts)) {
    if ("f_45_conjuncts" %in% colnames(biber_counts)) {
      # Subtract discourse uses from conjunct count
      biber_counts <- biber_counts %>%
        dplyr::mutate(
          f_45_conjuncts = pmax(.data$f_45_conjuncts - .data$donc_as_discourse_particle, 0)
        )
    }
    if ("f_50_discourse_particles" %in% colnames(biber_counts)) {
      # Add discourse uses to discourse particle count
  # Disambiguate 'ensuite' between time adverbials and conjuncts
  if ("ensuite_as_conjunct" %in% colnames(biber_counts)) {
    if ("f_05_time_adverbials" %in% colnames(biber_counts)) {
      # Subtract conjunct uses from time adverbial count
      biber_counts <- biber_counts %>%
        dplyr::mutate(
          f_05_time_adverbials = pmax(.data$f_05_time_adverbials - .data$ensuite_as_conjunct, 0)
        )
    }
    if ("f_45_conjuncts" %in% colnames(biber_counts)) {
      # Add conjunct uses to conjunct count
      biber_counts <- biber_counts %>%
        dplyr::mutate(
          f_45_conjuncts = .data$f_45_conjuncts + .data$ensuite_as_conjunct
        )
    }
    # Remove the temporary disambiguation column
    biber_counts <- biber_counts %>%
      dplyr::select(-"ensuite_as_conjunct")
  }

  # Disambiguate 'vraiment' between emphatics and amplifiers
  if ("vraiment_as_amplifier" %in% colnames(biber_counts)) {
    if ("f_49_emphatics" %in% colnames(biber_counts)) {
      # Subtract amplifier uses from emphatic count
      biber_counts <- biber_counts %>%
        dplyr::mutate(
          f_49_emphatics = pmax(.data$f_49_emphatics - .data$vraiment_as_amplifier, 0)
        )
    }
    if ("f_48_amplifiers" %in% colnames(biber_counts)) {
      # Add amplifier uses to amplifier count
      biber_counts <- biber_counts %>%
        dplyr::mutate(
          f_48_amplifiers = .data$f_48_amplifiers + .data$vraiment_as_amplifier
        )
    }
    # Remove the temporary disambiguation column
    biber_counts <- biber_counts %>%
      dplyr::select(-"vraiment_as_amplifier")
  }

  # Supplement pronoun counts with morphological features
  # Use whichever is higher: dictionary count or morphological count
  # This catches pronouns that udpipe tagged correctly but aren't in our dictionary
  if ("f_06_morph_supplement" %in% colnames(biber_counts)) {
    if ("f_06_first_person_pronouns" %in% colnames(biber_counts)) {
      biber_counts <- biber_counts %>%
        dplyr::mutate(
          f_06_first_person_pronouns = pmax(
            .data$f_06_first_person_pronouns,
            .data$f_06_morph_supplement,
            na.rm = TRUE
          )
        )
    }
    biber_counts <- biber_counts %>%
      dplyr::select(-"f_06_morph_supplement")
  }

  if ("f_07_morph_supplement" %in% colnames(biber_counts)) {
    if ("f_07_second_person_pronouns" %in% colnames(biber_counts)) {
      biber_counts <- biber_counts %>%
        dplyr::mutate(
          f_07_second_person_pronouns = pmax(
            .data$f_07_second_person_pronouns,
            .data$f_07_morph_supplement,
            na.rm = TRUE
          )
        )
    }
    biber_counts <- biber_counts %>%
      dplyr::select(-"f_07_morph_supplement")
  }

  if ("f_08_morph_supplement" %in% colnames(biber_counts)) {
    if ("f_08_third_person_pronouns" %in% colnames(biber_counts)) {
      biber_counts <- biber_counts %>%
        dplyr::mutate(
          f_08_third_person_pronouns = pmax(
            .data$f_08_third_person_pronouns,
            .data$f_08_morph_supplement,
            na.rm = TRUE
          )
        )
    }
    biber_counts <- biber_counts %>%
      dplyr::select(-"f_08_morph_supplement")
  }

      biber_counts <- biber_counts %>%
        dplyr::mutate(
          f_50_discourse_particles = .data$f_50_discourse_particles + .data$donc_as_discourse_particle
        )
    }
    # Remove the temporary disambiguation column
    biber_counts <- biber_counts %>%
      dplyr::select(-"donc_as_discourse_particle")
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
    dplyr::filter(
      stringr::str_detect(.data$token, "^[a-z]+$")
    ) %>%
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
