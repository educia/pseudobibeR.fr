# pseudobibeR.fr

[![R-CMD-check](https://github.com/browndw/pseudobibeR.fr/workflows/R-CMD-check/badge.svg)](https://github.com/browndw/pseudobibeR.fr/actions)
[![Tests](https://github.com/browndw/pseudobibeR.fr/workflows/Tests/badge.svg)](https://github.com/browndw/pseudobibeR.fr/actions)
[![CRAN status](https://www.r-pkg.org/badges/version/pseudobibeR.fr)](https://CRAN.R-project.org/package=pseudobibeR.fr)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/pseudobibeR.fr)](https://cran.r-project.org/package=pseudobibeR.fr)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Companion package to `pseudobibeR` that targets French-language parsing
resources and feature extraction. The code base is currently under active
development; see `docs/french_feature_mapping.md` in the repository root for
an overview of planned feature coverage.

The **pseudobibeR.fr** package calculates the lexicogrammatical and functional
features described by Biber (1985) and widely used for text-type, register, and
genre classification tasks. While it shares the same feature catalog as the
English-focused `pseudobibeR`, this variant relies on French-specific
morphology provided by spaCy or udpipe. The package extracts 67 different
linguistic features from pre-parsed text data, enabling researchers to analyze
linguistic variation across French text types and registers.

## Overview

This package doesn't perform part-of-speech tagging itself. Instead, it
leverages existing high-quality taggers from [udpipe](https://bnosac.github.io/udpipe/en/)
or [spaCy](https://spacy.io/) (via [spacyr](https://spacyr.quanteda.io/index.html))
to extract and aggregate linguistic patterns. For French we depend heavily on
the UD morphological features emitted by those taggers, so be sure to request
them during parsing. The accuracy of feature extraction depends on the quality
of the underlying part-of-speech tagging and dependency parsing.

**Note:** Texts with irregular spellings, non-normative punctuation, or domain-specific language may produce less reliable outputs unless the taggers are specifically tuned for those purposes.

## Installation

Install the development version from GitHub:

```r
# install.packages("devtools")
devtools::install_github("browndw/pseudobibeR.fr")
```

### Install from a release tarball

1. Download the latest `pseudobibeR.fr_<version>.tar.gz` file from the
  [GitHub Releases page](https://github.com/browndw/pseudobibeR.fr/releases).
2. In R, install the downloaded archive with:

  ```r
  install.packages("/path/to/pseudobibeR.fr_<version>.tar.gz", repos = NULL, type = "source")
  ```

  Replace `/path/to/` with the location where you saved the tarball; on macOS
  and Linux you can drag the file into the R console to fill in the path.
3. Restart your R session so the newly installed package is picked up.

## Quick Start

The package provides the main function `biber()`, which takes either udpipe- or spacyr-tagged text and produces a data frame of linguistic features for each document.

### Using spaCy (via spacyr)

```r
library(spacyr)
library(pseudobibeR.fr)

# Initialize spaCy with the small French model
spacy_initialize(model = "fr_core_news_sm")

parsed <- spacy_parse(
  c(
    "doc_1" = "Nous avons terminé l'étude hier soir.",
    "doc_2" = "Je pense qu'il faudrait revoir cette hypothèse."
  ),
  dependency = TRUE,
  tag = TRUE,
  pos = TRUE,
  morph = TRUE,
  additional_attributes = "morph"
)

features <- biber(parsed, measure = "none", normalize = FALSE)
print(features)

# Shut spaCy down when finished
spacy_finalize()
```

### Using udpipe

```r
library(udpipe)
library(pseudobibeR.fr)

model <- udpipe_download_model(language = "french-gsd")
ud_model <- udpipe_load_model(model$file_model)

text_data <- data.frame(
  doc_id = c("doc_1", "doc_2"),
  text = c(
    "Le rapport a été rédigé par l'équipe de recherche.",
    "Nous aimerions clarifier ce que vous avez proposé."
  )
)

parsed_data <- udpipe_annotate(
  ud_model,
  x = text_data$text,
  doc_id = text_data$doc_id,
  tagger = "default",
  parser = "default"
)

features <- biber(parsed_data, measure = "none", normalize = FALSE)
print(features)
```

### Using sample data

The package includes sample data to demonstrate functionality:

```r
library(pseudobibeR.fr)

# Extract features from sample spaCy-parsed data
spacy_features <- biber(spacy_samples)
head(spacy_features)

# Extract features from sample udpipe-parsed data
udpipe_features <- biber(udpipe_samples)
head(udpipe_features)
```

## Language-specific parsing notes

- **spaCy pipelines** must request morphological features: call
  `spacy_parse(..., morph = TRUE, additional_attributes = "morph")` or the
  feature extractor will miss tense and pronoun cues. The helper automatically
  collapses spaCy's Python morph objects to strings, so no extra processing is
  required after parsing.
- **spaCy models**: `fr_core_news_sm` works for quick starts, but richer models
  such as `fr_core_news_md` improve dependency accuracy and downstream feature
  counts.
- **udpipe models**: the bundled French UD model (`french-gsd-ud-2.5-191206`) is
  used in the package data. French UD corpora rarely populate `xpos`, so the
  feature code falls back to `upos` and morphological features exposed in the
  `feats` column.
- **Mixed-language corpora**: if your texts contain large amounts of English,
  consider running the original `pseudobibeR` side-by-side; the French heuristics
  prioritize finite verb morphology and may under-count English forms.

## Key Features

- **67 linguistic features** organized into 16 categories
- **Automatic feature normalization** to counts per 1,000 tokens (optional)
- **Multiple type-token ratio measures** (MATTR, TTR, CTTR, MSTTR)
- **Support for both spaCy and udpipe** parsing pipelines
- **Built-in dictionaries and word lists** for feature detection
- **Comprehensive test suite** with saved parsing examples

## Dependencies and Requirements

### Core Dependencies

- R (>= 3.5.0)
- dplyr, purrr, quanteda, quanteda.textstats, rlang, stringr, tibble, magrittr

### For Text Parsing

Choose one of the following parsing pipelines:

#### Option 1: spaCy + spacyr

- Python with spaCy installed
- spacyr R package
- A spaCy French language model (e.g., `fr_core_news_sm`)
- Use `spacy_parse(..., morph = TRUE, additional_attributes = "morph")` so that
  tense and agreement features needed by the French extractor are available.

#### Option 2: udpipe  

- udpipe R package
- A udpipe French language model (e.g., `french-gsd-ud-2.5`); French models
  often leave `xpos` empty, but the extractor relies on `upos` and `feats`.

See the respective package documentation for installation instructions.

## Function Arguments

The `biber()` function accepts the following arguments:

- `tokens`: Pre-parsed text data from `spacyr::spacy_parse()` or `udpipe::udpipe_annotate()`
- `measure`: Type-token ratio measure (`"MATTR"`, `"TTR"`, `"CTTR"`, `"MSTTR"`, or `"none"`)
- `normalize`: Whether to normalize counts to per 1,000 tokens (`TRUE`/`FALSE`)

```r
# Example with custom parameters
features <- biber(parsed_data, 
                  measure = "MATTR", 
                  normalize = TRUE)
```

## Development and Testing

pseudobibeR.fr uses [testthat](https://testthat.r-lib.org/) for comprehensive unit testing. To avoid distributing large language models with the package, tests use pre-saved parsing outputs.

### Test Structure

- `tests/testthat/text-samples/samples.tsv`: Sample sentences for testing
- `tests/testthat/text-samples/parse-samples.R`: Script to generate parsed samples  
- `tests/testthat/text-samples/*.rds`: Saved parsing outputs for tests

### Updating Test Data

If you modify `samples.tsv`, regenerate the parsed samples:

```r
# Navigate to tests/testthat/text-samples/
source("parse-samples.R")
```

### Running Tests

```r
# Run all tests
testthat::test_package("pseudobibeR.fr")

# Run specific test files
testthat::test_file("tests/testthat/test-features.R")
```

## Package Data

The package includes several built-in datasets:

- `dict`: Dictionary patterns for feature detection (e.g., third-person pronouns, conjunctions)
- `word_lists`: Exact word lists for specific features  
- `spacy_samples`: Sample spaCy-parsed text data
- `udpipe_samples`: Sample udpipe-parsed text data

These can be examined to understand feature definitions:

```r
# View available dictionaries
names(dict)

# Examine specific word lists
word_lists$f_06_first_person_pronouns

# Load sample data
data(spacy_samples)
data(udpipe_samples)
```

The sample objects are rebuilt with `data-raw/build_french_samples.R`, which
normalizes spaCy morphological attributes to plain strings before saving.

## Updating dictionaries and examples

Collaborators can extend the lexical resources that power the
feature extractor. The YAML files live in `data-raw/` and are converted into
package data (`dict.rda`, `word_lists.rda`, `french_examples.rda`) via the
`build_french_dictionaries.R` script.

### 1. Edit the YAML files

- `data-raw/dict.yaml`: maps each feature ID (e.g. `f_45_conjuncts`) to a list
  of underscore-separated lemmas or multiword patterns. All entries are
  lower-cased automatically; comments beginning with `#` are allowed.
- `data-raw/word_lists.yaml`: collects helper lists (pronouns, suffixes,
  stop-lists, etc.). Write phrases normally (`"à peine"`); the build script
  will normalize spaces to underscores as needed.
- `data-raw/french_examples.yaml`: provides illustrative sentences. Each
  entry is an object of the form:

  ```yaml
  - feature: f_05_time_adverbials
    example: Nous partirons demain matin.
    count: 1
  ```

  The optional `count` field is useful when a sentence demonstrates a feature
  more than once; omit it to default to `1`.

### 2. Rebuild the packaged data

From the project root, run:

```r
source("data-raw/build_french_dictionaries.R")
```

This script lowercases, de-duplicates, and saves the refreshed `dict`,
`word_lists`, and `french_examples` objects under `data/`. Re-run
`source("data-raw/build_french_samples.R")` if you also adjust the example
corpora.

### 3. Validate the changes

- `devtools::test()` confirms the feature detectors still pass the test suite.
- `devtools::document()` refreshes the help topics so the packaged
  documentation reflects any newly documented lexical cues.

Remember to commit the updated YAML files *and* the regenerated `.rda` assets
in `data/` so the package bundle stays in sync.

## Citation

When using pseudobibeR.fr in your research, please cite:

**The original Biber (1985) paper:**
> Biber, D. (1985). Investigating macroscopic textual variation through multifeature/multidimensional analyses. *Linguistics*, 23(2), 337-360. DOI: [10.1515/ling.1985.23.2.337](https://doi.org/10.1515/ling.1985.23.2.337)

**This package:**
> Brown, D. W. (2024). pseudobibeR.fr: French Morphological Feature Extraction for Biber Dimensions. R package version 0.0.0.9000. <https://github.com/browndw/pseudobibeR.fr>

## Linguistic Features Extracted

The package extracts 67 linguistic features organized into 16 categories based on Biber (1985). Each feature is identified through a combination of part-of-speech patterns, dependency relations, and lexical matching.

### Feature Categories

| Category | Features | Description |
|----------|----------|-------------|
| **A. Tense and aspect markers** | f_01-f_03 | Past tense, perfect aspect, present tense |
| **B. Place and time adverbials** | f_04-f_05 | Spatial and temporal adverbials |
| **C. Pronouns and pro-verbs** | f_06-f_12 | Personal pronouns, demonstratives, indefinites, pro-verb *do* |
| **D. Questions** | f_13 | Direct *wh*-questions |
| **E. Nominal forms** | f_14-f_16 | Nominalizations, gerunds, other nouns |
| **F. Passives** | f_17-f_18 | Agentless and *by*-passives |
| **G. Stative forms** | f_19-f_20 | *be* as main verb, existential *there* |
| **H. Subordination features** | f_21-f_38 | Various subordinate clause types |
| **I. Prepositional phrases, adjectives and adverbs** | f_39-f_42 | Prepositional phrases, attributive/predicative adjectives, adverbs |
| **J. Lexical specificity** | f_43-f_44 | Type-token ratio, average word length |
| **K. Lexical classes** | f_45-f_51 | Conjuncts, downtoners, hedges, amplifiers, emphatics, etc. |
| **L. Modals** | f_52-f_54 | Possibility, necessity, and predictive modals |
| **M. Specialized verb classes** | f_55-f_58 | Public, private, suasive verbs, *seem/appear* |
| **N. Reduced forms and dispreferred structures** | f_59-f_63 | Contractions, deletions, split constructions |
| **O. Co-ordination** | f_64-f_65 | Phrasal and clausal coordination |
| **P. Negation** | f_66-f_67 | Synthetic and analytic negation |

### Detailed Feature List

| Feature                     | Exemples d'équivalents français                                                                                                                                                         |
|--------------------------------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------|
| **A. Tense and aspect markers**                                 |                                                                            |
| f\_01\_past_tense                                  | Passé simple / imparfait / composé / plus-que-parfait                                                                                                    |
| f\_02\_perfect_aspect                              | Action achevée                                                                                                                                           |
| f\_03\_present_tense                               | Présent de l’indicatif                                                                                                                                   |
| **B. Place and time adverbials**                     |                                                                                                                                                          |
| f\_04\_place_adverbials                            | ici, là, dehors, dedans, au-dessus…                                                                                                                      |
| f\_05\_time_adverbials                             | hier, bientôt, soudain, ensuite…                                                                                                                         |
| **C. Pronouns and pro-verbs**                        |                                                                                                                                                          |
| f\_06\_first_person_pronouns                       | je, nous, me, moi…                                                                                                                                       |
| f\_07\_second_person_pronouns                      | tu, vous, te, toi…                                                                                                                                       |
| f\_08\_third_person_pronouns                       | il, elle, ils, elles, lui, leur…                                                                                                                         |
| f\_09\_pronoun_it                                  | il (explétif / non référentiel) / le (ex : nous le sommes, il l'est...)                                                                                  |
| f\_10\_demonstrative_pronoun                       | celui, celle, ceux, celles                                                                                                                               |
| f\_11\_indefinite_pronoun                          | quelqu’un, rien, tout, certains…                                                                                                                         |
| f\_12\_proverb_do                                  | verbe “faire” employé pro-verbalement (ex : “Je le fais”, “Il l'a pris”)                                                                                                |
| **D. Questions**                                     |                                                                                                                                                          |
| f\_13\_wh_question                                 | qui, que, quoi, où, quand, comment, pourquoi                                                                                                             |
| **E. Nominal forms**                                 |                                                                                                                                                          |
| f\_14\_nominalization                              | liste de suffixe : [référence PDF](https://facmed.univ-constantine3.dz/wp-content/uploads/2024/01/La-nominalisation-des-verbes-daction.pdf)             |
| f\_15\_gerunds                                     | en travaillant, en mangeant...                                                                                               |
| f\_16\_other_nouns                                 | autres noms                                                                                                                                              |
| **F. Passives**                                      |                                                                                                                                                          |
| f\_17\_agentless_passives                          | “est mangé”, sans “par…”                                                                                                                                 |
| f\_18\_by_passives                                 | “est mangé par le chat”                                                                                                                                  |
| **G. Stative forms**                                 |                                                                                                                                                          |
| f\_19\_be_main_verb                                | “être” comme verbe principal                                                                                                                             |
| f\_20\_existential_there                           | “il y a”                                                                                                                                                 |
| **H. Subordination features**                        |                                                                                                                                                          |
| f\_21\_that_verb_comp                              | “que” après verbe (“Je crois que…”)                                                                                                                      |
| f\_22\_that_adj_comp                               | “content que”, “heureux que…”                                                                                                                            |
| f\_23\_wh_clause                                   | “ce que”, “ce qui”, “ce dont…”                                                                                                                           |
| f\_24\_infinitives                                 | verbes à l'infinitif                                                                                                                                          |
| f\_25\_present_participle                          | “En mangeant, il partit”                                                                                                                                 |
| f\_26\_past_participle                             | “Construit en un jour…”                                                                                                                                  |
| f\_27\_past_participle_whiz                        | “la maison construite hier est magnifique”                                                                                                                              |
| f\_28\_present_participle_whiz                     | “les enfants jouant dehors sont bruyants”                                                                                                                              |
| f\_29\_that_subj                                   | “le chien qui m’a mordu”                                                                                                                                 |
| f\_30\_that_obj                                    | “le chien que j’ai vu”                                                                                                                                   |
| f\_31\_wh_subj                                     | “les candidats, lesquels réussissent…”                                                                                                                                           |
| f\_32\_wh_obj                                      | “les raisons dont il parle…”                                                                                                                                          |
| f\_33\_pied_piping                                 | “c'est... dont”, “voici... dans lequel…”                                                                                                                |
| f\_34\_sentence_relatives                          | “..., ce qui est étrange.”                                                                                                                               |
| f\_35\_because                                     | “parce que, puisque, car”                                                                                                                                |
| f\_36\_though                                      | “bien que, quoique, même si”                                                                                                                             |
| f\_37\_if                                          | “si, à moins que”                                                                                                                                        |
| f\_38\_other_adv_sub                               | “comme, tandis que, alors que, lorsque”                                                                                                                  |
| **I. Prepositional phrases, adjectives and adverbs** |                                                                                                                                                          |
| f\_39\_prepositions                                | à, de, en, sur, sous, pour, avec…                                                                                                                        |
| f\_40\_adj_attr                                    | "c'est une belle maison."                                                                                                                                |
| f\_41\_adj_pred                                    | “le chat est noir”                                                                                                                                      |
| f\_42\_adverbs                                     | tout, très, rapidement…                                                                                                                                  |
| **J. Lexical specificity**                           |                                                                                                                                                          |
| f\_43\_type_token                                  | ratio types/tokens                                                                                                                                       |
| f\_44\_mean_word_length                            | longueur moyenne de mots                                                                                                                                 |
| **K. Lexical classes**                               |                                                                                                                                                          |
| f\_45\_conjuncts                                   | cependant, en revanche, de plus                                                                                                                          |
| f\_46\_downtoners                                  | à peine, presque, légèrement                                                                                                                             |
| f\_47\_hedges                                      | environ, quelque peu, on dirait                                                                                                                          |
| f\_48\_amplifiers                                  | très, vraiment, extrêmement                                                                                                                              |
| f\_49\_emphatics                                   | beaucoup, bien sûr, absolument                                                                                                                           |
| f\_50\_discourse_particles                         | eh bien, enfin, bref                                                                                                                                     |
| f\_51\_demonstratives                              | ce, cette, ces, cet                                                                                                                                      |
| **L. Modals**                                        |                                                                                                                                                          |
| f\_52\_modal_possibility                           | pouvoir                                                                                                                                                  |
| f\_53\_modal_necessity                             | devoir, falloir                                                                                                                                          |
| f\_54\_modal_predictive                            | aller + infinitif, futur simple                                                                                                                          |
| **M. Specialized verb classes**                      |                                                                                                                                                          |
| f\_55\_verb_public                                 | affirmer, déclarer, annoncer                                                                                                                             |
| f\_56\_verb_private                                | penser, croire, savoir, douter                                                                                                                           |
| f\_57\_verb_suasive                                | ordonner, proposer, insister                                                                                                                             |
| f\_58\_verb_seem                                   | sembler, paraître                                                                                                                                        |
| **N. Reduced forms and dispreferred structures**     |                                                                                                                                                          |
| f\_59\_contractions                                | (n’existe pas vraiment ; “p’tit”, “t’es”, “t’as”... à l'oral)                                                                                                     |
| f\_60\_that_deletion                               | suppression de “que” (“je pense il vient”)                                                                                                               |
| f\_61\_stranded_preposition                        | "C'est la personne avec qui j'ai des problèmes." |
| f\_62\_split_infinitive                            | “j'essaie de vraiment comprendre”                                                                                                                                 |
| f\_63\_split_auxiliary                             | “a probablement été vu”                                                                                                                                  |
| **O. Coordination**                                  |                                                                                                                                                          |
| f\_64\_phrasal_coordination                        | "“N et N”, “Adj et Adj” (ex : ""de tels... et de tels..."", ""par... et par..."""                                                                        |
| f\_65\_clausal_coordination                        | “et” entre propositions indépendantes                                                                                                                    |
| **P. Negation**                                      |                                                                                                                                                          |
| f\_66\_neg_synthetic                               | “Aucune réponse n’est bonne”                                                                                                                             |
| f\_67\_neg_analytic                                | “ne/n'/Ø … pas / plus / jamais”                                                                                                                               |

## Contributing

Contributions are welcome! Please feel free to submit issues or pull requests on [GitHub](https://github.com/browndw/pseudobibeR.fr).

### Reporting Issues

When reporting bugs or requesting features, please include:

- A minimal reproducible example
- Your R version and package versions
- The parsing pipeline used (spaCy/udpipe)

## License

This package is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.

## Related Resources

- [Biber (1985) original paper](https://doi.org/10.1515/ling.1985.23.2.337)
- [quanteda](https://quanteda.io/) - Text analysis framework used internally
- [spaCy](https://spacy.io/) - Industrial-strength NLP library  
- [udpipe](https://bnosac.github.io/udpipe/en/) - NLP toolkit for R
