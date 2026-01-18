# Data Preparation Scripts

This directory contains scripts that build package datasets and test fixtures for
`pseudobibeR.fr`. Generated artifacts (for example, intermediate CSV exports)
should be written to `data-raw/probe-output/` and ignored via `.Rbuildignore`.

## Overview

The package data infrastructure has three main layers:

1. **Source YAML files** - Human-editable specifications
2. **Build scripts** - R scripts that transform YAML into package data
3. **Package data & test fixtures** - `.rda` files and test expectations

## Workflow for Collaborators

### Adding or Updating Lexical Features

#### 1. Edit YAML Source Files

**`dict.yaml`** - Dictionary patterns for lexical features (e.g., modal verbs, discourse markers)
- Each feature is a YAML list of terms/phrases
- Multiword expressions use underscores: `avoir_la_possibilité`
- Run: `source("data-raw/build_french_dictionaries.R")`

**`word_lists.yaml`** - Support lists for pattern matching
- `pronoun_matchlist` - pronouns for extraction
- `impersonal_verbs` - verbs like "falloir", "pleuvoir"  
- `nominalization_suffixes` - endings like "-tion", "-ment"
- `multiword_patterns` - manually-specified phrases
- Run: `source("data-raw/build_french_dictionaries.R")`

#### 2. Rebuild Package Data

After editing `dict.yaml` or `word_lists.yaml`:

```r
source("data-raw/build_french_dictionaries.R")
```

This creates/updates:
- `data/dict.rda` - Dictionary object used by `biber()`
- `data/word_lists.rda` - Support lists
- `data/french_examples.rda` - Test examples (from `french_examples.yaml`)

**When to rebuild:**
- After adding new feature terms to `dict.yaml`
- After updating support lists in `word_lists.yaml`
- After adding/editing examples in `french_examples.yaml`

### Updating Test Expectations

#### 3. Edit Test YAML Files

**`french_examples.yaml`** - Expected counts for individual features
- Each entry has: `feature`, `example` sentence, `count` (expected occurrences)
- Used by `test-french-examples.R` to validate feature extraction
- Format:
  ```yaml
  - feature: f_01_past_tense
    example: Il mangea rapidement son repas.
    count: 1.0
  ```

**`french_edge_cases.yaml`** - Complex linguistic constructions
- Organized by linguistic category (e.g., `svc_passive_postnominal`, `tough_constructions`)
- Used by `test-french-edge-cases.R` for regression testing
- Format:
  ```yaml
  edge_cases:
    category_name:
      - Sentence one.
      - Sentence two.
  ```

#### 4. Regenerate Test Fixtures

After editing `french_edge_cases.yaml`:

```r
source("data-raw/generate_edge_case_fixture.R")
```

This creates/updates:
- `tests/testthat/fixtures/french_edge_case_features.csv` - Expected feature counts

The test `test-french-edge-cases.R` compares live extraction against this fixture.

**When to regenerate:**
- After adding new edge cases to `french_edge_cases.yaml`
- After fixing bugs that change edge case feature extraction
- After intentional changes to feature detection algorithms

### Updating Sample Data

**For Chambers & LeBaron corpus samples:**

```r
source("data-raw/build_french_samples.R")
```

This requires:
- ChambersLeBaron corpus at `../ChambersLeBaron/2527_Corpus_text_files/`
- UDPipe model at `../tests/french-gsd-ud-2.5-191206.udpipe` or `tests/models/`
- spacyr with French model: `fr_core_news_sm`

Creates/updates:
- `data/udpipe_samples.rda` - Example parsed documents (udpipe)
- `data/spacy_samples.rda` - Example parsed documents (spaCy)
- `data-raw/french_raw_text.tsv` - Raw text samples

**When to rebuild:**
- When updating to new parser models
- When refreshing example documents

### Helper Scripts

**`extract_french_lists.R`** - Extract candidate lexical terms from Sharoff's translations
- Reads `../biber_features_equivalents_french.csv`
- Outputs to `probe-output/french_feature_terms.csv`
- Used for initial feature list construction (not part of regular workflow)

**`french_udpipe_probe.R`** - Diagnostic script for exploring udpipe parse output
- Useful for debugging feature extraction
- Outputs diagnostic CSVs to `probe-output/`

## Complete Rebuild Sequence

To rebuild all package data from scratch:

```r
# 1. Build dictionaries and word lists (required)
source("data-raw/build_french_dictionaries.R")

# 2. Rebuild sample data (optional, requires corpus access)
# source("data-raw/build_french_samples.R")

# 3. Regenerate edge case test fixtures (required after dict changes)
source("data-raw/generate_edge_case_fixture.R")

# 4. Re-document and check
devtools::document()
devtools::check()
```

## Dependency Chain

```
dict.yaml ──┐
            ├──> build_french_dictionaries.R ──> data/dict.rda ──┐
word_lists.yaml ──┘                                              │
                                                                  ├──> biber() function
french_examples.yaml ──> build_french_dictionaries.R ──>         │
                         data/french_examples.rda ──────────────┘

french_edge_cases.yaml ──> generate_edge_case_fixture.R ──>
                           tests/testthat/fixtures/french_edge_case_features.csv
                           (compared in test-french-edge-cases.R)
```

## Best Practices

1. **Version control**: Commit YAML changes with clear descriptions
2. **Test before merging**: Always run full test suite after rebuilding
3. **Document changes**: Note why features were added/modified
4. **Edge case coverage**: Add edge cases when fixing bugs
5. **Regenerate fixtures**: Don't manually edit CSV fixtures

## Troubleshooting

**Error: "UDPipe model missing"**
- Place `french-gsd-ud-2.5-191206.udpipe` in `tests/` or `tests/models/`

**Error: "ChambersLeBaron corpus not found"**
- Only needed for `build_french_samples.R`
- Can skip if not updating sample data

**Tests failing after dict update:**
1. Verify YAML syntax is valid
2. Rebuild with `build_french_dictionaries.R`
3. Regenerate fixtures with `generate_edge_case_fixture.R`
4. Review test output for expected vs actual counts

