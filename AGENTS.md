# Repository Guidelines

## Project Structure & Module Organization

RiboCrypt is an R/Bioconductor package with a Shiny application. Package code lives in `R/`; files named `page_*.R` define application pages, while browser, plotting, reactive, and helper modules are grouped by responsibility. Tests are in `tests/testthat/`, with the package test entry point at `tests/testthat.R`. User-facing vignettes belong in `vignettes/`. Static Shiny resources—including JavaScript, images, and rendered tutorial files—live under `inst/`. Files in `man/` and `NAMESPACE` are generated from roxygen2 comments and should not be edited by hand.

## Build, Test, and Development Commands

Run commands from the repository root:

- `R -q -e 'devtools::load_all()'` loads the package for interactive development.
- `R -q -e 'devtools::test()'` runs the complete `testthat` suite.
- `R -q -e 'devtools::test(filter = "browser")'` runs a focused group of tests.
- `R -q -e 'devtools::document()'` regenerates `NAMESPACE` and `man/*.Rd` after API or roxygen changes.
- `R CMD build .` creates the source package archive.
- `R CMD check --no-manual RiboCrypt_*.tar.gz` performs the package-level validation expected before review.
- `docker compose -f compose.dev.yml build` builds the Docker development image.
- `docker compose -f compose.dev.yml run --rm ribocrypt-dev Rscript scripts/prepare_dev_data.R` prepares the persistent demo data and index on first use.
- `docker compose -f compose.dev.yml up -d` starts the development app at `http://localhost:3838`; use `docker compose -f compose.dev.yml logs -f ribocrypt-dev` to follow its logs and `docker compose -f compose.dev.yml stop` to stop it while retaining the container and demo data.

`Rscript run_dev.R` starts the development app, but its dataset paths are workstation-specific; adjust them locally and do not commit personal paths.

## Coding Style & Naming Conventions

Use two-space indentation and conventional R spacing (`x <- value`, spaces after commas). Prefer descriptive `snake_case` for new helpers and variables, while preserving established exported names such as `RiboCrypt_app()` for compatibility. Keep UI pages, reactive logic, and data transformations in their existing modules. Document exported functions with roxygen2, including `@param`, `@return`, and `@export`. Use explicit namespace qualifiers in tests when they improve clarity.

## Testing Guidelines

Write tests with `testthat`; name files `test_<feature>.R` and use behavior-focused `test_that()` descriptions. Add regression coverage for bug fixes and test both normal and edge-case inputs. Avoid network calls, large datasets, and machine-specific files in unit tests. Run focused tests during development and the full suite before submitting.

## Commit & Pull Request Guidelines

Recent commits use short, sentence-style summaries such as `Fixed custom bigwig x range bug`. Keep each commit focused and describe the observable change in the imperative or simple past tense. Pull requests should explain the motivation, summarize implementation and tests, and link relevant issues. Include screenshots or recordings for Shiny UI changes and note any generated documentation or compatibility impact.
