# Minimal Dataset and Containerized App Plan

## Summary

Create a reproducible Docker workflow for the core RiboCrypt browser using ORFik's bundled simulated human dataset. This avoids downloading raw FASTQ/BAM data and intentionally leaves collection-dependent MegaBrowser, Observatory, UMAP, metadata, and translon features disabled.

Use the official `bioconductor/bioconductor_docker:RELEASE_3_23` base, which provides R 4.6 and Bioconductor 3.23 with common system dependencies preinstalled. See the [Bioconductor container documentation](https://bioconductor.org/help/docker/).

## Implementation

- Install Docker Engine as a host prerequisite; no container runtime is currently installed.
- Add a `Dockerfile` that:
  - Pins `bioconductor/bioconductor_docker:RELEASE_3_23`.
  - Copies the repository into `/workspace/RiboCrypt`.
  - Installs RiboCrypt's dependencies from `DESCRIPTION` using Bioconductor-aware repositories.
  - Installs the checked-out repository with `R CMD INSTALL`.
  - Exposes Shiny port `3838`.
- Add an idempotent dataset-preparation script that:
  - Uses `system.file("extdata/Homo_sapiens_sample", package = "ORFik")`.
  - Locates `Homo_sapiens_dummy.gtf.db` and `Homo_sapiens_dummy.fasta` recursively under ORFik's `extdata`, accommodating the layouts shown by both repository and current ORFik documentation.
  - Creates `ORFik_tutorial_data` under `ORFik::config()["exp"]` with `create.experiment(..., types = "ofst")`.
  - Loads the saved experiment and runs `convert_to_bigWig()` only when its BigWig tracks are absent.
  - Fails early if sample libraries, annotation, FASTA, experiment CSV, or generated tracks cannot be found.
- Persist `/home/rstudio/Bio_data` in a named Docker volume so preparation occurs once and the ORFik configuration and derived BigWigs survive container replacement.
- Add a launch script that:
  - Discovers and validates `ORFik_tutorial_data` with `list.experiments()`.
  - Calls `RiboCrypt_app(all_exp = ..., all_exp_meta = NULL, metadata = NULL)`.
  - Selects the tutorial experiment, the documented CAGE/PAS/RFP/RNA libraries, column-style frames, and automatic initial plotting.
  - Runs Shiny on `0.0.0.0:3838` with browser launching disabled inside the container.
- Document exact commands to build the image, prepare the volume, start the app with `-p 3838:3838`, and open `http://localhost:3838`.

## Interfaces and Failure Handling

- No R package APIs or dataset schemas change.
- Operational interfaces are the Docker image, named data volume, preparation script, and port `3838`.
- Dataset preparation is safe to rerun: reuse a valid experiment, regenerate only missing BigWigs, and stop on incomplete data rather than silently launching.
- Keep the existing workstation-specific `run_dev.R` unchanged; the container launch script becomes the portable demo entry point.

## Verification

- Build the image from a clean Docker cache.
- Prepare the dataset in an empty named volume and confirm:
  - `ORFik_tutorial_data` appears in `list.experiments(validate = TRUE)`.
  - The experiment loads its TxDb, FASTA, and at least one library.
  - Required BigWig paths exist and are readable.
- Run preparation a second time and verify it is idempotent.
- Start the container and confirm the root endpoint returns HTTP 200.
- In a browser, verify the tutorial experiment is selected, a gene can be plotted, sequence/gene-model panels render, and at least one coverage track displays.
- Confirm logs report that collection and metadata features are skipped without terminating the app.

## Assumptions

- "Minimal working" means the standard single-experiment browser, not collection-only features.
- The bundled ORFik sample is sufficient and no public sequencing dataset needs to be downloaded.
- Docker installation is handled as a host prerequisite; repository automation begins after `docker version` succeeds.
- The container image is pinned to Bioconductor 3.23 rather than `latest` or `devel` for reproducibility.
