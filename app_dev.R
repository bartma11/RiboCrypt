pkgload::load_all(".", quiet = TRUE)

experiment_name <- "ORFik_tutorial_data"
experiment_dir <- ORFik::config()[["exp"]]
experiment_file <- file.path(experiment_dir, paste0(experiment_name, ".csv"))

if (!file.exists(experiment_file)) {
  stop(
    "Demo data is not prepared. Run scripts/prepare_demo_data.R first.",
    call. = FALSE
  )
}

all_exp <- ORFik::list.experiments(
  validate = TRUE,
  BPPARAM = BiocParallel::SerialParam(),
  dir = experiment_dir
)
all_exp <- all_exp[all_exp$name == experiment_name, ]
if (nrow(all_exp) != 1L) {
  stop("Could not discover exactly one tutorial experiment.", call. = FALSE)
}

experiment <- ORFik::read.experiment(
  experiment_name,
  validate = TRUE,
  in.dir = experiment_dir
)
documented_libraries <- c(
  "CAGE_WT_r1",
  "PAS_WT_r1",
  "RFP_WT_r1",
  "RNA_WT_r1"
)
available_libraries <- ORFik::bamVarName(experiment)
selected_libraries <- intersect(documented_libraries, available_libraries)
if (length(selected_libraries) == 0L) {
  selected_libraries <- available_libraries[[1L]]
  warning(
    "The documented tutorial library names were unavailable; using ",
    selected_libraries, "."
  )
}

RiboCrypt::RiboCrypt_app(
  validate.experiments = TRUE,
  all_exp = all_exp,
  all_exp_meta = NULL,
  metadata = NULL,
  browser_options = c(
    default_experiment = experiment_name,
    default_libs = paste(selected_libraries, collapse = ","),
    default_frame_type = "columns",
    plot_on_start = "TRUE"
  ),
  options = list(launch.browser = FALSE)
)
