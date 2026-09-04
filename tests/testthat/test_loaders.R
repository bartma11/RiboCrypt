make_multifolder_coverage_fixture <- function(root, same_stem = FALSE) {
  study_folders <- file.path(root, c("study-a", "study-b"))
  for (study_folder in study_folders) {
    dir.create(file.path(study_folder, "cov_RLE"), recursive = TRUE)
    dir.create(file.path(study_folder, "cov_RLE_List"), recursive = TRUE)
    dir.create(file.path(study_folder, "pshifted"), recursive = TRUE)
  }

  stems <- if (same_stem) rep("sample", 2) else c("sample-a", "sample-b")
  dff <- ORFik::ORFik.template.experiment()[9:10, ]
  dff@listData$filepath <- file.path(study_folders, paste0(stems, ".ofst"))
  dff@envir <- new.env(parent = globalenv())

  cov_paths <- file.path(study_folders, "cov_RLE", paste0(stems, ".covrds"))
  covl_paths <- file.path(
    study_folders, "cov_RLE_List", paste0(stems, ".covrds")
  )
  for (i in seq_along(study_folders)) {
    coverage <- ORFik::covRle(IRanges::RleList(
      chr1 = S4Vectors::Rle(c(0L, i, i + 1L))
    ))
    coverage_by_length <- ORFik::covRleList(list(`28` = coverage))
    saveRDS(coverage, cov_paths[[i]])
    saveRDS(coverage_by_length, covl_paths[[i]])
  }

  list(
    dff = dff,
    study_folders = study_folders,
    cov_paths = cov_paths,
    covl_paths = covl_paths
  )
}

test_that("covRle path helpers resolve every folder in a collection", {
  fixture <- make_multifolder_coverage_fixture(
    withr::local_tempdir(), same_stem = FALSE
  )

  expect_error(
    ORFik::filepath(
      fixture$dff, "cov", suffix_stem = c("_pshifted", "")
    ),
    "File did not exist"
  )
  expect_error(
    ORFik::filepath(
      fixture$dff, "covl", suffix_stem = c("_pshifted", "")
    ),
    "File did not exist"
  )

  expect_identical(
    RiboCrypt:::get_covRle_paths_internal(fixture$dff),
    fixture$cov_paths
  )
  expect_identical(
    RiboCrypt:::get_covRleList_paths_internal(fixture$dff),
    fixture$covl_paths
  )
})

test_that("collection path helpers reject duplicate first-folder matches", {
  fixture <- make_multifolder_coverage_fixture(
    withr::local_tempdir(), same_stem = TRUE
  )
  first_folder_paths <- ORFik::filepath(
    fixture$dff, "cov", suffix_stem = c("_pshifted", "")
  )

  expect_true(anyDuplicated(first_folder_paths) > 0)
  expect_false(RiboCrypt:::library_paths_are_valid(
    first_folder_paths, nrow(fixture$dff)
  ))
  expect_identical(
    RiboCrypt:::get_covRle_paths_internal(fixture$dff),
    fixture$cov_paths
  )
  expect_identical(
    RiboCrypt:::get_covRleList_paths_internal(fixture$dff),
    fixture$covl_paths
  )
})

test_that("covRle loaders import coverage from multiple study folders", {
  fixture <- make_multifolder_coverage_fixture(
    withr::local_tempdir(), same_stem = TRUE
  )

  loaded_cov <- suppressMessages(RiboCrypt:::load_covRle(fixture$dff))
  loaded_covl <- suppressMessages(RiboCrypt:::load_covRleList(fixture$dff))

  expect_length(loaded_cov, 2)
  expect_length(loaded_covl, 2)
  expect_true(all(vapply(loaded_cov, methods::is, logical(1), "covRle")))
  expect_true(all(vapply(loaded_covl, methods::is, logical(1), "covRleList")))
  expect_identical(
    unname(vapply(loaded_cov, attr, character(1), "filepath")),
    fixture$cov_paths
  )
  expect_identical(
    unname(vapply(loaded_covl, attr, character(1), "filepath")),
    fixture$covl_paths
  )
})

test_that("shift loading follows the selected library's study folder", {
  fixture <- make_multifolder_coverage_fixture(withr::local_tempdir())
  study_a_shift <- list(data.table::data.table(
    fraction = 28L, offsets_start = -9L
  ))
  names(study_a_shift) <- paste0(fixture$dff$filepath[[1]], "/")
  saveRDS(
    study_a_shift,
    file.path(fixture$study_folders[[1]], "pshifted", "shifting_table.rds")
  )

  study_b_shifts <- list(
    other = data.table::data.table(fraction = 28L, offsets_start = -10L),
    selected = data.table::data.table(
      fraction = c(27L, 28L), offsets_start = c(-12L, -13L)
    )
  )
  names(study_b_shifts) <- c(
    file.path(fixture$study_folders[[2]], "other.ofst"),
    paste0(fixture$dff$filepath[[2]], "/")
  )
  saveRDS(
    study_b_shifts,
    file.path(fixture$study_folders[[2]], "pshifted", "shifting_table.rds")
  )

  selected_shift <- RiboCrypt:::load_library_shift_table(fixture$dff[2, ])

  expect_s3_class(selected_shift, "data.table")
  expect_equal(selected_shift$fraction, c(27L, 28L))
  expect_equal(selected_shift$offsets_start, c(-12L, -13L))
})

test_that("missing selected-library shift tables fail softly", {
  fixture <- make_multifolder_coverage_fixture(withr::local_tempdir())

  expect_warning(
    shift_table <- RiboCrypt:::load_library_shift_table(fixture$dff[2, ]),
    "Shift table not found for the selected library"
  )
  expect_s3_class(shift_table, "data.table")
  expect_equal(nrow(shift_table), 0)
})

test_that("shift loading does not guess named mismatches or malformed tables", {
  fixture <- make_multifolder_coverage_fixture(withr::local_tempdir())
  shift_path <- file.path(
    fixture$study_folders[[2]], "pshifted", "shifting_table.rds"
  )

  mismatched_shift <- list(data.table::data.table(
    fraction = 28L, offsets_start = -9L
  ))
  names(mismatched_shift) <- "a-different-library.ofst"
  saveRDS(mismatched_shift, shift_path)

  expect_warning(
    shift_table <- RiboCrypt:::load_library_shift_table(fixture$dff[2, ]),
    "Could not match the selected library"
  )
  expect_equal(nrow(shift_table), 0)

  malformed_shift <- list(data.table::data.table(fraction = 28L))
  names(malformed_shift) <- fixture$dff$filepath[[2]]
  saveRDS(malformed_shift, shift_path)

  expect_warning(
    shift_table <- RiboCrypt:::load_library_shift_table(fixture$dff[2, ]),
    "shift table is malformed"
  )
  expect_equal(nrow(shift_table), 0)
})

test_that("frame-bias QC does not load an unused study shift table", {
  dff <- ORFik::ORFik.template.experiment()[9, ]
  testthat::local_mocked_bindings(
    observed_gene_heatmap = function(...) "all",
    observed_cds_heatmap = function(...) "cds",
    observed_exp_subset = function(...) dff,
    load_library_shift_table = function(...) {
      stop("The QC page must not load a shift table")
    },
    load_covRleList = function(...) list("coverage"),
    .package = "RiboCrypt"
  )
  input <- list(
    tx = "all", library = "library", extendLeaders = 30,
    extendTrailers = 30, region = "Start codon", customSequence = NULL,
    normalization = "transcriptNormalized", readlength_min = 26,
    readlength_max = 34, p_shifted = NULL, viewMode = NULL,
    summary_track = NULL
  )

  controls <- RiboCrypt:::click_plot_heatmap_main_controller(
    input, function() NULL, function() NULL,
    function() "library", function() dff
  )

  expect_s3_class(shiny::isolate(controls$shift_table), "data.table")
  expect_equal(nrow(shiny::isolate(controls$shift_table)), 0)
  expect_identical(shiny::isolate(controls$reads), list("coverage"))
})

test_that("motif heatmap uses the selected library shift table", {
  dff <- ORFik::ORFik.template.experiment()[9, ]
  testthat::local_mocked_bindings(
    observed_gene_heatmap = function(...) "all",
    observed_cds_heatmap = function(...) "cds",
    observed_exp_subset = function(...) dff,
    load_library_shift_table = function(...) data.table::data.table(
      fraction = c(25L, 28L), offsets_start = c(-8L, -13L)
    ),
    load_covRleList = function(...) list("coverage"),
    .package = "RiboCrypt"
  )
  input <- list(
    tx = "all", library = "library", extendLeaders = 30,
    extendTrailers = 30, region = "Start codon", customSequence = "",
    normalization = "transcriptNormalized", readlength_min = 26,
    readlength_max = 34, p_shifted = FALSE, viewMode = FALSE,
    summary_track = FALSE
  )

  controls <- RiboCrypt:::click_plot_heatmap_main_controller(
    input, function() NULL, function() NULL,
    function() "library", function() dff
  )

  expect_equal(shiny::isolate(controls$extendLeaders), 43)
  expect_equal(shiny::isolate(controls$extendTrailers), 43)
  expect_equal(shiny::isolate(controls$shift_table)$fraction, 28L)
  expect_equal(shiny::isolate(controls$shift_table)$offsets_start, -13L)
})

test_that("motif heatmap tolerates a missing selected-study shift table", {
  fixture <- make_multifolder_coverage_fixture(withr::local_tempdir())
  selected_dff <- fixture$dff[2, ]
  testthat::local_mocked_bindings(
    observed_gene_heatmap = function(...) "all",
    observed_cds_heatmap = function(...) "cds",
    observed_exp_subset = function(...) selected_dff,
    load_covRleList = function(...) list("coverage"),
    .package = "RiboCrypt"
  )
  input <- list(
    tx = "all", library = "library", extendLeaders = 30,
    extendTrailers = 30, region = "Start codon", customSequence = "",
    normalization = "transcriptNormalized", readlength_min = 26,
    readlength_max = 34, p_shifted = TRUE, viewMode = FALSE,
    summary_track = FALSE
  )

  expect_warning(
    controls <- RiboCrypt:::click_plot_heatmap_main_controller(
      input, function() NULL, function() NULL,
      function() "library", function() fixture$dff
    ),
    "Shift table not found for the selected library"
  )
  expect_equal(nrow(shiny::isolate(controls$shift_table)), 0)
  expect_identical(shiny::isolate(controls$reads), list("coverage"))
})
