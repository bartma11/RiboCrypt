df <- ORFik::ORFik.template.experiment()[9:10, ]
tx <- ORFik::loadRegion(df, "mrna")
cds <- ORFik::loadRegion(df, "cds")

test_that("multiOmicsPlot_ORFikExp works as intended", {
  t <- tx[1]
  c <- cds[1]
  reads <- ORFik::filepath(df[1,], "bigwig")[[1]]
  res <- multiOmicsPlot_ORFikExp(display_range = t, df = df[1,], annotation = c, reads = reads)
  # profvis::profvis(multiOmicsPlot_ORFikExp(display_range = t, df = df[1,], annotation = c, reads = reads))
  expect_true(isTRUE(res$x$layout$yaxis$showticklabels))
  expect_length(res$x$layout$yaxis$tickvals, 3)
  expect_false(any(res$x$layout$yaxis$tickvals == 0))
  expect_true(isTRUE(res$x$layout$yaxis$fixedrange))
})

test_that("single-transcript gene model stays on one layer", {
  t <- tx[1]
  c <- cds[1]

  gene_model <- createGeneModelPanel(
    display_range = t,
    annotation = c,
    tx_annotation = NULL,
    custom_regions = NULL,
    viewMode = "tx",
    collapse_intron_flank = 100,
    frame_colors = "R"
  )[[1]]

  expect_true(nrow(gene_model) >= 1)
  expect_identical(unique(gene_model$layers), 1L)
  expect_false(any(gene_model$type %in% c("intron", "intron_collapsed")))
})
