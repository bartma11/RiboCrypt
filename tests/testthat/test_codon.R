test_that("ordinary codon plots exclude start and stop codons when requested", {
  codons <- c("#:ATG", "*:TAA", "*:TAG", "*:TGA", "A:GCT", "K:AAA")
  coverage <- data.table::CJ(
    variable = "lib1",
    type = c("A", "P"),
    seqs = codons
  )
  coverage[, seqs := factor(seqs, levels = codons)]
  coverage[, `:=`(
    relative_to_max_score = as.numeric(seq_len(.N)),
    N.total = 200L
  )]
  controls <- list(
    ratio_thresh = 1.7,
    exclude_start_stop = TRUE,
    codon_score = "percentage",
    differential = FALSE,
    background = NULL,
    only_significant_difexp = FALSE,
    plot_export_format = "png"
  )

  excluded_plot <- RiboCrypt:::click_plot_codon_shiny(
    controls, data.table::copy(coverage)
  )
  excluded_labels <- plotly::plotly_build(excluded_plot)$x$layout$yaxis$ticktext

  controls$exclude_start_stop <- FALSE
  included_plot <- RiboCrypt:::click_plot_codon_shiny(
    controls, data.table::copy(coverage)
  )
  included_labels <- plotly::plotly_build(included_plot)$x$layout$yaxis$ticktext

  expect_setequal(excluded_labels, c("A:GCT", "K:AAA"))
  expect_false(any(grepl("^(#|\\*)", excluded_labels)))
  expect_true(all(c("#:ATG", "*:TAA", "*:TAG", "*:TGA") %in% included_labels))
  expect_false(any(grepl("^(#|\\*)", as.character(
    attr(excluded_plot, "input_data")$seqs
  ))))
})
