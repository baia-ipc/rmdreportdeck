make_mock_rmd <- function(path) {
  writeLines(
    c(
      "---",
      "title: \"P00 | Synthetic Report\"",
      "output: html_document",
      "params:",
      "  report_title: \"Synthetic Title\"",
      "---",
      "",
      "```{r setup, include=FALSE}",
      "library(ggplot2)",
      "library(knitr)",
      "library(rmdreportdeck)",
      "df <- data.frame(group = c(\"A\", \"B\"), value = c(2, 5))",
      "plot_obj <- ggplot(df, aes(group, value, fill = group)) + geom_col()",
      "bundle <- report_plot_bundle(plot_obj, block_prefix = \"V01.01\", suffix = \"plot\", width = 6, height = 4, data = df)",
      "```",
      "",
      "# Description",
      "",
      "Synthetic report body.",
      "",
      "## Figure",
      "",
      "```{r}",
      "print(report_bundle_asset_bar(",
      "  \"Downloads\",",
      "  bundle,",
      "  tsv_label = \"TSV\"",
      "))",
      "```",
      "",
      "```{r}",
      "print(plot_obj)",
      "```",
      "",
      "## Table",
      "",
      "```{r}",
      "knitr::kable(df)",
      "```"
    ),
    con = path,
    useBytes = TRUE
  )
}

make_mock_pdf_rmd <- function(path) {
  writeLines(
    c(
      "---",
      "title: \"P00 | Synthetic PDF Report\"",
      "output: pdf_document",
      "params:",
      "  report_title: \"Synthetic PDF\"",
      "---",
      "",
      "```{r setup, include=FALSE}",
      "library(ggplot2)",
      "df <- data.frame(group = c(\"A\", \"B\"), value = c(3, 7))",
      "```",
      "",
      "# Workflow",
      "",
      "Synthetic PDF report body.",
      "",
      "## Figure",
      "",
      "```{r}",
      "ggplot(df, aes(group, value, fill = group)) + geom_col()",
      "```"
    ),
    con = path,
    useBytes = TRUE
  )
}

make_mock_loop_rmd <- function(path, output_format = "html_document") {
  writeLines(
    c(
      "---",
      "title: \"P00 | Synthetic Loop Report\"",
      paste0("output: ", output_format),
      "---",
      "",
      "```{r setup, include=FALSE}",
      "library(ggplot2)",
      "library(rmdreportdeck)",
      "samples <- data.frame(",
      "  sample = paste0(\"S\", 1:5),",
      "  value = c(2, 4, 3, 5, 6),",
      "  stringsAsFactors = FALSE",
      ")",
      "plots_dir <- file.path(tempdir(), \"plots\")",
      "data_dir <- file.path(tempdir(), \"data\")",
      "```",
      "",
      "# Description",
      "",
      "Synthetic loop report body.",
      "",
      "## Per-sample panels",
      "",
      "```{r results='asis', echo=FALSE}",
      "items <- vector(\"list\", nrow(samples))",
      "for (i in seq_len(nrow(samples))) {",
      "  row <- samples[i, , drop = FALSE]",
      "  plot_obj <- ggplot(row, aes(sample, value, fill = sample)) + geom_col(show.legend = FALSE)",
      "  bundle <- report_plot_bundle(",
      "    plot_obj,",
      "    block_prefix = paste0(\"V01.\", sprintf(\"%02d\", i)),",
      "    suffix = row$sample,",
      "    width = 5,",
      "    height = 3.5,",
      "    data = row,",
      "    plot_dir = plots_dir,",
      "    data_dir = data_dir",
      "  )",
      "  items[[i]] <- report_item_panel(",
      "    title = paste0(\"Sample \", row$sample, \": alpha/beta\"),",
      "    asset_bar = report_bundle_asset_bar(",
      "      \"Downloads\",",
      "      bundle,",
      "      tsv_label = \"TSV\"",
      "    ),",
      "    note = \"Synthetic loop item\",",
      "    report_item_plot(plot_obj, width = 5, height = 3.5),",
      "    report_item_table(row)",
      "  )",
      "}",
      "report_loop_section(items)",
      "```"
    ),
    con = path,
    useBytes = TRUE
  )
}

skip_if_render_stack_missing <- function() {
  testthat::skip_if_not_installed("rmarkdown")
  testthat::skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")
}

test_that("report_asset_paths resolves packaged assets", {
  paths <- report_asset_paths()

  expect_true(all(file.exists(unlist(paths, use.names = FALSE))))
})

test_that("bundle helpers create download bundles and optional files", {
  skip_if_not_installed("ggplot2")

  tmp_dir <- tempfile("rmdreportdeck-bundle-")
  dir.create(tmp_dir, recursive = TRUE)
  plot_dir <- file.path(tmp_dir, "plots")
  data_dir <- file.path(tmp_dir, "data")

  df <- data.frame(group = c("A", "B"), value = c(2, 5))
  plot_obj <- ggplot2::ggplot(df, ggplot2::aes(group, value, fill = group)) + ggplot2::geom_col()

  bundle <- report_plot_bundle(
    plot_obj,
    block_prefix = "V01.01",
    suffix = "synthetic_plot",
    width = 6,
    height = 4,
    data = df,
    plot_dir = plot_dir,
    data_dir = data_dir,
    save_svg = TRUE
  )

  expect_s3_class(bundle, "reportdeck_plot_bundle")
  expect_true(file.exists(file.path(plot_dir, "FigV01.01.synthetic_plot.pdf")))
  expect_true(file.exists(file.path(plot_dir, "FigV01.01.synthetic_plot.svg")))
  expect_true(file.exists(file.path(data_dir, "TabV01.01.synthetic_plot.tsv")))
  expect_match(bundle$png_uri, "^data:image/png;base64,")
  expect_match(bundle$pdf_uri, "^data:application/pdf;base64,")
  expect_match(bundle$data_uri, "^data:text/tab-separated-values")

  links <- report_bundle_download_links(bundle, tsv_label = "TSV")
  expect_length(links, 3)

  asset_bar_html <- as.character(report_bundle_asset_bar("Downloads", bundle, tsv_label = "TSV"))
  expect_match(asset_bar_html, "FigV01.01.synthetic_plot.png")
  expect_match(asset_bar_html, "FigV01.01.synthetic_plot.pdf")
  expect_match(asset_bar_html, "TabV01.01.synthetic_plot.tsv")

  table_link_html <- as.character(report_tsv_download_link(df, "table.tsv", "Table TSV"))
  expect_match(table_link_html, "table.tsv")
  expect_match(table_link_html, "Table TSV")
})

test_that("bundle helpers derive figure and table names from one block prefix", {
  skip_if_not_installed("ggplot2")

  df <- data.frame(group = c("A", "B"), value = c(2, 5))
  plot_obj <- ggplot2::ggplot(df, ggplot2::aes(group, value, fill = group)) + ggplot2::geom_col()

  bundle <- report_plot_bundle(
    plot_obj,
    block_prefix = "V01.03",
    suffix = "nGenes_per_sample",
    data = df
  )

  expect_identical(bundle$stem, "FigV01.03.nGenes_per_sample")
  expect_identical(bundle$file_names$png, "FigV01.03.nGenes_per_sample.png")
  expect_identical(bundle$file_names$pdf, "FigV01.03.nGenes_per_sample.pdf")
  expect_identical(bundle$file_names$tsv, "TabV01.03.nGenes_per_sample.tsv")
})

test_that("asset naming helpers apply the standard block naming format", {
  expect_identical(
    report_asset_name("Fig", "V01.03"),
    "FigV01.03"
  )
  expect_identical(
    report_figure_name("V01.03", "nGenes_per_sample"),
    "FigV01.03.nGenes_per_sample"
  )
  expect_identical(
    report_table_name("V01.03", "nGenes_per_sample.tsv"),
    "TabV01.03.nGenes_per_sample.tsv"
  )
  expect_identical(
    report_asset_name("Aux", "V01.03", "qc", "raw"),
    "AuxV01.03.qc-raw"
  )
})

test_that("asset naming helpers reject invalid inputs", {
  expect_error(
    report_asset_name("", "V01.03"),
    "asset_type"
  )
  expect_error(
    report_asset_name("Fig", ""),
    "block_prefix"
  )
  expect_error(
    report_asset_name("Fig", "V01.03", ""),
    "suffix"
  )
  expect_error(
    report_asset_name("Fig", "V01.03", descriptor = ""),
    "descriptor"
  )
  expect_error(
    report_plot_bundle(NULL, block_prefix = ""),
    "block_prefix"
  )
})

test_that("HTML renderer creates interactive report and runinfo", {
  skip_if_render_stack_missing()

  tmp_dir <- tempfile("rmdreportdeck-html-")
  dir.create(tmp_dir, recursive = TRUE)
  rmd_file <- file.path(tmp_dir, "report.Rmd")
  make_mock_rmd(rmd_file)

  html_file <- render_html_report_with_runinfo(rmd_file, params = list(report_title = "Synthetic Title"))
  runinfo_file <- sub("\\.html$", ".runinfo", html_file)

  expect_true(file.exists(html_file))
  expect_true(file.exists(runinfo_file))

  html_text <- paste(readLines(html_file, warn = FALSE), collapse = "\n")
  runinfo_text <- paste(readLines(runinfo_file, warn = FALSE), collapse = "\n")

  expect_match(html_text, "report-sidebar")
  expect_match(html_text, "report-asset-bar")
  expect_match(runinfo_text, "Absolute path of report file:")
  expect_match(runinfo_text, "Output of timing:")
})

test_that("PDF renderer creates static PDF and runinfo", {
  skip_if_render_stack_missing()
  testthat::skip_if_not(nzchar(Sys.which("pdflatex")), "pdflatex not available")

  tmp_dir <- tempfile("rmdreportdeck-pdf-")
  dir.create(tmp_dir, recursive = TRUE)
  rmd_file <- file.path(tmp_dir, "report.Rmd")
  make_mock_pdf_rmd(rmd_file)

  pdf_file <- render_pdf_report_with_runinfo(rmd_file, params = list(report_title = "Synthetic PDF"))
  runinfo_file <- sub("\\.pdf$", ".runinfo", pdf_file)

  expect_true(file.exists(pdf_file))
  expect_true(file.exists(runinfo_file))

  runinfo_text <- paste(readLines(runinfo_file, warn = FALSE), collapse = "\n")
  expect_match(runinfo_text, "Absolute path of report file:")
  expect_match(runinfo_text, "rmdreportdeck::render_pdf_report")
})

test_that("runinfo follows an explicit relative output path", {
  skip_if_render_stack_missing()

  tmp_dir <- tempfile("rmdreportdeck-relative-output-")
  dir.create(tmp_dir, recursive = TRUE)
  rmd_file <- file.path(tmp_dir, "report.Rmd")
  out_dir <- file.path(tmp_dir, "nested")
  dir.create(out_dir)
  make_mock_rmd(rmd_file)

  html_file <- render_html_report_with_runinfo(
    rmd_file,
    output_file = "nested/custom-report.html",
    params = list(report_title = "Synthetic Title")
  )
  runinfo_file <- file.path(out_dir, "custom-report.runinfo")

  expect_identical(html_file, normalizePath(file.path(out_dir, "custom-report.html"), mustWork = FALSE))
  expect_true(file.exists(html_file))
  expect_true(file.exists(runinfo_file))
})

test_that("loop section renderer creates collapsible item panels", {
  skip_if_render_stack_missing()

  tmp_dir <- tempfile("rmdreportdeck-loop-html-")
  dir.create(tmp_dir, recursive = TRUE)
  rmd_file <- file.path(tmp_dir, "loop_report.Rmd")
  make_mock_loop_rmd(rmd_file, output_format = "html_document")

  html_file <- render_html_report_with_runinfo(rmd_file)
  html_text <- paste(readLines(html_file, warn = FALSE), collapse = "\n")

  expect_match(html_text, "report-loop-section")
  expect_match(html_text, "report-item-panel")
  expect_match(html_text, "Sample S1: alpha/beta")
  expect_match(html_text, "Sample S5: alpha/beta")
  expect_match(html_text, "sample-s1-alpha-beta")
  expect_match(html_text, "report-item-table-wrap")
  expect_match(html_text, "report-item-plot-card")
  expect_match(html_text, "TabV01.01.S1.tsv")
  expect_match(html_text, "FigV01.01.S1.pdf")
  expect_match(html_text, "data-open-first=\"true\"")
  expect_false(grepl("&lt;div class=&quot;report-asset-bar&quot;", html_text, fixed = TRUE))
  expect_false(grepl("items &lt;- vector", html_text, fixed = TRUE))
})

test_that("loop section renderer degrades safely in PDF", {
  skip_if_render_stack_missing()
  testthat::skip_if_not(nzchar(Sys.which("pdflatex")), "pdflatex not available")

  tmp_dir <- tempfile("rmdreportdeck-loop-pdf-")
  dir.create(tmp_dir, recursive = TRUE)
  rmd_file <- file.path(tmp_dir, "loop_report.Rmd")
  make_mock_loop_rmd(rmd_file, output_format = "pdf_document")

  pdf_file <- render_pdf_report_with_runinfo(rmd_file)
  runinfo_file <- sub("\\.pdf$", ".runinfo", pdf_file)

  expect_true(file.exists(pdf_file))
  expect_true(file.exists(runinfo_file))
})

test_that("CLI entrypoints accept key=value parameters", {
  skip_if_render_stack_missing()
  testthat::skip_if_not(nzchar(Sys.which("bash")), "bash not available")

  tmp_dir <- tempfile("rmdreportdeck-cli-")
  dir.create(tmp_dir, recursive = TRUE)
  html_rmd <- file.path(tmp_dir, "cli_html.Rmd")
  pdf_rmd <- file.path(tmp_dir, "cli_pdf.Rmd")
  make_mock_rmd(html_rmd)
  make_mock_pdf_rmd(pdf_rmd)

  html_cli <- system.file("cli", "knit2html", package = "rmdreportdeck")
  pdf_cli <- system.file("cli", "knit2pdf", package = "rmdreportdeck")

  expect_identical(
    system2(html_cli, args = c(html_rmd, "report_title=CLI_HTML"), env = c("REPORTDECK_COMMAND_PATH=cli-test")),
    0L
  )

  expect_true(file.exists(sub("\\.Rmd$", ".html", html_rmd)))
  expect_true(file.exists(sub("\\.Rmd$", ".runinfo", html_rmd)))

  if (nzchar(Sys.which("pdflatex"))) {
    expect_identical(
      system2(pdf_cli, args = c(pdf_rmd, "report_title=CLI_PDF"), env = c("REPORTDECK_COMMAND_PATH=cli-test")),
      0L
    )
    expect_true(file.exists(sub("\\.Rmd$", ".pdf", pdf_rmd)))
    expect_true(file.exists(sub("\\.Rmd$", ".runinfo", pdf_rmd)))
  }
})
