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

make_mock_loop_rmd_linked <- function(path) {
  writeLines(
    c(
      "---",
      "title: \"P00 | Synthetic Linked Loop Report\"",
      "output: html_document",
      "---",
      "",
      "```{r setup, include=FALSE}",
      "library(ggplot2)",
      "library(rmdreportdeck)",
      "samples <- data.frame(",
      "  sample = paste0(\"S\", 1:3),",
      "  value = c(2, 4, 3),",
      "  stringsAsFactors = FALSE",
      ")",
      "```",
      "",
      "# Description",
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
      "    width = 4, height = 3,",
      "    data = row",
      "  )",
      "  items[[i]] <- report_item_panel(",
      "    title = paste0(\"Sample \", row$sample),",
      "    asset_bar = report_bundle_asset_bar(\"Downloads\", bundle, tsv_label = \"TSV\"),",
      "    report_item_plot(plot_obj, width = 4, height = 3)",
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

test_that("default output follows the invoked symlink path", {
  skip_if_render_stack_missing()

  tmp_dir <- tempfile("rmdreportdeck-symlink-output-")
  dir.create(tmp_dir, recursive = TRUE)
  src_dir <- file.path(tmp_dir, "src")
  out_dir <- file.path(tmp_dir, "out")
  dir.create(src_dir)
  dir.create(out_dir)

  rmd_source <- file.path(src_dir, "report.Rmd")
  rmd_link <- file.path(out_dir, "report.Rmd")
  make_mock_rmd(rmd_source)
  file.symlink(rmd_source, rmd_link)

  html_file <- render_html_report_with_runinfo(rmd_link, params = list(report_title = "Synthetic Title"))
  runinfo_file <- file.path(out_dir, "report.runinfo")

  expect_identical(html_file, normalizePath(file.path(out_dir, "report.html"), mustWork = FALSE))
  expect_true(file.exists(html_file))
  expect_true(file.exists(runinfo_file))
  expect_false(file.exists(file.path(src_dir, "report.html")))
  expect_false(file.exists(file.path(src_dir, "report.runinfo")))
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

  expect_true(file.exists(sub("\\.Rmd$", ".linked.html", html_rmd)))
  expect_true(file.exists(sub("\\.Rmd$", ".linked.runinfo", html_rmd)))

  if (nzchar(Sys.which("pdflatex"))) {
    expect_identical(
      system2(pdf_cli, args = c(pdf_rmd, "report_title=CLI_PDF"), env = c("REPORTDECK_COMMAND_PATH=cli-test")),
      0L
    )
    expect_true(file.exists(sub("\\.Rmd$", ".pdf", pdf_rmd)))
    expect_true(file.exists(sub("\\.Rmd$", ".runinfo", pdf_rmd)))
  }
})

test_that("linked renderer creates .linked.html with file-backed assets and no data URIs", {
  skip_if_render_stack_missing()
  skip_if_not_installed("ggplot2")

  tmp_dir <- tempfile("rmdreportdeck-linked-")
  dir.create(tmp_dir, recursive = TRUE)
  rmd_file <- file.path(tmp_dir, "report.Rmd")
  make_mock_rmd(rmd_file)

  html_file <- render_html_report_linked_with_runinfo(
    rmd_file,
    params = list(report_title = "Linked Title")
  )
  runinfo_file <- sub("\\.linked\\.html$", ".linked.runinfo", html_file)
  knit_md_file <- file.path(tmp_dir, "report.knit.md")
  knit_meta_file <- file.path(tmp_dir, "report.knit.meta")
  asset_dir <- file.path(tmp_dir, "report_files", "reportdeck")

  expect_identical(basename(html_file), "report.linked.html")
  expect_true(file.exists(html_file))
  expect_true(file.exists(runinfo_file))
  expect_true(file.exists(knit_md_file))
  expect_true(file.exists(knit_meta_file))
  expect_true(dir.exists(asset_dir))

  html_text <- paste(readLines(html_file, warn = FALSE), collapse = "\n")
  expect_false(grepl("data:image/png;base64,", html_text, fixed = TRUE))
  expect_match(html_text, "report_files/reportdeck/")

  meta_text <- paste(readLines(knit_meta_file, warn = FALSE), collapse = "\n")
  expect_match(meta_text, "rmd_mtime=")
  expect_match(meta_text, "param\\.report_title=Linked Title")

  runinfo_text <- paste(readLines(runinfo_file, warn = FALSE), collapse = "\n")
  expect_match(runinfo_text, "rmdreportdeck::render_html_report_linked")
  expect_match(runinfo_text, "Output of timing:")
})

test_that("packhtml reuses .knit.md when fresh and params match", {
  skip_if_render_stack_missing()
  skip_if_not_installed("ggplot2")

  tmp_dir <- tempfile("rmdreportdeck-packhtml-reuse-")
  dir.create(tmp_dir, recursive = TRUE)
  rmd_file <- file.path(tmp_dir, "report.Rmd")
  make_mock_rmd(rmd_file)

  render_html_report_linked_with_runinfo(rmd_file, params = list(report_title = "Pack Test"))
  knit_md_file <- file.path(tmp_dir, "report.knit.md")
  linked_mtime <- file.info(knit_md_file)$mtime

  Sys.sleep(0.2)

  portable_file <- render_html_report_portable_with_runinfo(
    rmd_file,
    params = list(report_title = "Pack Test")
  )
  runinfo_file <- sub("\\.portable\\.html$", ".portable.runinfo", portable_file)

  expect_identical(basename(portable_file), "report.portable.html")
  expect_true(file.exists(portable_file))
  expect_true(file.exists(runinfo_file))

  new_mtime <- file.info(knit_md_file)$mtime
  expect_equal(linked_mtime, new_mtime)

  html_text <- paste(readLines(portable_file, warn = FALSE), collapse = "\n")
  expect_match(html_text, "data:image/png;base64,")

  runinfo_text <- paste(readLines(runinfo_file, warn = FALSE), collapse = "\n")
  expect_match(runinfo_text, "Knit reused: yes")
  expect_match(runinfo_text, "Pack timing:")
  expect_false(grepl("Knit timing:", runinfo_text, fixed = TRUE))
})

test_that("packhtml re-knits when params differ", {
  skip_if_render_stack_missing()
  skip_if_not_installed("ggplot2")

  tmp_dir <- tempfile("rmdreportdeck-packhtml-stale-")
  dir.create(tmp_dir, recursive = TRUE)
  rmd_file <- file.path(tmp_dir, "report.Rmd")
  make_mock_rmd(rmd_file)

  render_html_report_linked_with_runinfo(rmd_file, params = list(report_title = "Old Title"))
  knit_md_file <- file.path(tmp_dir, "report.knit.md")
  linked_mtime <- file.info(knit_md_file)$mtime

  Sys.sleep(0.2)

  portable_file <- render_html_report_portable_with_runinfo(
    rmd_file,
    params = list(report_title = "New Title")
  )
  runinfo_file <- sub("\\.portable\\.html$", ".portable.runinfo", portable_file)

  new_mtime <- file.info(knit_md_file)$mtime
  expect_true(new_mtime > linked_mtime)

  runinfo_text <- paste(readLines(runinfo_file, warn = FALSE), collapse = "\n")
  expect_match(runinfo_text, "Knit reused: no")
  expect_match(runinfo_text, "Knit timing:")
  expect_match(runinfo_text, "Pack timing:")
})

test_that("packhtml re-knits when .knit.md is missing", {
  skip_if_render_stack_missing()
  skip_if_not_installed("ggplot2")

  tmp_dir <- tempfile("rmdreportdeck-packhtml-missing-")
  dir.create(tmp_dir, recursive = TRUE)
  rmd_file <- file.path(tmp_dir, "report.Rmd")
  make_mock_rmd(rmd_file)

  portable_file <- render_html_report_portable_with_runinfo(
    rmd_file,
    params = list(report_title = "Fresh Pack")
  )
  runinfo_file <- sub("\\.portable\\.html$", ".portable.runinfo", portable_file)

  expect_true(file.exists(portable_file))
  runinfo_text <- paste(readLines(runinfo_file, warn = FALSE), collapse = "\n")
  expect_match(runinfo_text, "Knit reused: no")
  expect_true(file.exists(file.path(tmp_dir, "report.linked.html")))
  expect_true(file.exists(portable_file))
})

# --- Staleness edge cases ---

test_that("packhtml re-knits when .knit.meta is absent but .knit.md exists", {
  skip_if_render_stack_missing()
  skip_if_not_installed("ggplot2")

  tmp_dir <- tempfile("rmdreportdeck-packhtml-nometa-")
  dir.create(tmp_dir, recursive = TRUE)
  rmd_file <- file.path(tmp_dir, "report.Rmd")
  make_mock_rmd(rmd_file)

  render_html_report_linked_with_runinfo(rmd_file, params = list(report_title = "No Meta"))
  knit_meta_file <- file.path(tmp_dir, "report.knit.meta")
  knit_md_file   <- file.path(tmp_dir, "report.knit.md")
  unlink(knit_meta_file)
  expect_false(file.exists(knit_meta_file))
  expect_true(file.exists(knit_md_file))

  original_mtime <- file.info(knit_md_file)$mtime
  Sys.sleep(0.2)
  render_html_report_portable_with_runinfo(rmd_file, params = list(report_title = "No Meta"))

  runinfo_text <- paste(readLines(
    file.path(tmp_dir, "report.portable.runinfo"), warn = FALSE), collapse = "\n")
  expect_match(runinfo_text, "Knit reused: no")
  expect_true(file.info(knit_md_file)$mtime > original_mtime)
})

test_that("packhtml re-knits when Rmd is newer than stored mtime", {
  skip_if_render_stack_missing()
  skip_if_not_installed("ggplot2")

  tmp_dir <- tempfile("rmdreportdeck-packhtml-newer-rmd-")
  dir.create(tmp_dir, recursive = TRUE)
  rmd_file <- file.path(tmp_dir, "report.Rmd")
  make_mock_rmd(rmd_file)

  render_html_report_linked_with_runinfo(rmd_file, params = list(report_title = "Older"))
  knit_md_file <- file.path(tmp_dir, "report.knit.md")
  original_mtime <- file.info(knit_md_file)$mtime

  Sys.sleep(1.1)
  writeLines(c(readLines(rmd_file, warn = FALSE), ""), con = rmd_file)

  Sys.sleep(0.1)
  render_html_report_portable_with_runinfo(rmd_file, params = list(report_title = "Older"))

  runinfo_text <- paste(readLines(
    file.path(tmp_dir, "report.portable.runinfo"), warn = FALSE), collapse = "\n")
  expect_match(runinfo_text, "Knit reused: no")
  expect_true(file.info(knit_md_file)$mtime > original_mtime)
})

# --- Unit tests for helpers in linked mode ---

test_that("report_plot_bundle in linked mode writes files to asset_dir and returns file hrefs", {
  skip_if_not_installed("ggplot2")

  tmp_dir <- tempfile("rmdreportdeck-bundle-linked-")
  dir.create(tmp_dir, recursive = TRUE)
  asset_dir <- file.path(tmp_dir, "report_files", "reportdeck")

  options(rmdreportdeck.render_mode = "linked")
  options(rmdreportdeck.asset_dir = asset_dir)
  on.exit({
    options(rmdreportdeck.render_mode = NULL)
    options(rmdreportdeck.asset_dir = NULL)
  }, add = TRUE)

  df <- data.frame(group = c("A", "B"), value = c(2, 5))
  plot_obj <- ggplot2::ggplot(df, ggplot2::aes(group, value, fill = group)) + ggplot2::geom_col()

  bundle <- report_plot_bundle(
    plot_obj,
    block_prefix = "V01.01",
    suffix = "linked_plot",
    width = 4, height = 3,
    data = df
  )

  expect_true(file.exists(file.path(asset_dir, "FigV01.01.linked_plot.png")))
  expect_true(file.exists(file.path(asset_dir, "FigV01.01.linked_plot.pdf")))
  expect_true(file.exists(file.path(asset_dir, "TabV01.01.linked_plot.tsv")))

  expect_null(bundle$png_uri)
  expect_null(bundle$pdf_uri)
  expect_null(bundle$data_uri)

  expect_match(bundle$png_href,  "report_files/reportdeck/FigV01.01.linked_plot.png",  fixed = TRUE)
  expect_match(bundle$pdf_href,  "report_files/reportdeck/FigV01.01.linked_plot.pdf",  fixed = TRUE)
  expect_match(bundle$data_href, "report_files/reportdeck/TabV01.01.linked_plot.tsv",  fixed = TRUE)

  links <- report_bundle_download_links(bundle, tsv_label = "TSV")
  expect_length(links, 3)
  links_html <- as.character(htmltools::tagList(links))
  expect_false(grepl("data:image/png;base64,", links_html, fixed = TRUE))
  expect_match(links_html, "report_files/reportdeck/FigV01.01.linked_plot.png", fixed = TRUE)
})

test_that("report_plot_bundle in linked mode with explicit dirs also writes archival copies", {
  skip_if_not_installed("ggplot2")

  tmp_dir  <- tempfile("rmdreportdeck-bundle-linked-extradirs-")
  dir.create(tmp_dir, recursive = TRUE)
  asset_dir  <- file.path(tmp_dir, "report_files", "reportdeck")
  plot_dir   <- file.path(tmp_dir, "plots")
  data_dir   <- file.path(tmp_dir, "data")

  options(rmdreportdeck.render_mode = "linked")
  options(rmdreportdeck.asset_dir = asset_dir)
  on.exit({
    options(rmdreportdeck.render_mode = NULL)
    options(rmdreportdeck.asset_dir = NULL)
  }, add = TRUE)

  df <- data.frame(x = 1:3)
  plot_obj <- ggplot2::ggplot(df, ggplot2::aes(x)) + ggplot2::geom_histogram(bins = 3)

  bundle <- report_plot_bundle(
    plot_obj,
    block_prefix = "V02.01",
    width = 4, height = 3,
    data = df,
    plot_dir = plot_dir,
    data_dir = data_dir
  )

  # Primary copies in asset_dir
  expect_true(file.exists(file.path(asset_dir, "FigV02.01.png")))
  expect_true(file.exists(file.path(asset_dir, "TabV02.01.tsv")))

  # Archival copies in explicit dirs
  expect_true(file.exists(file.path(plot_dir, "FigV02.01.png")))
  expect_true(file.exists(file.path(data_dir,  "TabV02.01.tsv")))

  # hrefs always point to asset_dir
  expect_match(bundle$png_href, "report_files/reportdeck/FigV02.01.png", fixed = TRUE)
})

test_that("report_tsv_download_link in linked mode writes file and returns path href", {
  tmp_dir   <- tempfile("rmdreportdeck-tsv-linked-")
  dir.create(tmp_dir, recursive = TRUE)
  asset_dir <- file.path(tmp_dir, "report_files", "reportdeck")

  options(rmdreportdeck.render_mode = "linked")
  options(rmdreportdeck.asset_dir = asset_dir)
  on.exit({
    options(rmdreportdeck.render_mode = NULL)
    options(rmdreportdeck.asset_dir = NULL)
  }, add = TRUE)

  df   <- data.frame(a = 1:3, b = letters[1:3])
  link <- report_tsv_download_link(df, "my_table.tsv", "Download TSV")

  expect_true(file.exists(file.path(asset_dir, "my_table.tsv")))
  link_html <- as.character(link)
  expect_false(grepl("data:text/", link_html, fixed = TRUE))
  expect_match(link_html, "report_files/reportdeck/my_table.tsv", fixed = TRUE)
  expect_match(link_html, "Download TSV", fixed = TRUE)
})

test_that("render_reportdeck_item_content_html in linked mode writes PNG and returns img path", {
  skip_if_not_installed("ggplot2")

  tmp_dir   <- tempfile("rmdreportdeck-itemplot-linked-")
  dir.create(tmp_dir, recursive = TRUE)
  asset_dir <- file.path(tmp_dir, "report_files", "reportdeck")

  options(rmdreportdeck.render_mode = "linked")
  options(rmdreportdeck.asset_dir = asset_dir)
  on.exit({
    options(rmdreportdeck.render_mode = NULL)
    options(rmdreportdeck.asset_dir = NULL)
  }, add = TRUE)

  df       <- data.frame(x = 1:2, y = c(3, 5))
  plot_obj <- ggplot2::ggplot(df, ggplot2::aes(x, y)) + ggplot2::geom_line()
  item     <- report_item_plot(plot_obj, width = 4, height = 3)

  result <- rmdreportdeck:::render_reportdeck_item_content_html(item, prefix = "panel1-1")

  png_files <- list.files(asset_dir, pattern = "\\.png$")
  expect_length(png_files, 1L)
  img_html <- as.character(result)
  expect_false(grepl("data:image/png;base64,", img_html, fixed = TRUE))
  expect_match(img_html, "report_files/reportdeck/", fixed = TRUE)
  expect_match(img_html, ".png", fixed = TRUE)
})

# --- Integration: linked renderer with symlink and loop ---

test_that("linked renderer output follows invoked symlink path", {
  skip_if_render_stack_missing()
  skip_if_not_installed("ggplot2")

  tmp_dir  <- tempfile("rmdreportdeck-linked-symlink-")
  dir.create(tmp_dir, recursive = TRUE)
  src_dir  <- file.path(tmp_dir, "src")
  out_dir  <- file.path(tmp_dir, "out")
  dir.create(src_dir)
  dir.create(out_dir)

  rmd_source <- file.path(src_dir, "report.Rmd")
  rmd_link   <- file.path(out_dir, "report.Rmd")
  make_mock_rmd(rmd_source)
  file.symlink(rmd_source, rmd_link)

  html_file <- render_html_report_linked_with_runinfo(rmd_link, params = list(report_title = "Symlink"))

  expect_identical(html_file, normalizePath(file.path(out_dir, "report.linked.html"), mustWork = FALSE))
  expect_true(file.exists(html_file))
  expect_true(file.exists(file.path(out_dir, "report.linked.runinfo")))
  expect_true(file.exists(file.path(out_dir, "report.knit.md")))
  expect_false(file.exists(file.path(src_dir, "report.linked.html")))
})

test_that("linked renderer with loop section writes panel assets to asset_dir", {
  skip_if_render_stack_missing()
  skip_if_not_installed("ggplot2")

  tmp_dir  <- tempfile("rmdreportdeck-linked-loop-")
  dir.create(tmp_dir, recursive = TRUE)
  rmd_file <- file.path(tmp_dir, "loop.Rmd")
  make_mock_loop_rmd_linked(rmd_file)

  html_file  <- render_html_report_linked_with_runinfo(rmd_file)
  asset_dir  <- file.path(tmp_dir, "loop_files", "reportdeck")

  expect_true(file.exists(html_file))
  expect_true(dir.exists(asset_dir))

  png_files <- list.files(asset_dir, pattern = "\\.png$")
  expect_gte(length(png_files), 3L)

  html_text <- paste(readLines(html_file, warn = FALSE), collapse = "\n")
  expect_false(grepl("data:image/png;base64,", html_text, fixed = TRUE))
  expect_match(html_text, "loop_files/reportdeck/")
  expect_match(html_text, "report-loop-section")
  expect_match(html_text, "Sample S1")
})

# --- CLI tests for packhtml and knit2html ---

test_that("packhtml CLI produces .portable.html", {
  skip_if_render_stack_missing()
  skip_if_not_installed("ggplot2")
  testthat::skip_if_not(nzchar(Sys.which("bash")), "bash not available")

  tmp_dir  <- tempfile("rmdreportdeck-packhtml-cli-")
  dir.create(tmp_dir, recursive = TRUE)
  rmd_file <- file.path(tmp_dir, "report.Rmd")
  make_mock_rmd(rmd_file)

  pack_cli <- system.file("cli", "packhtml", package = "rmdreportdeck")
  expect_identical(
    system2(pack_cli, args = c(rmd_file, "report_title=CLI_Pack"), env = c("REPORTDECK_COMMAND_PATH=cli-test")),
    0L
  )
  expect_true(file.exists(file.path(tmp_dir, "report.portable.html")))
  expect_true(file.exists(file.path(tmp_dir, "report.portable.runinfo")))
  expect_true(file.exists(file.path(tmp_dir, "report.linked.html")))
})

# --- Unit test for reportdeck_setup_linked ---

test_that("reportdeck_setup_linked sets cache options based on output.file", {
  testthat::skip_if_not_installed("knitr")

  tmp_dir  <- tempfile("rmdreportdeck-setup-linked-")
  dir.create(tmp_dir, recursive = TRUE)
  fake_output <- file.path(tmp_dir, "report.linked.html")

  old_output <- knitr::opts_knit$get("output.file")
  old_cache  <- knitr::opts_chunk$get("cache")
  on.exit({
    knitr::opts_knit$set(output.file = old_output)
    knitr::opts_chunk$set(cache = old_cache)
  }, add = TRUE)

  knitr::opts_knit$set(output.file = fake_output)
  cache_path <- reportdeck_setup_linked()

  expect_match(cache_path, ".knitr-cache/report/", fixed = TRUE)
  expect_match(cache_path, tmp_dir, fixed = TRUE)
  expect_true(isTRUE(knitr::opts_chunk$get("cache")))
  expect_identical(knitr::opts_chunk$get("cache.path"), cache_path)
})
