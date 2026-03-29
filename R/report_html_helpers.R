# Helpers for self-contained downloadable assets inside HTML reports.

report_tsv_text <- function(df) {
  con <- textConnection("out", "w", local = TRUE)
  on.exit(close(con), add = TRUE)
  utils::write.table(
    df,
    file = con,
    sep = "\t",
    quote = FALSE,
    row.names = FALSE,
    na = ""
  )
  paste(out, collapse = "\n")
}

report_text_data_uri <- function(text, mime = "text/tab-separated-values;charset=utf-8") {
  base64enc::dataURI(data = charToRaw(enc2utf8(text)), mime = mime)
}

report_plot_png_data_uri <- function(plot_obj, width = 8, height = 6, res = 144) {
  png_file <- tempfile(fileext = ".png")
  grDevices::png(
    filename = png_file,
    width = width,
    height = height,
    units = "in",
    res = res,
    bg = "white"
  )
  on.exit(unlink(png_file), add = TRUE)
  print(plot_obj)
  grDevices::dev.off()
  base64enc::dataURI(file = png_file, mime = "image/png")
}

report_plot_pdf_data_uri <- function(plot_obj, width = 8, height = 6) {
  pdf_file <- tempfile(fileext = ".pdf")
  grDevices::pdf(
    file = pdf_file,
    width = width,
    height = height,
    onefile = TRUE
  )
  on.exit(unlink(pdf_file), add = TRUE)
  print(plot_obj)
  grDevices::dev.off()
  base64enc::dataURI(file = pdf_file, mime = "application/pdf")
}

report_download_link <- function(label, filename, href) {
  htmltools::tags$a(
    label,
    href = href,
    download = filename,
    class = "btn btn-default btn-xs"
  )
}

report_asset_bar <- function(asset_label, ..., note = NULL) {
  controls <- list(...)
  children <- list(
    htmltools::tags$span(asset_label, class = "report-asset-label")
  )
  children <- c(children, controls)
  if (!is.null(note) && nzchar(note)) {
    children <- c(children, list(htmltools::tags$span(note, class = "report-asset-note")))
  }
  do.call(htmltools::tags$div, c(list(class = "report-asset-bar"), children))
}
