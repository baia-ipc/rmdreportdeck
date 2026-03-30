# Helpers for self-contained downloadable assets and loop-aware panels inside reports.

reportdeck_draw_plot <- function(plot_obj) {
  if (inherits(plot_obj, "grob")) {
    grid::grid.draw(plot_obj)
  } else {
    print(plot_obj)
  }
}

reportdeck_write_plot_png <- function(plot_obj, path, width = 8, height = 6, res = 144) {
  grDevices::png(
    filename = path,
    width = width,
    height = height,
    units = "in",
    res = res,
    bg = "white"
  )
  on.exit(grDevices::dev.off(), add = TRUE)
  reportdeck_draw_plot(plot_obj)
  invisible(path)
}

reportdeck_write_plot_pdf <- function(plot_obj, path, width = 8, height = 6) {
  grDevices::pdf(
    file = path,
    width = width,
    height = height,
    onefile = TRUE
  )
  on.exit(grDevices::dev.off(), add = TRUE)
  reportdeck_draw_plot(plot_obj)
  invisible(path)
}

reportdeck_write_plot_svg <- function(plot_obj, path, width = 8, height = 6) {
  grDevices::svg(
    filename = path,
    width = width,
    height = height
  )
  on.exit(grDevices::dev.off(), add = TRUE)
  reportdeck_draw_plot(plot_obj)
  invisible(path)
}

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
  on.exit(unlink(png_file), add = TRUE)
  reportdeck_write_plot_png(plot_obj, png_file, width = width, height = height, res = res)
  base64enc::dataURI(file = png_file, mime = "image/png")
}

report_plot_pdf_data_uri <- function(plot_obj, width = 8, height = 6) {
  pdf_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(pdf_file), add = TRUE)
  reportdeck_write_plot_pdf(plot_obj, pdf_file, width = width, height = height)
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

report_tsv_download_link <- function(df, filename, label = "Download TSV") {
  report_download_link(
    label = label,
    filename = filename,
    href = report_text_data_uri(report_tsv_text(df))
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

report_plot_bundle <- function(
  plot_obj,
  stem,
  width = 8,
  height = 6,
  data = NULL,
  plot_dir = NULL,
  data_dir = NULL,
  res = 144,
  save_pdf = TRUE,
  save_svg = FALSE
) {
  if (!is.character(stem) || length(stem) != 1L || !nzchar(stem)) {
    stop("'stem' must be a single non-empty character string.", call. = FALSE)
  }

  if (!is.null(plot_dir)) {
    dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
  }
  if (!is.null(data_dir) && !is.null(data)) {
    dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  }

  file_names <- list(
    png = paste0(stem, ".png"),
    pdf = if (isTRUE(save_pdf)) paste0(stem, ".pdf") else NULL,
    svg = if (isTRUE(save_svg)) paste0(stem, ".svg") else NULL,
    tsv = if (!is.null(data)) paste0(stem, ".tsv") else NULL
  )

  if (!is.null(plot_dir) && isTRUE(save_pdf)) {
    reportdeck_write_plot_pdf(
      plot_obj,
      file.path(plot_dir, file_names$pdf),
      width = width,
      height = height
    )
  }
  if (!is.null(plot_dir) && isTRUE(save_svg)) {
    reportdeck_write_plot_svg(
      plot_obj,
      file.path(plot_dir, file_names$svg),
      width = width,
      height = height
    )
  }
  if (!is.null(data) && !is.null(data_dir)) {
    utils::write.table(
      data,
      file.path(data_dir, file_names$tsv),
      sep = "\t",
      quote = FALSE,
      row.names = FALSE,
      na = ""
    )
  }

  structure(
    list(
      stem = stem,
      plot_obj = plot_obj,
      data = data,
      width = width,
      height = height,
      res = res,
      file_names = file_names,
      png_uri = report_plot_png_data_uri(plot_obj, width = width, height = height, res = res),
      pdf_uri = if (isTRUE(save_pdf)) report_plot_pdf_data_uri(plot_obj, width = width, height = height) else NULL,
      data_uri = if (!is.null(data)) report_text_data_uri(report_tsv_text(data)) else NULL
    ),
    class = "reportdeck_plot_bundle"
  )
}

report_bundle_download_links <- function(
  bundle,
  png_label = "Download PNG",
  pdf_label = "Download PDF",
  tsv_label = NULL
) {
  if (!inherits(bundle, "reportdeck_plot_bundle")) {
    stop("'bundle' must be created by report_plot_bundle().", call. = FALSE)
  }

  links <- list(
    report_download_link(
      png_label,
      bundle$file_names$png,
      bundle$png_uri
    )
  )

  if (!is.null(bundle$pdf_uri) && !is.null(bundle$file_names$pdf)) {
    links <- c(
      links,
      list(report_download_link(pdf_label, bundle$file_names$pdf, bundle$pdf_uri))
    )
  }

  if (!is.null(tsv_label) && !is.null(bundle$data_uri) && !is.null(bundle$file_names$tsv)) {
    links <- c(
      links,
      list(report_download_link(tsv_label, bundle$file_names$tsv, bundle$data_uri))
    )
  }

  links
}

report_bundle_asset_bar <- function(
  asset_label,
  bundle,
  png_label = "Download PNG",
  pdf_label = "Download PDF",
  tsv_label = NULL,
  note = NULL
) {
  if (!inherits(bundle, "reportdeck_plot_bundle")) {
    stop("'bundle' must be created by report_plot_bundle().", call. = FALSE)
  }

  do.call(
    report_asset_bar,
    c(
      list(asset_label),
      report_bundle_download_links(
        bundle,
        png_label = png_label,
        pdf_label = pdf_label,
        tsv_label = tsv_label
      ),
      list(note = note)
    )
  )
}

reportdeck_require_knitr <- function() {
  if (!requireNamespace("knitr", quietly = TRUE)) {
    stop("The 'knitr' package is required for this helper.", call. = FALSE)
  }
}

reportdeck_slugify <- function(text) {
  if (is.null(text) || length(text) == 0 || all(is.na(text))) {
    return("item")
  }

  value <- tolower(enc2utf8(paste(text, collapse = "-")))
  value <- gsub("[^a-z0-9]+", "-", value, perl = TRUE)
  value <- gsub("(^-+|-+$)", "", value, perl = TRUE)
  value <- gsub("-{2,}", "-", value, perl = TRUE)
  if (!nzchar(value)) {
    value <- "item"
  }
  value
}

reportdeck_unique_ids <- function(ids) {
  seen <- new.env(parent = emptyenv())
  unique_ids <- character(length(ids))

  for (i in seq_along(ids)) {
    id <- ids[[i]]
    if (!exists(id, envir = seen, inherits = FALSE)) {
      assign(id, 1L, envir = seen)
      unique_ids[[i]] <- id
    } else {
      count <- get(id, envir = seen, inherits = FALSE) + 1L
      assign(id, count, envir = seen)
      unique_ids[[i]] <- paste0(id, "-", count)
    }
  }

  unique_ids
}

render_reportdeck_item_content_html <- function(content) {
  if (inherits(content, "reportdeck_item_plot")) {
    png_uri <- report_plot_png_data_uri(
      content$plot_obj,
      width = content$width,
      height = content$height,
      res = content$res
    )
    return(htmltools::tags$div(
      class = "report-item-plot-card report-plot-card",
      do.call(
        htmltools::tags$img,
        c(
          list(
            src = png_uri,
            alt = "",
            class = "report-item-plot-image"
          ),
          content$img_attrs
        )
      )
    ))
  }

  if (inherits(content, "reportdeck_item_table")) {
    reportdeck_require_knitr()
    return(htmltools::tags$div(
      class = "report-inline-table-wrap report-item-table-wrap",
      htmltools::HTML(do.call(knitr::kable, c(list(content$df, format = "html"), content$kable_args)))
    ))
  }

  if (inherits(content, c("shiny.tag", "shiny.tag.list", "html"))) {
    return(content)
  }

  if (is.character(content)) {
    text <- paste(content, collapse = "\n")
    return(htmltools::tags$p(text))
  }

  htmltools::tags$pre(paste(capture.output(str(content)), collapse = "\n"))
}

render_reportdeck_item_content_markdown <- function(content) {
  if (inherits(content, "reportdeck_item_plot")) {
    png_file <- tempfile(fileext = ".png")
    grDevices::png(
      filename = png_file,
      width = content$width,
      height = content$height,
      units = "in",
      res = content$res,
      bg = "white"
    )
    reportdeck_draw_plot(content$plot_obj)
    grDevices::dev.off()
    return(paste0("![](", normalizePath(png_file, winslash = "/", mustWork = FALSE), ")"))
  }

  if (inherits(content, "reportdeck_item_table")) {
    reportdeck_require_knitr()
    return(paste(do.call(knitr::kable, c(list(content$df, format = "pipe"), content$kable_args)), collapse = "\n"))
  }

  if (is.character(content)) {
    return(paste(content, collapse = "\n"))
  }

  ""
}

`%||%` <- function(lhs, rhs) {
  if (is.null(lhs)) rhs else lhs
}

reportdeck_compact_html <- function(tag) {
  rendered <- htmltools::renderTags(tag)$html
  rendered <- gsub(">\\s+<", "><", rendered, perl = TRUE)
  trimws(rendered)
}

report_item_plot <- function(plot_obj, width = 8, height = 6, res = 144, ...) {
  structure(
    list(
      plot_obj = plot_obj,
      width = width,
      height = height,
      res = res,
      img_attrs = list(...)
    ),
    class = "reportdeck_item_plot"
  )
}

report_item_table <- function(df, ...) {
  structure(
    list(
      df = df,
      kable_args = list(...)
    ),
    class = "reportdeck_item_table"
  )
}

report_item_panel <- function(title, ..., id = NULL, open = FALSE, asset_bar = NULL, note = NULL) {
  structure(
    list(
      title = title,
      body = list(...),
      id = id,
      open = isTRUE(open),
      asset_bar = asset_bar,
      note = note
    ),
    class = "reportdeck_item_panel"
  )
}

render_reportdeck_item_panel_html <- function(panel, panel_id, open = FALSE) {
  body_children <- list()
  if (!is.null(panel$asset_bar)) {
    body_children <- c(body_children, list(panel$asset_bar))
  }
  if (!is.null(panel$note) && nzchar(panel$note)) {
    body_children <- c(body_children, list(htmltools::tags$p(panel$note, class = "report-item-note")))
  }
  if (length(panel$body) > 0) {
    rendered <- lapply(panel$body, render_reportdeck_item_content_html)
    body_children <- c(body_children, rendered)
  }

  htmltools::tags$details(
    class = "report-item-panel",
    id = panel_id,
    open = if (isTRUE(open)) NA else NULL,
    htmltools::tags$summary(
      htmltools::tags$span(panel$title, class = "report-item-panel-title")
    ),
    htmltools::tags$div(
      class = "report-item-panel-body",
      htmltools::tagList(body_children)
    )
  )
}

render_reportdeck_item_panel_markdown <- function(panel) {
  parts <- c(paste0("### ", panel$title), "")
  if (!is.null(panel$note) && nzchar(panel$note)) {
    parts <- c(parts, panel$note, "")
  }

  if (length(panel$body) > 0) {
    body_parts <- vapply(panel$body, render_reportdeck_item_content_markdown, character(1))
    body_parts <- body_parts[nzchar(body_parts)]
    if (length(body_parts) > 0) {
      parts <- c(parts, body_parts, "")
    }
  }

  paste(parts, collapse = "\n")
}

report_loop_section <- function(items, open_first = TRUE, max_open = 1L) {
  reportdeck_require_knitr()

  if (!is.list(items)) {
    stop("'items' must be a list of report_item_panel() outputs.", call. = FALSE)
  }
  if (length(items) == 0) {
    return(knitr::asis_output(""))
  }
  if (!all(vapply(items, inherits, logical(1), "reportdeck_item_panel"))) {
    stop("All 'items' must be produced by report_item_panel().", call. = FALSE)
  }

  base_ids <- vapply(items, function(item) {
    reportdeck_slugify(item$id %||% item$title)
  }, character(1))
  panel_ids <- reportdeck_unique_ids(base_ids)

  open_vec <- vapply(items, function(item) isTRUE(item$open), logical(1))
  if (!any(open_vec) && isTRUE(open_first) && length(open_vec) > 0) {
    open_vec[[1]] <- TRUE
  }
  if (is.finite(max_open) && max_open >= 1L && sum(open_vec) > max_open) {
    keep <- which(open_vec)[seq_len(max_open)]
    open_vec[] <- FALSE
    open_vec[keep] <- TRUE
  }

  if (knitr::is_html_output()) {
    rendered <- Map(
      function(item, panel_id, open_flag) {
        render_reportdeck_item_panel_html(item, panel_id = panel_id, open = open_flag)
      },
      items,
      panel_ids,
      open_vec
    )

    html <- htmltools::tags$div(
      class = "report-loop-section",
      `data-open-first` = tolower(as.character(isTRUE(open_first))),
      `data-max-open` = as.character(max_open),
      htmltools::tagList(rendered)
    )
    return(knitr::asis_output(reportdeck_compact_html(html)))
  }

  markdown <- paste(
    vapply(items, render_reportdeck_item_panel_markdown, character(1)),
    collapse = "\n\n"
  )
  knitr::asis_output(markdown)
}
