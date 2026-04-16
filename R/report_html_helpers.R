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

reportdeck_download_asset <- function(kind, ...) {
  structure(
    c(list(kind = kind), list(...)),
    class = "reportdeck_download_asset"
  )
}

reportdeck_asset_dir <- function() {
  if (!reportdeck_is_linked_mode()) {
    return(NULL)
  }
  getOption("rmdreportdeck.asset_dir")
}

reportdeck_asset_href <- function(filename) {
  asset_dir <- reportdeck_asset_dir()
  if (is.null(asset_dir)) {
    return(NULL)
  }
  file.path(basename(dirname(asset_dir)), basename(asset_dir), filename)
}

reportdeck_write_text_file <- function(text, path) {
  writeLines(enc2utf8(text), con = path, useBytes = TRUE)
  invisible(path)
}

reportdeck_asset_data_uri <- function(asset) {
  if (!inherits(asset, "reportdeck_download_asset")) {
    return(asset)
  }

  switch(
    asset$kind,
    text = base64enc::dataURI(data = charToRaw(enc2utf8(asset$text)), mime = asset$mime),
    plot_png = {
      png_file <- tempfile(fileext = ".png")
      on.exit(unlink(png_file), add = TRUE)
      reportdeck_write_plot_png(
        asset$plot_obj,
        png_file,
        width = asset$width,
        height = asset$height,
        res = asset$res
      )
      base64enc::dataURI(file = png_file, mime = asset$mime)
    },
    plot_pdf = {
      pdf_file <- tempfile(fileext = ".pdf")
      on.exit(unlink(pdf_file), add = TRUE)
      reportdeck_write_plot_pdf(
        asset$plot_obj,
        pdf_file,
        width = asset$width,
        height = asset$height
      )
      base64enc::dataURI(file = pdf_file, mime = asset$mime)
    },
    stop("Unsupported download asset kind: ", asset$kind, call. = FALSE)
  )
}

reportdeck_materialize_download_asset <- function(asset, filename) {
  if (!inherits(asset, "reportdeck_download_asset")) {
    return(asset)
  }

  asset_dir <- reportdeck_asset_dir()
  if (is.null(asset_dir)) {
    return(reportdeck_asset_data_uri(asset))
  }

  dir.create(asset_dir, recursive = TRUE, showWarnings = FALSE)
  asset_path <- file.path(asset_dir, filename)

  switch(
    asset$kind,
    text = reportdeck_write_text_file(asset$text, asset_path),
    plot_png = reportdeck_write_plot_png(
      asset$plot_obj,
      asset_path,
      width = asset$width,
      height = asset$height,
      res = asset$res
    ),
    plot_pdf = reportdeck_write_plot_pdf(
      asset$plot_obj,
      asset_path,
      width = asset$width,
      height = asset$height
    ),
    stop("Unsupported download asset kind: ", asset$kind, call. = FALSE)
  )

  reportdeck_asset_href(filename)
}

as.character.reportdeck_download_asset <- function(x, ...) {
  reportdeck_asset_data_uri(x)
}

report_text_data_uri <- function(text, mime = "text/tab-separated-values;charset=utf-8") {
  reportdeck_download_asset("text", text = text, mime = mime)
}

report_plot_png_data_uri <- function(plot_obj, width = 8, height = 6, res = 144) {
  reportdeck_download_asset(
    "plot_png",
    plot_obj = plot_obj,
    width = width,
    height = height,
    res = res,
    mime = "image/png"
  )
}

report_plot_pdf_data_uri <- function(plot_obj, width = 8, height = 6) {
  reportdeck_download_asset(
    "plot_pdf",
    plot_obj = plot_obj,
    width = width,
    height = height,
    mime = "application/pdf"
  )
}

report_download_link <- function(label, filename, href) {
  href <- reportdeck_materialize_download_asset(href, filename)
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

report_asset_name <- function(asset_type, block_prefix, suffix = NULL, descriptor = NULL) {
  if (!is.character(asset_type) || length(asset_type) != 1L || !nzchar(asset_type)) {
    stop("'asset_type' must be a single non-empty character string.", call. = FALSE)
  }
  if (!is.character(block_prefix) || length(block_prefix) != 1L || !nzchar(block_prefix)) {
    stop("'block_prefix' must be a single non-empty character string.", call. = FALSE)
  }
  if (!is.null(suffix) && (!is.character(suffix) || length(suffix) != 1L || !nzchar(suffix))) {
    stop("'suffix' must be NULL or a single non-empty character string.", call. = FALSE)
  }
  if (!is.null(descriptor) && (!is.character(descriptor) || length(descriptor) != 1L || !nzchar(descriptor))) {
    stop("'descriptor' must be NULL or a single non-empty character string.", call. = FALSE)
  }

  name <- paste0(asset_type, block_prefix)
  if (!is.null(suffix)) {
    name <- paste0(name, ".", suffix)
  }
  if (!is.null(descriptor)) {
    name <- paste0(name, "-", descriptor)
  }

  name
}

report_figure_name <- function(block_prefix, suffix = NULL, descriptor = NULL) {
  report_asset_name("Fig", block_prefix, suffix = suffix, descriptor = descriptor)
}

report_table_name <- function(block_prefix, suffix = NULL, descriptor = NULL) {
  report_asset_name("Tab", block_prefix, suffix = suffix, descriptor = descriptor)
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
  block_prefix,
  suffix = NULL,
  descriptor = NULL,
  width = 8,
  height = 6,
  data = NULL,
  plot_dir = NULL,
  data_dir = NULL,
  res = 144,
  save_pdf = TRUE,
  save_svg = FALSE
) {
  if (!is.character(block_prefix) || length(block_prefix) != 1L || !nzchar(block_prefix)) {
    stop("'block_prefix' must be a single non-empty character string.", call. = FALSE)
  }

  plot_stem <- report_figure_name(block_prefix, suffix = suffix, descriptor = descriptor)
  table_stem <- report_table_name(block_prefix, suffix = suffix, descriptor = descriptor)

  file_names <- list(
    png = paste0(plot_stem, ".png"),
    pdf = if (isTRUE(save_pdf)) paste0(plot_stem, ".pdf") else NULL,
    svg = if (isTRUE(save_svg)) paste0(plot_stem, ".svg") else NULL,
    tsv = if (!is.null(data)) paste0(table_stem, ".tsv") else NULL
  )

  if (reportdeck_is_linked_mode()) {
    asset_dir <- getOption("rmdreportdeck.asset_dir")
    if (!is.null(asset_dir)) {
      # Always write to asset_dir so HTML hrefs are always valid
      dir.create(asset_dir, recursive = TRUE, showWarnings = FALSE)
      reportdeck_write_plot_png(plot_obj, file.path(asset_dir, file_names$png),
        width = width, height = height, res = res)
      if (isTRUE(save_pdf)) {
        reportdeck_write_plot_pdf(plot_obj, file.path(asset_dir, file_names$pdf),
          width = width, height = height)
      }
      if (isTRUE(save_svg)) {
        reportdeck_write_plot_svg(plot_obj, file.path(asset_dir, file_names$svg),
          width = width, height = height)
      }
      if (!is.null(data)) {
        utils::write.table(data, file.path(asset_dir, file_names$tsv),
          sep = "\t", quote = FALSE, row.names = FALSE, na = "")
      }

      # Also write archival copies to explicit dirs if given and different from asset_dir
      norm_asset <- normalizePath(asset_dir, mustWork = FALSE)
      if (!is.null(plot_dir) && !identical(normalizePath(plot_dir, mustWork = FALSE), norm_asset)) {
        dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
        reportdeck_write_plot_png(plot_obj, file.path(plot_dir, file_names$png),
          width = width, height = height, res = res)
        if (isTRUE(save_pdf)) {
          reportdeck_write_plot_pdf(plot_obj, file.path(plot_dir, file_names$pdf),
            width = width, height = height)
        }
        if (isTRUE(save_svg)) {
          reportdeck_write_plot_svg(plot_obj, file.path(plot_dir, file_names$svg),
            width = width, height = height)
        }
      }
      if (!is.null(data_dir) && !is.null(data) &&
          !identical(normalizePath(data_dir, mustWork = FALSE), norm_asset)) {
        dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
        utils::write.table(data, file.path(data_dir, file_names$tsv),
          sep = "\t", quote = FALSE, row.names = FALSE, na = "")
      }

      asset_rel <- file.path(basename(dirname(asset_dir)), basename(asset_dir))
      png_href  <- file.path(asset_rel, file_names$png)
      pdf_href  <- if (isTRUE(save_pdf)) file.path(asset_rel, file_names$pdf) else NULL
      data_href <- if (!is.null(data)) file.path(asset_rel, file_names$tsv) else NULL

      return(structure(
        list(
          stem = plot_stem,
          block_prefix = block_prefix,
          suffix = suffix,
          descriptor = descriptor,
          plot_obj = plot_obj,
          data = data,
          width = width,
          height = height,
          res = res,
          file_names = file_names,
          png_uri = NULL, pdf_uri = NULL, data_uri = NULL,
          png_href = png_href, pdf_href = pdf_href, data_href = data_href
        ),
        class = "reportdeck_plot_bundle"
      ))
    }
  }

  # Portable mode: write explicit dirs, generate data URIs
  if (!is.null(plot_dir)) {
    dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
  }
  if (!is.null(data_dir) && !is.null(data)) {
    dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  }

  if (!is.null(plot_dir) && isTRUE(save_pdf)) {
    reportdeck_write_plot_pdf(plot_obj, file.path(plot_dir, file_names$pdf),
      width = width, height = height)
  }
  if (!is.null(plot_dir) && isTRUE(save_svg)) {
    reportdeck_write_plot_svg(plot_obj, file.path(plot_dir, file_names$svg),
      width = width, height = height)
  }
  if (!is.null(data) && !is.null(data_dir)) {
    utils::write.table(data, file.path(data_dir, file_names$tsv),
      sep = "\t", quote = FALSE, row.names = FALSE, na = "")
  }

  png_uri  <- reportdeck_asset_data_uri(report_plot_png_data_uri(plot_obj, width = width, height = height, res = res))
  pdf_uri  <- if (isTRUE(save_pdf)) reportdeck_asset_data_uri(report_plot_pdf_data_uri(plot_obj, width = width, height = height)) else NULL
  data_uri <- if (!is.null(data)) reportdeck_asset_data_uri(report_text_data_uri(report_tsv_text(data))) else NULL

  structure(
    list(
      stem = plot_stem,
      block_prefix = block_prefix,
      suffix = suffix,
      descriptor = descriptor,
      plot_obj = plot_obj,
      data = data,
      width = width,
      height = height,
      res = res,
      file_names = file_names,
      png_uri = png_uri, pdf_uri = pdf_uri, data_uri = data_uri,
      png_href = png_uri, pdf_href = pdf_uri, data_href = data_uri
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

  png_href  <- bundle$png_href  %||% bundle$png_uri
  pdf_href  <- bundle$pdf_href  %||% bundle$pdf_uri
  data_href <- bundle$data_href %||% bundle$data_uri

  links <- list(
    report_download_link(png_label, bundle$file_names$png, png_href)
  )

  if (!is.null(pdf_href) && !is.null(bundle$file_names$pdf)) {
    links <- c(links, list(report_download_link(pdf_label, bundle$file_names$pdf, pdf_href)))
  }

  if (!is.null(tsv_label) && !is.null(data_href) && !is.null(bundle$file_names$tsv)) {
    links <- c(links, list(report_download_link(tsv_label, bundle$file_names$tsv, data_href)))
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

# Package-level environment for state shared between hooks during a knit session.
.reportdeck_env <- new.env(parent = emptyenv())

reportdeck_setup <- function(extra_cache_deps = list(), cache_size_limit = 500 * 1024^2) {
  reportdeck_require_knitr()
  output_file <- knitr::opts_knit$get("output.file")
  if (is.null(output_file)) {
    output_file <- file.path(getwd(), "report.linked.html")
  }
  output_dir <- dirname(output_file)
  report_stem <- tools::file_path_sans_ext(tools::file_path_sans_ext(basename(output_file)))
  cache_path <- file.path(output_dir, ".knitr-cache", report_stem, "")

  # Store session state for use in hooks
  .reportdeck_env$cache_path <- cache_path
  .reportdeck_env$cache_size_limit <- cache_size_limit
  .reportdeck_env$registry_path <- paste0(cache_path, ".reportdeck_large_chunks.rds")
  .reportdeck_env$pre_chunk_vars <- character(0)

  # Load persistent large-chunk registry from previous renders
  if (file.exists(.reportdeck_env$registry_path)) {
    .reportdeck_env$large_chunk_labels <- readRDS(.reportdeck_env$registry_path)
  } else {
    .reportdeck_env$large_chunk_labels <- character(0)
  }

  knitr::opts_chunk$set(
    cache = TRUE,
    cache.path = cache_path,
    cache.extra = c(list(knitr::current_input()), extra_cache_deps),
    cache.lazy = FALSE,
    reportdeck_size_check = TRUE
  )
  knitr::opts_hooks$set(cache = reportdeck_chunk_cache_hook)
  knitr::knit_hooks$set(reportdeck_size_check = reportdeck_size_check_hook)
  invisible(cache_path)
}

reportdeck_setup_linked <- function(extra_cache_deps = list()) {
  reportdeck_setup(extra_cache_deps = extra_cache_deps)
}

reportdeck_require_knitr <- function() {
  if (!requireNamespace("knitr", quietly = TRUE)) {
    stop("The 'knitr' package is required for this helper.", call. = FALSE)
  }
}

reportdeck_is_uncached_chunk_label <- function(label) {
  if (is.null(label) || !length(label) || !nzchar(label[[1]])) {
    return(FALSE)
  }
  grepl("^(setup|load($|[_-])|meta(data)?$|strsanitize$|sanitize($|[_-])|helper(s)?($|[_-]))",
        label[[1]], perl = TRUE)
}

reportdeck_chunk_cache_hook <- function(options) {
  if (isTRUE(options$cache)) {
    if (reportdeck_is_uncached_chunk_label(options$label)) {
      options$cache <- FALSE
    } else if (options$label %in% .reportdeck_env$large_chunk_labels) {
      options$cache <- FALSE
    }
  }
  options
}

# Knit hook that runs before/after each chunk to detect newly created large objects.
# When large objects are found, the chunk is added to a persistent registry so that
# on subsequent renders the cache hook can disable caching for it before knitr
# attempts to serialize the objects.
reportdeck_size_check_hook <- function(before, options) {
  if (before) {
    .reportdeck_env$pre_chunk_vars <- ls(envir = knitr::knit_global())
    return(NULL)
  }

  label <- options$label

  # Skip chunks already handled by the label convention or registry
  if (reportdeck_is_uncached_chunk_label(label)) return(NULL)
  if (label %in% .reportdeck_env$large_chunk_labels) return(NULL)

  limit <- .reportdeck_env$cache_size_limit
  new_vars <- setdiff(ls(envir = knitr::knit_global()), .reportdeck_env$pre_chunk_vars)

  if (length(new_vars) == 0L) return(NULL)

  env <- knitr::knit_global()
  sizes <- vapply(new_vars, function(v) {
    tryCatch(
      as.numeric(object.size(get(v, envir = env, inherits = FALSE))),
      error = function(e) 0
    )
  }, numeric(1))
  has_large <- any(sizes > limit) || sum(sizes) > limit

  if (has_large) {
    updated <- unique(c(.reportdeck_env$large_chunk_labels, label))
    .reportdeck_env$large_chunk_labels <- updated
    dir.create(.reportdeck_env$cache_path, recursive = TRUE, showWarnings = FALSE)
    saveRDS(updated, .reportdeck_env$registry_path)
    # Remove any cache files already written for this chunk in the current render
    cache_files <- list.files(
      .reportdeck_env$cache_path,
      pattern = paste0("^", label, "[._]"),
      full.names = TRUE
    )
    if (length(cache_files) > 0L) unlink(cache_files)
  }

  NULL
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

render_reportdeck_item_content_html <- function(content, prefix = NULL) {
  if (inherits(content, "reportdeck_item_plot")) {
    if (reportdeck_is_linked_mode()) {
      asset_dir <- getOption("rmdreportdeck.asset_dir")
      if (!is.null(asset_dir)) {
        dir.create(asset_dir, recursive = TRUE, showWarnings = FALSE)
        png_stem <- if (!is.null(prefix)) paste0("plot-", prefix) else paste0("plot-", format(Sys.time(), "%H%M%OS6"))
        png_filename <- paste0(png_stem, ".png")
        reportdeck_write_plot_png(content$plot_obj, file.path(asset_dir, png_filename),
          width = content$width, height = content$height, res = content$res)
        img_src <- file.path(basename(dirname(asset_dir)), basename(asset_dir), png_filename)
        return(htmltools::tags$div(
          class = "report-item-plot-card report-plot-card",
          do.call(htmltools::tags$img,
            c(list(src = img_src, alt = "", class = "report-item-plot-image"), content$img_attrs))
        ))
      }
    }
    png_uri <- reportdeck_asset_data_uri(report_plot_png_data_uri(
      content$plot_obj,
      width = content$width,
      height = content$height,
      res = content$res
    ))
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

  if (inherits(content, "knit_asis")) {
    return(htmltools::HTML(paste(as.character(content), collapse = "\n")))
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

  if (inherits(content, "knit_asis")) {
    return(paste(as.character(content), collapse = "\n"))
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

render_reportdeck_item_panel_html <- function(panel, panel_id, open = FALSE, level = 1L) {
  body_children <- list()
  if (!is.null(panel$asset_bar)) {
    body_children <- c(body_children, list(panel$asset_bar))
  }
  if (!is.null(panel$note) && nzchar(panel$note)) {
    body_children <- c(body_children, list(htmltools::tags$p(panel$note, class = "report-item-note")))
  }
  if (length(panel$body) > 0) {
    rendered <- lapply(seq_along(panel$body), function(i) {
      render_reportdeck_item_content_html(panel$body[[i]], prefix = paste0(panel_id, "-", i))
    })
    body_children <- c(body_children, rendered)
  }

  panel_level <- max(1L, as.integer(level[[1]]))
  htmltools::tags$details(
    class = paste("report-item-panel", paste0("report-item-panel-level-", panel_level)),
    id = panel_id,
    `data-level` = as.character(panel_level),
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

render_reportdeck_item_panel_markdown <- function(panel, level = 1L) {
  heading_level <- max(3L, min(6L, as.integer(level[[1]]) + 2L))
  parts <- c(paste0(strrep("#", heading_level), " ", panel$title), "")
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

report_loop_section <- function(items, open_first = TRUE, max_open = 1L, level = 1L) {
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
  level <- as.integer(level[[1]])
  if (is.na(level) || level < 1L) {
    stop("'level' must be a positive integer.", call. = FALSE)
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
        render_reportdeck_item_panel_html(item, panel_id = panel_id, open = open_flag, level = level)
      },
      items,
      panel_ids,
      open_vec
    )

    html <- htmltools::tags$div(
      class = paste("report-loop-section", paste0("report-loop-section-level-", level)),
      `data-level` = as.character(level),
      `data-open-first` = tolower(as.character(isTRUE(open_first))),
      `data-max-open` = as.character(max_open),
      htmltools::tagList(rendered)
    )
    return(knitr::asis_output(reportdeck_compact_html(html)))
  }

  markdown <- paste(
    vapply(items, render_reportdeck_item_panel_markdown, character(1), level = level),
    collapse = "\n\n"
  )
  knitr::asis_output(markdown)
}

# ---------------------------------------------------------------------------
# Rmd authoring utilities
# ---------------------------------------------------------------------------

#' Emit a markdown section heading
#'
#' Writes a blank line, a heading of the requested level, and another blank
#' line to the output document. Call inside a \code{results="asis"} chunk.
#'
#' @param level Integer heading level (1 = \code{#}, 2 = \code{##}, etc.).
#' @param text Heading text.
#'
#' @return Called for its side-effect. Returns \code{invisible(NULL)}.
#' @export
h <- function(level, text) {
  writeLines(c("", paste0(strrep("#", level), " ", text), ""))
  invisible(NULL)
}

#' Write a data frame to a tab-separated file
#'
#' Creates parent directories if necessary, then writes \code{x} as a
#' tab-separated file without quoting or row names.
#'
#' @param x A data frame.
#' @param path Output file path.
#'
#' @return \code{path}, invisibly.
#' @export
write_tsv <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.table(x, file = path, sep = "\t", quote = FALSE,
                     row.names = FALSE, na = "")
  invisible(path)
}

#' Sanitize a string for use as an asset file suffix
#'
#' Replaces runs of characters outside \code{[A-Za-z0-9_.-]} with a single
#' underscore. Suitable for constructing asset filenames that are safe across
#' all file systems.
#'
#' @param x A character string.
#'
#' @return A sanitized character string.
#' @export
safe_asset_suffix <- function(x) {
  gsub("[^A-Za-z0-9_.\\-]+", "_", x)
}
