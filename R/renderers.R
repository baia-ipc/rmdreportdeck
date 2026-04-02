report_asset_paths <- function() {
  list(
    lua_filter = system.file("report-assets", "report_sections.lua", package = "rmdreportdeck"),
    header_html = system.file("report-assets", "report_sections_header.html", package = "rmdreportdeck"),
    footer_html = system.file("report-assets", "report_sections_footer.html", package = "rmdreportdeck")
  )
}

parse_cli_params <- function(values) {
  params <- list()
  if (length(values) == 0) {
    return(params)
  }

  for (value in values) {
    split_at <- regexpr("=", value, fixed = TRUE)[[1]]
    if (split_at < 1) {
      stop("Invalid parameter, expected key=value: ", value, call. = FALSE)
    }
    key <- substr(value, 1, split_at - 1)
    val <- substr(value, split_at + 1, nchar(value))
    params[[key]] <- val
  }

  params
}

normalize_report_path <- function(path) {
  if (grepl("^/", path)) {
    return(gsub("/+", "/", path))
  }
  gsub("/+", "/", file.path(getwd(), path))
}

absolute_cli_path <- function(path) {
  normalize_report_path(path)
}

default_output_basename <- function(input, extension) {
  input_name <- basename(input)
  sub("\\.[^.]+$", extension, input_name)
}

ensure_extension <- function(path, extension) {
  if (grepl(paste0("\\", extension, "$"), path, perl = TRUE)) {
    return(path)
  }
  paste0(path, extension)
}

resolve_expected_output_path <- function(input, output_file = NULL, extension) {
  input_path <- absolute_cli_path(input)
  input_dir <- dirname(input_path)

  if (is.null(output_file) || !nzchar(output_file)) {
    return(normalize_report_path(file.path(input_dir, default_output_basename(input_path, extension))))
  }

  if (grepl("^/", output_file)) {
    return(normalize_report_path(ensure_extension(output_file, extension)))
  }

  normalize_report_path(file.path(input_dir, ensure_extension(output_file, extension)))
}

normalize_optional_path <- function(path) {
  if (is.null(path) || length(path) == 0 || is.na(path) || !nzchar(path)) {
    return("unavailable")
  }
  normalize_report_path(path)
}

format_timing_block <- function(timing) {
  if (is.null(timing)) {
    return("timing unavailable")
  }

  lines <- c(
    sprintf("user.self: %.3f", unname(timing[["user.self"]] %||% 0)),
    sprintf("sys.self: %.3f", unname(timing[["sys.self"]] %||% 0)),
    sprintf("elapsed: %.3f", unname(timing[["elapsed"]] %||% 0)),
    sprintf("user.child: %.3f", unname(timing[["user.child"]] %||% 0)),
    sprintf("sys.child: %.3f", unname(timing[["sys.child"]] %||% 0))
  )
  paste(lines, collapse = "\n")
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || is.na(x)) y else x
}

capture_system_command <- function(command, args = character()) {
  output <- tryCatch(
    system2(command, args = args, stdout = TRUE, stderr = TRUE),
    error = function(e) paste("unavailable:", conditionMessage(e))
  )
  paste(output, collapse = "\n")
}

read_memtotal <- function() {
  if (!file.exists("/proc/meminfo")) {
    return("unavailable")
  }

  lines <- readLines("/proc/meminfo", warn = FALSE)
  match <- grep("^MemTotal:", lines, value = TRUE)
  if (length(match) == 0) {
    return("unavailable")
  }
  match[[1]]
}

relocate_rendered_output <- function(rendered, expected_output) {
  rendered_path <- normalize_report_path(rendered)
  expected_path <- normalize_report_path(expected_output)

  if (identical(rendered_path, expected_path) || !file.exists(rendered_path)) {
    return(expected_path)
  }

  dir.create(dirname(expected_path), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(expected_path)) {
    unlink(expected_path)
  }

  moved <- file.rename(rendered_path, expected_path)
  if (!isTRUE(moved)) {
    ok <- file.copy(rendered_path, expected_path, overwrite = TRUE)
    if (!isTRUE(ok)) {
      stop("Failed to move rendered report to expected output path.", call. = FALSE)
    }
    unlink(rendered_path)
  }

  expected_path
}

reportdeck_is_linked_mode <- function() {
  identical(getOption("rmdreportdeck.render_mode", default = "portable"), "linked")
}

write_knit_meta <- function(rmd_path, params, knit_meta_path) {
  mtime <- as.integer(file.info(rmd_path)$mtime)
  lines <- c(paste0("rmd_mtime=", mtime))
  if (length(params) > 0) {
    lines <- c(lines, paste0("param.", names(params), "=", unlist(params, use.names = FALSE)))
  }
  writeLines(lines, con = knit_meta_path, useBytes = TRUE)
  invisible(knit_meta_path)
}

read_knit_meta <- function(knit_meta_path) {
  if (!file.exists(knit_meta_path)) {
    return(NULL)
  }
  lines <- readLines(knit_meta_path, warn = FALSE)
  kv <- list()
  for (line in lines) {
    eq <- regexpr("=", line, fixed = TRUE)[[1]]
    if (eq < 1) next
    kv[[substr(line, 1, eq - 1)]] <- substr(line, eq + 1, nchar(line))
  }
  params <- list()
  for (k in grep("^param\\.", names(kv), value = TRUE)) {
    params[[sub("^param\\.", "", k)]] <- kv[[k]]
  }
  list(rmd_mtime = kv[["rmd_mtime"]], params = params)
}

knit_md_is_valid <- function(rmd_path, knit_md_path, knit_meta_path, params) {
  if (!file.exists(knit_md_path)) {
    return(FALSE)
  }
  meta <- read_knit_meta(knit_meta_path)
  if (is.null(meta) || is.null(meta$rmd_mtime)) {
    return(FALSE)
  }
  rmd_mtime <- as.integer(file.info(rmd_path)$mtime)
  stored_mtime <- suppressWarnings(as.integer(meta$rmd_mtime))
  if (is.na(stored_mtime) || rmd_mtime > stored_mtime) {
    return(FALSE)
  }
  norm <- function(p) {
    if (length(p) == 0) {
      return(list())
    }
    p <- lapply(p, as.character)
    nms <- names(p)
    if (is.null(nms)) {
      nms <- rep("", length(p))
    }
    names(p) <- nms
    p[order(names(p))]
  }
  if (!identical(norm(lapply(meta$params, as.character)), norm(lapply(params, as.character)))) {
    return(FALSE)
  }
  TRUE
}

render_html_report <- function(input, params = list(), output_file = NULL, envir = new.env(parent = globalenv())) {
  rmd_file <- absolute_cli_path(input)
  assets <- report_asset_paths()
  final_output <- resolve_expected_output_path(rmd_file, output_file, ".html")
  old_chunk_opts <- knitr::opts_chunk$get()
  old_output_file <- knitr::opts_knit$get("output.file")
  on.exit({
    do.call(knitr::opts_chunk$set, old_chunk_opts)
    knitr::opts_knit$set(output.file = old_output_file)
  }, add = TRUE)
  knitr::opts_knit$set(output.file = final_output)
  reportdeck_setup(extra_cache_deps = list(params, report_mode = "html"))

  rmarkdown::render(
    input = rmd_file,
    output_format = "html_document",
    params = params,
    output_file = final_output,
    output_options = list(
      pandoc_args = c(
        "--lua-filter", assets$lua_filter,
        "--include-in-header", assets$header_html,
        "--include-after-body", assets$footer_html
      )
    ),
    envir = envir
  )
}

render_html_report_linked <- function(input, params = list(), output_file = NULL, envir = new.env(parent = globalenv())) {
  rmd_file <- absolute_cli_path(input)
  assets <- report_asset_paths()
  final_output <- resolve_expected_output_path(rmd_file, output_file, ".linked.html")
  output_dir <- dirname(final_output)
  report_stem <- tools::file_path_sans_ext(tools::file_path_sans_ext(basename(final_output)))
  asset_dir <- file.path(output_dir, paste0(report_stem, "_files"), "reportdeck")
  old_chunk_opts <- knitr::opts_chunk$get()
  old_output_file <- knitr::opts_knit$get("output.file")

  options(rmdreportdeck.render_mode = "linked")
  options(rmdreportdeck.asset_dir = asset_dir)
  on.exit({
    do.call(knitr::opts_chunk$set, old_chunk_opts)
    knitr::opts_knit$set(output.file = old_output_file)
    options(rmdreportdeck.render_mode = NULL)
    options(rmdreportdeck.asset_dir = NULL)
  }, add = TRUE)
  knitr::opts_knit$set(output.file = final_output)
  reportdeck_setup(extra_cache_deps = list(params, report_mode = "linked"))

  rmarkdown::render(
    input = rmd_file,
    output_format = "html_document",
    params = params,
    output_file = final_output,
    intermediates_dir = output_dir,
    clean = FALSE,
    output_options = list(
      self_contained = FALSE,
      pandoc_args = c(
        "--lua-filter", assets$lua_filter,
        "--include-in-header", assets$header_html,
        "--include-after-body", assets$footer_html
      )
    ),
    envir = envir
  )
}

render_pdf_report <- function(input, params = list(), output_file = NULL, envir = new.env(parent = globalenv())) {
  rmd_file <- absolute_cli_path(input)
  final_output <- resolve_expected_output_path(rmd_file, output_file, ".pdf")
  old_chunk_opts <- knitr::opts_chunk$get()
  old_output_file <- knitr::opts_knit$get("output.file")
  on.exit({
    do.call(knitr::opts_chunk$set, old_chunk_opts)
    knitr::opts_knit$set(output.file = old_output_file)
  }, add = TRUE)
  knitr::opts_knit$set(output.file = final_output)
  reportdeck_setup(extra_cache_deps = list(params, report_mode = "pdf"))

  rmarkdown::render(
    input = rmd_file,
    output_format = "pdf_document",
    params = params,
    output_file = final_output,
    envir = envir
  )
}

write_runinfo <- function(output_report, command = NULL, renderer = NULL, params = list(),
                          timing = NULL, knit_reused = NULL, knit_timing = NULL) {
  report_path <- normalize_report_path(output_report)
  runinfo_path <- sub("\\.[^.]+$", ".runinfo", report_path)
  report_dir <- dirname(report_path)
  rscript_path <- Sys.which("Rscript")

  lines <- c(
    "# Basic information",
    paste("User:", Sys.info()[["user"]] %||% NA_character_),
    paste("System:", Sys.info()[["nodename"]] %||% NA_character_),
    paste("Date:", as.character(Sys.time())),
    "",
    "# Command information",
    paste("Absolute path of command:", normalize_optional_path(command)),
    paste("Absolute path of renderer:", renderer %||% "rmdreportdeck"),
    paste("Absolute path of report file:", report_path),
    paste("Absolute path of Rscript:", normalize_report_path(rscript_path)),
    paste("Rscript version:", paste(R.version$major, R.version$minor, sep = ".")),
    paste("Parameters:", paste(sprintf("%s=%s", names(params), unlist(params, use.names = FALSE)), collapse = " ")),
    ""
  )

  if (!is.null(knit_reused)) {
    lines <- c(
      lines,
      "# Knit reuse",
      paste("Knit reused:", if (isTRUE(knit_reused)) "yes" else "no"),
      ""
    )
  }

  lines <- c(lines, "# Resources used")

  if (!is.null(knit_timing)) {
    lines <- c(lines, "Knit timing:", format_timing_block(knit_timing), "")
  }

  pack_label <- if (!is.null(knit_timing) || !is.null(knit_reused)) "Pack timing:" else "Output of timing:"
  lines <- c(
    lines,
    pack_label,
    format_timing_block(timing),
    "",
    "# System information",
    paste("OS:", Sys.info()[["sysname"]] %||% NA_character_),
    paste("Kernel:", Sys.info()[["release"]] %||% NA_character_),
    paste("CPU architecture:", Sys.info()[["machine"]] %||% NA_character_),
    paste("Memory total:", read_memtotal()),
    paste("Number of logical CPUs:", parallel::detectCores(logical = TRUE)),
    "",
    "# Disk space",
    "Disk space left on report file partition (after running):",
    capture_system_command("df", c("-h", report_dir))
  )

  if (grepl("rundir", report_dir, fixed = TRUE)) {
    lines <- c(
      lines,
      "Space used by the step directory (after running):",
      capture_system_command("bash", c("-lc", sprintf("cd %s && du -sh *", shQuote(dirname(report_dir)))))
    )
  }

  writeLines(lines, con = runinfo_path, useBytes = TRUE)
  invisible(runinfo_path)
}

render_html_report_with_runinfo <- function(input, params = list(), output_file = NULL, command = NULL) {
  expected_output <- resolve_expected_output_path(input, output_file, ".html")
  timing <- system.time({
    rendered <- render_html_report(input = input, params = params, output_file = output_file)
  })
  final_output <- relocate_rendered_output(rendered, expected_output)
  write_runinfo(
    output_report = final_output,
    command = command,
    renderer = "rmdreportdeck::render_html_report",
    params = params,
    timing = timing
  )
  invisible(final_output)
}

render_html_report_linked_with_runinfo <- function(input, params = list(), output_file = NULL, command = NULL) {
  rmd_file <- absolute_cli_path(input)
  expected_output <- resolve_expected_output_path(input, output_file, ".linked.html")
  output_dir <- dirname(expected_output)
  report_stem <- tools::file_path_sans_ext(tools::file_path_sans_ext(basename(expected_output)))
  knit_md_path <- file.path(output_dir, paste0(report_stem, ".knit.md"))
  knit_meta_path <- file.path(output_dir, paste0(report_stem, ".knit.meta"))
  assets <- report_asset_paths()
  knit_reused <- knit_md_is_valid(rmd_file, knit_md_path, knit_meta_path, params)
  knit_timing <- NULL

  timing <- system.time({
    if (isTRUE(knit_reused)) {
      rendered <- render_linked_from_knit_md(knit_md_path, expected_output, assets)
    } else {
      knit_timing <<- system.time({
        rendered <- render_html_report_linked(input = input, params = params, output_file = output_file)
      })
    }
  })
  final_output <- relocate_rendered_output(rendered, expected_output)

  write_knit_meta(rmd_file, params, knit_meta_path)

  write_runinfo(
    output_report = final_output,
    command = command,
    renderer = "rmdreportdeck::render_html_report_linked",
    params = params,
    timing = timing,
    knit_reused = knit_reused,
    knit_timing = knit_timing
  )
  invisible(final_output)
}

render_pdf_report_with_runinfo <- function(input, params = list(), output_file = NULL, command = NULL) {
  expected_output <- resolve_expected_output_path(input, output_file, ".pdf")
  timing <- system.time({
    rendered <- render_pdf_report(input = input, params = params, output_file = output_file)
  })
  final_output <- relocate_rendered_output(rendered, expected_output)
  write_runinfo(
    output_report = final_output,
    command = command,
    renderer = "rmdreportdeck::render_pdf_report",
    params = params,
    timing = timing
  )
  invisible(final_output)
}

pack_from_knit_md <- function(knit_md_path, output_file, assets) {
  tmp_md <- sub("\\.knit\\.md$", "_pack_build.md", knit_md_path)
  file.copy(knit_md_path, tmp_md, overwrite = TRUE)
  on.exit(unlink(tmp_md), add = TRUE)
  rmarkdown::render(
    input = tmp_md,
    output_format = "html_document",
    output_file = output_file,
    output_options = list(
      self_contained = TRUE,
      pandoc_args = c(
        "--lua-filter", assets$lua_filter,
        "--include-in-header", assets$header_html,
        "--include-after-body", assets$footer_html
      )
    )
  )
}

render_linked_from_knit_md <- function(knit_md_path, output_file, assets) {
  tmp_md <- sub("\\.knit\\.md$", "_linked_build.md", knit_md_path)
  file.copy(knit_md_path, tmp_md, overwrite = TRUE)
  on.exit(unlink(tmp_md), add = TRUE)

  output_dir <- dirname(output_file)
  report_stem <- tools::file_path_sans_ext(tools::file_path_sans_ext(basename(output_file)))
  asset_dir <- file.path(output_dir, paste0(report_stem, "_files"), "reportdeck")

  options(rmdreportdeck.render_mode = "linked")
  options(rmdreportdeck.asset_dir = asset_dir)
  on.exit({
    options(rmdreportdeck.render_mode = NULL)
    options(rmdreportdeck.asset_dir = NULL)
  }, add = TRUE)

  rmarkdown::render(
    input = tmp_md,
    output_format = "html_document",
    output_file = output_file,
    intermediates_dir = output_dir,
    clean = FALSE,
    output_options = list(
      self_contained = FALSE,
      pandoc_args = c(
        "--lua-filter", assets$lua_filter,
        "--include-in-header", assets$header_html,
        "--include-after-body", assets$footer_html
      )
    )
  )
}

render_html_report_portable_with_runinfo <- function(input, params = list(), output_file = NULL, command = NULL) {
  rmd_file <- absolute_cli_path(input)
  expected_output <- resolve_expected_output_path(rmd_file, output_file, ".portable.html")
  output_dir <- dirname(expected_output)
  report_stem <- tools::file_path_sans_ext(tools::file_path_sans_ext(basename(expected_output)))
  knit_md_path <- file.path(output_dir, paste0(report_stem, ".knit.md"))
  knit_meta_path <- file.path(output_dir, paste0(report_stem, ".knit.meta"))
  assets <- report_asset_paths()

  knit_reused <- knit_md_is_valid(rmd_file, knit_md_path, knit_meta_path, params)

  knit_timing <- NULL
  if (!knit_reused) {
    knit_timing <- system.time({
      render_html_report_linked_with_runinfo(input = rmd_file, params = params, command = command)
    })
  }

  pack_timing <- system.time({
    packed <- pack_from_knit_md(knit_md_path, expected_output, assets)
  })
  final_output <- relocate_rendered_output(packed, expected_output)

  write_runinfo(
    output_report = final_output,
    command = command,
    renderer = "rmdreportdeck::render_html_report_portable",
    params = params,
    timing = pack_timing,
    knit_reused = knit_reused,
    knit_timing = knit_timing
  )
  invisible(final_output)
}

run_cli <- function(args, format) {
  if (length(args) < 1) {
    stop(
      sprintf("Usage: knit2%s <Rmdfile> [param1=value1] [param2=value2] ...", format),
      call. = FALSE
    )
  }

  rmd_file <- args[[1]]
  if (!file.exists(rmd_file)) {
    stop("File does not exist: ", rmd_file, call. = FALSE)
  }

  params <- parse_cli_params(args[-1])
  command_path <- Sys.getenv("REPORTDECK_COMMAND_PATH", unset = NA_character_)

  if (identical(format, "html")) {
    render_html_report_linked_with_runinfo(
      input = rmd_file,
      params = params,
      command = command_path
    )
  } else {
    render_pdf_report_with_runinfo(
      input = rmd_file,
      params = params,
      command = command_path
    )
  }

  invisible(NULL)
}

knit2html_cli <- function(args = commandArgs(trailingOnly = TRUE)) {
  run_cli(args = args, format = "html")
}

knit2pdf_cli <- function(args = commandArgs(trailingOnly = TRUE)) {
  run_cli(args = args, format = "pdf")
}

packhtml_cli <- function(args = commandArgs(trailingOnly = TRUE)) {
  if (length(args) < 1) {
    stop("Usage: packhtml <Rmdfile> [param1=value1] [param2=value2] ...", call. = FALSE)
  }
  rmd_file <- args[[1]]
  if (!file.exists(rmd_file)) {
    stop("File does not exist: ", rmd_file, call. = FALSE)
  }
  params <- parse_cli_params(args[-1])
  command_path <- Sys.getenv("REPORTDECK_COMMAND_PATH", unset = NA_character_)
  render_html_report_portable_with_runinfo(
    input = rmd_file,
    params = params,
    command = command_path
  )
  invisible(NULL)
}
