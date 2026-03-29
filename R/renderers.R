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
  normalizePath(path, winslash = "/", mustWork = FALSE)
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
  input_path <- normalizePath(input, mustWork = TRUE)
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

render_html_report <- function(input, params = list(), output_file = NULL, envir = new.env(parent = globalenv())) {
  rmd_file <- normalizePath(input, mustWork = TRUE)
  assets <- report_asset_paths()

  rmarkdown::render(
    input = rmd_file,
    output_format = "html_document",
    params = params,
    output_file = output_file,
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

render_pdf_report <- function(input, params = list(), output_file = NULL, envir = new.env(parent = globalenv())) {
  rmd_file <- normalizePath(input, mustWork = TRUE)

  rmarkdown::render(
    input = rmd_file,
    output_format = "pdf_document",
    params = params,
    output_file = output_file,
    envir = envir
  )
}

write_runinfo <- function(output_report, command = NULL, renderer = NULL, params = list(), timing = NULL) {
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
    "",
    "# Resources used",
    "Output of timing:",
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
  final_output <- if (file.exists(expected_output)) expected_output else normalize_report_path(rendered)
  write_runinfo(
    output_report = final_output,
    command = command,
    renderer = "rmdreportdeck::render_html_report",
    params = params,
    timing = timing
  )
  invisible(final_output)
}

render_pdf_report_with_runinfo <- function(input, params = list(), output_file = NULL, command = NULL) {
  expected_output <- resolve_expected_output_path(input, output_file, ".pdf")
  timing <- system.time({
    rendered <- render_pdf_report(input = input, params = params, output_file = output_file)
  })
  final_output <- if (file.exists(expected_output)) expected_output else normalize_report_path(rendered)
  write_runinfo(
    output_report = final_output,
    command = command,
    renderer = "rmdreportdeck::render_pdf_report",
    params = params,
    timing = timing
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
    render_html_report_with_runinfo(
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
