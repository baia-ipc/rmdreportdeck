# rmdreportdeck

`rmdreportdeck` is an R package for rendering polished, reusable styled R Markdown
reports with collapsible sections, embedded data/images with user download buttons,
a provenance run log file.

The project is meant for analytical workflows that already use `.Rmd` reports.
Its main goal is to make reports feel like durable analysis assets rather than ad hoc notebooks:
structured, navigable, downloadable, and easy to reuse across projects.

The package is intentionally generic. It is not tied to a specific scientific
domain, pipeline layout, dataset, or project repository.

## Interactive HTML and Fixed PDF reports

In practice, `rmdreportdeck` provides two closely related report targets:

- an HTML path with an interactive report shell
- a PDF path with the same rendering contract and the same `.runinfo`
  provenance logfile, but without the HTML-only interactive features

The HTML renderer is the feature-rich target. It turns a normal R Markdown
document into a report with collapsible sections, sidebar navigation, a fixed
layout, embedded figure/data download controls, and a presentation style that
is suitable for analysis reports shared with collaborators. The PDF renderer is
the static counterpart for cases where a plain document output is still needed.

## Core functionality

`rmdreportdeck` provides:

- HTML rendering through `render_html_report()` and
  `render_html_report_with_runinfo()`
- PDF rendering through `render_pdf_report()` and
  `render_pdf_report_with_runinfo()`
- `.runinfo` logfile generation for both HTML and PDF outputs
- packaged HTML shell assets for collapsible sections and report navigation
- helper functions for embedding downloadable figures and source data inside
  HTML reports
- bundle helpers for pairing one plot with its downloadable files and source
  table
- standardized asset naming helpers for block-oriented report sections
- loop-aware helpers for repeated figures/tables produced inside `for` loops
- POSIX CLI wrappers `knit2html` and `knit2pdf`

## What the HTML shell adds

For HTML reports, the packaged shell adds a consistent report interface on top
of standard R Markdown content:

- level-1 sections become collapsible top-level report sections
- level-2 sections become nested collapsible subsections
- a section named `# Goal`, `# Description`, `# Methods`, `# Workflow`, or `# Pipeline` opens by default when present
- a fixed left sidebar is generated from the section hierarchy
- a styled top bar and section cards make the report easier to scan
- downloadable figure/data controls can be embedded directly in the page

This means authors can keep writing ordinary `.Rmd` content while still getting
a much more structured report output.

## Installation

Install from GitHub with `remotes` or `renv`:

```r
remotes::install_github("baia-ipc/rmdreportdeck")
```

or:

```r
renv::install("baia-ipc/rmdreportdeck")
```

For stronger reproducibility, install a tagged release or a specific commit.

## Writing the Rmd

Write a normal R Markdown report with a simple structure.
The package does not replace standard R Markdown authoring. It adds a rendering
layer and helper functions around it.

Conventions:

- in the setup chunk, call `reportdeck_setup()` immediately after
  `library(rmdreportdeck)` — it configures knitr caching with provenance
  tracking and registers size-aware hooks
- use `#` headings for major sections
- use `##` headings for subsections
- add a `# Goal`, `# Description`, `# Methods`, `# Workflow`, or `# Pipeline`
  section near the top when the purpose or structure of the report should open
  immediately
- render figures and tables inline in standard chunks
- for important HTML figures/tables, emit download controls in a dedicated
  chunk with `results='asis'` and `cat(as.character(report_asset_bar(...)))` —
  keep the plot in its own separate chunk
- when a plot and its source table travel together, prefer
  `report_plot_bundle()`, `report_bundle_asset_bar()`, and
  `report_tsv_download_link()` instead of rebuilding the same three links by
  hand in every report
- when a report uses per-section identifiers such as `V01.03`, derive labels
  and filenames with `report_asset_name()`, `report_figure_name()`, and
  `report_table_name()` instead of open-coded string concatenation
- for repeated loop-generated outputs, keep one `##` subsection and render the
  repeated items inside it with `report_item_panel()` and `report_loop_section()`

## Minimal example

A complete minimal `.Rmd` for HTML rendering:

````markdown
---
title: "Example Report"
output: html_document
params:
  prjpath: !expr paste0(normalizePath(getwd()), "/")
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(message = FALSE, warning = FALSE, echo = FALSE)

library(ggplot2)
library(rmdreportdeck)
reportdeck_setup()

df <- data.frame(group = c("A", "B"), value = c(2, 5))
plot_obj <- ggplot(df, aes(group, value, fill = group)) + geom_col()
bundle <- report_plot_bundle(plot_obj, block_prefix = "V01.01", suffix = "plot", data = df)
```

# Goal

Describe the purpose of the report here.

## Figure

```{r fig_assets, results='asis', echo=FALSE}
cat(as.character(report_bundle_asset_bar("Downloads", bundle, tsv_label = "TSV")))
```

```{r fig}
print(plot_obj)
```
````

Key points:

- call `reportdeck_setup()` right after `library(rmdreportdeck)` in the setup chunk — this enables caching with provenance tracking
- emit asset bars in a dedicated chunk with `results='asis'` and `cat(as.character(...))`
- keep `print(plot_obj)` in a separate chunk so the figure renders independently of its download controls

`report_plot_bundle()` is the non-overlapping high-level helper added on top of
the existing low-level primitives:

- use `report_download_link()`, `report_plot_*_data_uri()`, and
  `report_text_data_uri()` directly when you need full manual control
- use `report_plot_bundle()` plus `report_bundle_asset_bar()` when you want one
  reusable object representing a plot, its downloadable PNG/PDF, and its TSV
  source data
- use `report_tsv_download_link()` when you only need a direct TSV download
  button for a table
- bundle naming is standardized automatically from one `block_prefix`

## Standardized asset naming

When a report follows a block-style naming scheme, keep the naming logic in one
place and reuse it for filenames and asset-bar labels.

The naming helpers use this format:

`<asset_type><block_prefix>[.<suffix>][-<descriptor>]`

Example:

```r
block_prefix <- "V01.03"

figure_stem <- report_figure_name(block_prefix, "nGenes_per_sample")
table_name <- report_table_name(block_prefix, "nGenes_per_sample.tsv")

figure_stem
# "FigV01.03.nGenes_per_sample"

table_name
# "TabV01.03.nGenes_per_sample.tsv"
```

Use the helpers when:

- the report already numbers sections or blocks
- the same name should appear in the saved filename and the visible asset bar
- you want a stable naming convention across multiple reports

Use the bundle helpers instead when:

- one plot and one source table travel together as a single reusable object
- the main need is automatic download-link assembly rather than naming policy

The two layers are compatible. A standardized bundle can use a figure-prefixed
plot stem and a table-prefixed TSV stem derived from one block identifier:

```r
block_prefix <- "V01.03"
bundle <- report_plot_bundle(
  plot_obj,
  block_prefix = block_prefix,
  suffix = "nGenes_per_sample",
  data = df
)
```

## Loop-generated outputs

For repeated outputs such as one figure per sample, pathway, or gene:

- keep one normal `##` subsection in the Rmd
- build one `report_item_panel()` per iteration
- finish the chunk with `report_loop_section(items)`

```r
items <- list()

for (i in seq_len(nrow(df))) {
  row <- df[i, , drop = FALSE]
  plot_obj <- ggplot(row, aes(group, value, fill = group)) + geom_col()
  bundle <- report_plot_bundle(
    plot_obj,
    stem = paste0("sample-", row$group),
    width = 5,
    height = 3.5,
    data = row
  )

  items[[i]] <- report_item_panel(
    title = paste("Sample", row$group),
    asset_bar = report_bundle_asset_bar(
      "Downloads",
      bundle,
      tsv_label = "TSV"
    ),
    report_item_plot(plot_obj),
    report_item_table(row)
  )
}

report_loop_section(items)
```

## Rendering

From R:

```r
render_html_report_with_runinfo("report.Rmd")
render_pdf_report_with_runinfo("report.Rmd")
```

From a project-level CLI wrapper (see below):

```bash
scripts/knit2html report.Rmd param=value
```

Both rendering paths create a `.runinfo` file next to the rendered report.

## Project-level `knit2html` wrapper

The package ships a generic `knit2html` CLI entry point, but projects that use
`renv` need a thin project-level wrapper script so that the correct library
path is active before the Rmd is knitted. Place the wrapper at
`scripts/knit2html` in the project root and make it executable.

A minimal template:

```bash
#!/usr/bin/env bash

set -euo pipefail

if [ $# -lt 1 ]; then
  echo "Usage: knit2html <Rmdfile> [param1=value1] ..." > /dev/stderr
  exit 1
elif [ ! -e "$1" ]; then
  echo "File does not exist: $1" > /dev/stderr
  exit 1
fi

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
PROJECT_ROOT="$( cd "$SCRIPT_DIR/.." && pwd )"
RMD_FILE="$1"
shift

# Resolve to absolute path before cd
if [[ "$RMD_FILE" != /* ]]; then
  RMD_FILE="$PWD/$RMD_FILE"
fi

# Run from project root so renv and .Rprofile are found
cd "$PROJECT_ROOT"
export R_PROFILE_USER="$PROJECT_ROOT/.Rprofile"

Rscript -e 'rmdreportdeck::knit2html_cli(commandArgs(trailingOnly = TRUE))' \
  "$RMD_FILE" "$@"
```

Projects may extend this wrapper to source additional environment files
(e.g. `config/render_env.sh`) or to support loading `rmdreportdeck` from
source via a `REPORTDECK_DEV_PATH` environment variable.

Why `cd "$PROJECT_ROOT"` matters: `renv` activates via `.Rprofile` in the
working directory. Without changing to the project root first,
`R_PROFILE_USER` may point to the right file but path-relative calls inside
the Rmd will resolve against whatever directory the user was in when they
called the wrapper. Running from the project root ensures that `here::here()`
and relative paths inside the Rmd all resolve consistently.

## Documentation and examples

See:

- `vignettes/getting-started.Rmd`
- `inst/examples/minimal_html_report.Rmd`
- `inst/examples/loop_html_report.Rmd`
- `inst/examples/minimal_pdf_report.Rmd`

Those examples are generic and synthetic; no project data is bundled in the
package.
