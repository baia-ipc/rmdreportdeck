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
- POSIX CLI wrappers `knit2html` and `knit2pdf`

## What the HTML shell adds

For HTML reports, the packaged shell adds a consistent report interface on top
of standard R Markdown content:

- level-1 sections become collapsible top-level report sections
- level-2 sections become nested collapsible subsections
- a section named `# Goal` opens by default when present
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
- use `#` headings for major sections
- use `##` headings for subsections
- add a `# Goal` section near the top when the purpose of the report should be
  opened immediately
- render figures and tables inline in standard chunks
- for important HTML figures/tables, add a `report_asset_bar()` so users can
  download the figure and the source data directly from the page

## Minimal example

```r
library(ggplot2)
library(rmdreportdeck)

df <- data.frame(group = c("A", "B"), value = c(2, 5))
plot_obj <- ggplot(df, aes(group, value, fill = group)) + geom_col()

report_asset_bar(
  "Downloads",
  report_download_link("PNG", "plot.png", report_plot_png_data_uri(plot_obj)),
  report_download_link("PDF", "plot.pdf", report_plot_pdf_data_uri(plot_obj)),
  report_download_link("TSV", "plot.tsv", report_text_data_uri(report_tsv_text(df)))
)
```

## Rendering

From R:

```r
render_html_report_with_runinfo("report.Rmd")
render_pdf_report_with_runinfo("report.Rmd")
```

From the CLI wrappers:

```bash
knit2html report.Rmd param=value
knit2pdf report.Rmd param=value
```

Both rendering paths create a `.runinfo` file next to the rendered report.

## Documentation and examples

See:

- `vignettes/getting-started.Rmd`
- `inst/examples/minimal_html_report.Rmd`
- `inst/examples/minimal_pdf_report.Rmd`

Those examples are generic and synthetic; no project data is bundled in the
package.
