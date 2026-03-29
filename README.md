# rmdreportdeck

Standalone R package project for reusable R Markdown report rendering.

## What it provides

- `render_html_report()` and `render_html_report_with_runinfo()`
- `render_pdf_report()` and `render_pdf_report_with_runinfo()`
- `knit2html` and `knit2pdf` thin CLI wrappers
- reusable HTML shell assets for collapsible reports
- helper functions for embedded figure and data downloads inside HTML reports

## How to structure a report

Use a normal R Markdown document with standard YAML and then follow these
conventions:

- the report title should describe the step or report clearly
- level-1 headers (`#`) define the major report sections
- level-2 headers (`##`) define nested subsections inside each major section
- a section named `# Goal` opens by default when present
- figures and tables should be rendered inline in normal chunks
- for HTML reports, add a `report_asset_bar()` above important figures/tables so
  users can download the figure and the source data directly from the page

## Example HTML report pattern

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

See:

- `vignettes/getting-started.Rmd`
- `inst/examples/minimal_html_report.Rmd`
- `inst/examples/minimal_pdf_report.Rmd`
