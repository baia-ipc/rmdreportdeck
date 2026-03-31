\name{report_plot_bundle}
\alias{report_plot_bundle}
\alias{report_bundle_download_links}
\alias{report_bundle_asset_bar}
\alias{report_tsv_download_link}
\title{Bundle helpers for plot-plus-data report downloads}
\description{
High-level helpers that package an in-report plot together with its
downloadable PNG/PDF assets and optional TSV source data.
}
\usage{
report_plot_bundle(
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
)

report_bundle_download_links(
  bundle,
  png_label = "Download PNG",
  pdf_label = "Download PDF",
  tsv_label = NULL
)

report_bundle_asset_bar(
  asset_label,
  bundle,
  png_label = "Download PNG",
  pdf_label = "Download PDF",
  tsv_label = NULL,
  note = NULL
)

report_tsv_download_link(df, filename, label = "Download TSV")
}
\arguments{
  \item{plot_obj}{A plot object that can be printed, such as a \code{ggplot}
  object or grob.}

  \item{block_prefix}{A standardized block identifier such as
  \code{"V01.03"}. The bundle computes a figure stem with
  \code{report_figure_name()} and a table stem with \code{report_table_name()}.}

  \item{suffix}{Optional suffix passed through to the standardized naming
  helpers when \code{block_prefix} is used.}

  \item{descriptor}{Optional descriptor passed through to the standardized
  naming helpers when \code{block_prefix} is used.}

  \item{width, height}{Plot dimensions in inches.}

  \item{data}{Optional data frame to expose as a TSV download and optionally
  write to disk.}

  \item{plot_dir}{Optional directory where a PDF side file, and optionally an
  SVG side file, should be written.}

  \item{data_dir}{Optional directory where the TSV side file should be written
  when \code{data} is supplied.}

  \item{res}{PNG resolution used for the embedded image data URI.}

  \item{save_pdf}{Whether to include a downloadable PDF representation.}

  \item{save_svg}{Whether to write an SVG side file when \code{plot_dir} is
  supplied. SVG files are not embedded as download links by these helpers.}

  \item{bundle}{A bundle created by \code{report_plot_bundle()}.}

  \item{png_label, pdf_label, tsv_label}{Labels used for the generated
  download links. Set \code{tsv_label = NULL} to suppress the TSV link even
  when the bundle contains data.}

  \item{asset_label}{Label shown at the start of the rendered asset bar.}

  \item{note}{Optional note passed through to \code{report_asset_bar()}.}

  \item{df}{A data frame to expose as a direct TSV download link.}

  \item{filename}{Download filename for the TSV link.}

  \item{label}{Visible label for the generated TSV link.}
}
\value{
  \code{report_plot_bundle()} returns a \code{"reportdeck_plot_bundle"} list.
  \code{report_bundle_download_links()} returns a list of HTML download-link
  tags. \code{report_bundle_asset_bar()} returns an HTML asset-bar tag.
  \code{report_tsv_download_link()} returns a single HTML anchor tag.
}
\details{
These helpers intentionally sit above the lower-level primitives already
provided by \pkg{rmdreportdeck}, such as \code{report_download_link()},
\code{report_plot_png_data_uri()}, and \code{report_text_data_uri()}.

Use the bundle helpers when one plot and one table travel together repeatedly,
especially inside loops. Use the lower-level primitives directly when you need
custom link composition or non-TSV payloads.

The helpers are intentionally aligned with the standardized naming layer. A
bundle always derives \code{Fig...} and \code{Tab...} names from one
\code{block_prefix}, plus the optional shared \code{suffix} and
\code{descriptor}.
}
\examples{
if (requireNamespace("ggplot2", quietly = TRUE)) {
  df <- data.frame(group = c("A", "B"), value = c(2, 5))
  plot_obj <- ggplot2::ggplot(df, ggplot2::aes(group, value, fill = group)) +
    ggplot2::geom_col()

  block_prefix <- "V01.01"
  bundle <- report_plot_bundle(
    plot_obj,
    block_prefix = block_prefix,
    suffix = "plot",
    data = df
  )
  report_bundle_download_links(bundle, tsv_label = "TSV")
  report_bundle_asset_bar("Downloads", bundle, tsv_label = "TSV")
  report_tsv_download_link(df, "plot.tsv", "TSV")
}
}
