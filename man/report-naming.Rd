\name{report_asset_name}
\alias{report_asset_name}
\alias{report_figure_name}
\alias{report_table_name}
\title{Standardized asset naming helpers}
\usage{
report_asset_name(asset_type, block_prefix, suffix = NULL, descriptor = NULL)

report_figure_name(block_prefix, suffix = NULL, descriptor = NULL)

report_table_name(block_prefix, suffix = NULL, descriptor = NULL)
}
\arguments{
  \item{asset_type}{A short asset class prefix such as \code{"Fig"} or \code{"Tab"}.}

  \item{block_prefix}{A block identifier such as \code{"V01.03"}.}

  \item{suffix}{Optional filename suffix appended after a dot.}

  \item{descriptor}{Optional descriptor appended after a dash.}
}
\value{
Character strings suitable for report asset labels and filenames.
}
\description{
Use these helpers to apply one naming convention across a report:
\code{<asset_type><block_prefix>[.<suffix>][-<descriptor>]}.
}
\details{
The naming helpers are intentionally separate from the bundle helpers. Use them
when the report already has a block-oriented naming convention and you want
consistent labels for filenames, download buttons, and asset bars.

Typical usage is:
\enumerate{
  \item define a \code{block_prefix} before a report section
  \item use \code{report_figure_name()} or \code{report_table_name()} to derive
  output filenames
  \item reuse the same generated name as the asset-bar label
}
}
\examples{
block_prefix <- "V01.03"

report_figure_name(block_prefix)
report_figure_name(block_prefix, "nGenes_per_sample")
report_table_name(block_prefix, "nGenes_per_sample.tsv")
report_asset_name("Aux", block_prefix, "qc", "raw")
}
