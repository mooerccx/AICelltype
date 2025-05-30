% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GenerateComparisonTable.r
\name{GenerateComparisonTable}
\alias{GenerateComparisonTable}
\title{Generate cell type mapping table.}
\usage{
GenerateComparisonTable(
  CelltypeList,
  model = "gpt-4.1-mini",
  api_key = "",
  base_url = ""
)
}
\arguments{
\item{CelltypeList}{: A deduplicated list of all annotated cell types.}

\item{model}{: The model name you want to use}

\item{api_key}{: Key for API interface}

\item{base_url}{: API interface address}
}
\value{
: Cell type mapping table
}
\description{
Generate cell type mapping table.
}
