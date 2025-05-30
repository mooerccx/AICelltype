% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SeuratMarkerGeneToStr.r
\name{SeuratMarkerGeneToStr}
\alias{SeuratMarkerGeneToStr}
\title{Convert the top 10 marker genes obtained from Seurat analysis into a string that meets the input requirements of the GetCellType function.}
\usage{
SeuratMarkerGeneToStr(markergenes)
}
\arguments{
\item{markergenes}{: Top 10 marker gene list obtained from Seurat analysis.}
}
\value{
The concatenated string.
}
\description{
Convert the top 10 marker genes obtained from Seurat analysis into a string that meets the input requirements of the GetCellType function.
}
