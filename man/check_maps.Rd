% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_maps.R
\name{check_maps}
\alias{check_maps}
\title{Check if Required Maps Exist in Directory and Construct Download Queue}
\usage{
check_maps(ids, start, end, export_folder_path)
}
\arguments{
\item{ids}{Vector of map fragment IDs}

\item{start}{Starting year}

\item{end}{Ending year}
}
\value{
List of downloaded maps
}
\description{
Check if Required Maps Exist in Directory and Construct Download Queue
}
\examples{
\dontrun{
check_maps(ids = c(1, 2, 3), start = 1990, end = 2000, export_folder_path)
}
}
