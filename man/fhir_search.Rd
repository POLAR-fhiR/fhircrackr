% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download_resources.R
\name{fhir_search}
\alias{fhir_search}
\title{Download FHIR search result}
\usage{
fhir_search(
  request = fhir_current_request(),
  username = NULL,
  password = NULL,
  max_bundles = Inf,
  verbose = 1,
  max_attempts = 5,
  delay_between_attempts = 10,
  log_errors = 0,
  save_to_disc = FALSE,
  delay_between_pages = 0,
  directory = paste0("FHIR_bundles_", gsub("-| |:", "", Sys.time()))
)
}
\arguments{
\item{request}{A string containing the full FHIR search request. Defaults to \code{\link{fhir_current_request}}}

\item{username}{A string containing the username for basic authentication. Defaults to NULL, meaning no authentication.}

\item{password}{A string containing the password for basic authentication. Defaults to NULL, meaning no authentication.}

\item{max_bundles}{Maximal number of bundles to get. Defaults to Inf meaning all available bundles are downloaded.}

\item{verbose}{An Integer Scalar.  If 0, nothings is printed, if 1, only finishing message is printed, if > 1,
downloading progress will be printed. Defaults to 2.}

\item{max_attempts}{A numeric scalar. The maximal number of attempts to send a request, defaults to 5.}

\item{delay_between_attempts}{A numeric scalar specifying the delay in seconds between two attempts. Defaults to 10.}

\item{log_errors}{Takes values 0, 1 or 2. Controls the logging of errors. 1 and 2 will write a file to the current working directory.

0: no logging of errors,

1: tabulate http response and write to csv-file

2: write http response as to xml-file}

\item{save_to_disc}{A logical scalar. If TRUE the bundles are saved as numerated xml-files into the directory specified
in the argument \code{directory} and not returned as a bundle list in the R session. This is useful when a lot of
bundles are to be downloaded and keeping them all in one R session might overburden working memory. When download
is complete, the bundles can be loaded into R using \code{\link{fhir_load}}. Defaults to FALSE, i.e. bundles are
returned as a list within the R session.}

\item{delay_between_pages}{A numeric scalar specifying a time in seconds to wait between pages of the search result,
i.e. between downloading the current bundle and the next bundle. This can be used to avoid choking a weak server with
too many requests to quickly. Defaults to zero.}

\item{directory}{The directory the bundles are saved to when \code{save_to_disc} is TRUE. Defaults to creating a
time-stamped directory into the current working directory.}
}
\value{
A list of bundles in xml format when \code{save_to_disc = FALSE} (the default),  else NULL.
}
\description{
Downloads all FHIR bundles of a FHIR search request from a FHIR server.
}
\examples{
\donttest{bundles <- fhir_search("https://hapi.fhir.org/baseR4/Medication?", max_bundles=3)}
}
