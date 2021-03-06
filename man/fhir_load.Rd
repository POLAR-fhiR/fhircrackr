% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download_resources.R
\name{fhir_load}
\alias{fhir_load}
\title{Load bundles from xml-files}
\usage{
fhir_load(directory)
}
\arguments{
\item{directory}{A string containing the path to the folder were the files are stored.}
}
\value{
A list of bundles in xml format.
}
\description{
Reads all bundles stored as xml files from a directory.
}
\examples{
#unserialize example bundle
bundles <- fhir_unserialize(medication_bundles)

#save to temporary directory
fhir_save(bundles, directory = tempdir())

#load from temporary directory
loaded_bundles <- fhir_load(tempdir())
}
