% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fhir_columns.R
\docType{methods}
\name{fhir_columns}
\alias{fhir_columns}
\alias{fhir_columns,missing,missing-method}
\alias{fhir_columns,NULL,missing-method}
\alias{fhir_columns,character,character-method}
\alias{fhir_columns,character,missing-method}
\alias{fhir_columns,list,missing-method}
\title{Create \linkS4class{fhir_columns} object}
\usage{
fhir_columns(xpaths, colnames)

\S4method{fhir_columns}{missing,missing}()

\S4method{fhir_columns}{NULL,missing}(xpaths)

\S4method{fhir_columns}{character,character}(xpaths, colnames)

\S4method{fhir_columns}{character,missing}(xpaths)

\S4method{fhir_columns}{list,missing}(xpaths)
}
\arguments{
\item{xpaths}{A (named) character vector or (named) list containing xpath xpaths,
or a \linkS4class{fhir_xpath_expression} object.}

\item{colnames}{The names of the columns to create. If no colnames are provided and the list or vector
in \code{xpaths} has names, those names are taken as the colnames. If no colnames are provided and
\code{xpaths} is unnamed too, the colnames are generated automatically from the xpath xpaths. See examples.}
}
\description{
An object of class \code{fhir_columns} is part of a \code{fhir_table_description} in a \code{fhir_design} and holds information on the elements
that should be extracted from the FHIR resources, as well as the column names of the resulting data.frame.
The elements to be extracted are indicated by XPath xpaths. If no column names are provided,
they are generated automatically and reflect the elements position in the resource.
}
\examples{
 #provide colnames explicitly
 fhir_columns(xpaths = c("name/given", "code/coding/code"),
              colnames = c("name", "code"))

 #colnames are taken from xpaths argument
 fhir_columns(xpaths = c(name = "name/given", code = "code/coding/code"))

 #colnames are taken from xpaths argument
 fhir_columns(xpaths = list(name = "name/given", code = "code/coding/code"))

 #colnames are generated automatically
 fhir_columns(xpaths = c("name/given", "code/coding/code"))
}
