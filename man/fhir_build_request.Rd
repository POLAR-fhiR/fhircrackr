% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download_resources.R
\name{fhir_build_request}
\alias{fhir_build_request}
\title{Build FHIR search request from base url, resource type and search parameters}
\usage{
fhir_build_request(...)
}
\arguments{
\item{...}{Calls to  \code{\link{fhir_base}}, \code{\link{fhir_resource}} and \code{\link{fhir_key_value}}}
}
\value{
A string containing a FHIR search request ready for use
}
\description{
This function takes its arguments from the functions \code{\link{fhir_base}},
\code{\link{fhir_resource}} and \code{\link{fhir_key_value}}
You must provide exactly one call to \code{\link{fhir_base}}, and one call to
\code{\link{fhir_resource}}. You can provide none, one or multiple calls
to \code{\link{fhir_key_value}} (See examples).
}
\details{
Apart from returning the string the function saves the url as the current request.
It can be accessed with \code{\link{fhir_current_request}}
}
\examples{

#Look for all MedicationAdministration resources

fhir_build_request(fhir_base(url = "http://hapi.fhir.org/baseR4"),
               fhir_resource(resource = "MedicationAdministration")
               )

#current search request is updated to this url:
fhir_current_request()

#Look for all Condition resources,
#include Patient resources they refer to

fhir_build_request(fhir_base(url = "http://hapi.fhir.org/baseR4"),
               fhir_resource(resource = "Condition"),
               fhir_key_value(key = "_include", value = "Condition:patient")
               )

#Look for all Patient resources of Patients born before 1980,
#sort by death date

fhir_build_request(fhir_base("http://hapi.fhir.org/baseR4"),
               fhir_resource("Patient"),
               fhir_key_value("birthdate", "lt1980-01-01"),
               fhir_key_value("_sort", "death-date")
               )
}
