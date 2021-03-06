% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multiple_entries.R
\name{fhir_melt_all}
\alias{fhir_melt_all}
\title{Melt all multiple entries}
\usage{
fhir_melt_all(indexed_data_frame, sep, brackets, rm_indices = TRUE)
}
\arguments{
\item{indexed_data_frame}{A data frame with indexed multiple entries.column names must
reflect the path to the corresponding resource element with \code{.} as a separator, e.g. \code{name.given}.
These names are produced automatically by \code{\link[=fhir_crack]{fhir_crack()}} when the cols element of the design is omitted or set to \code{NULL}.}

\item{sep}{A string defining the separator that was used when pasting together multiple entries in \code{\link[=fhir_crack]{fhir_crack()}}.}

\item{brackets}{A character vector of length 2, defining the brackets used for the indices.}

\item{rm_indices}{Logical of length one. Should indices be removed? If \code{FALSE} the indices from the input data are preserved
the way they are. They can be extracted with \code{\link[=fhir_extract_indices]{fhir_extract_indices()}}, removed with \code{\link[=fhir_rm_indices]{fhir_rm_indices()}}
and restored with \code{\link[=fhir_restore_indices]{fhir_restore_indices()}}}
}
\value{
A data frame where each multiple entry appears in a separate row.
}
\description{
This function divides all multiple entries in an indexed data frame as produced by \code{\link[=fhir_crack]{fhir_crack()}}
into separate observations.
}
\details{
Every row containing values that consist of multiple entries will be turned into multiple rows, one for each entry.
Values on other variables will be repeated in all the new rows. This function will only work if the column names
reflect the path to the corresponding resource element with \code{.} as a separator, e.g. \code{name.given}.
These names are produced automatically by \code{\link[=fhir_crack]{fhir_crack()}} when the cols element of the design is omitted or set to \code{NULL}.

If \code{rm_indices=FALSE} the original indices are kept for every entry. These are needed if you want to transform
the data back to FHIR resources.

For a more detailed description on how to use this function please see the package vignette.
}
\examples{
#generate example
bundle <- xml2::read_xml(
	"<Bundle>

		<Patient>
			<id value='id1'/>
			<address>
				<use value='home'/>
				<city value='Amsterdam'/>
				<type value='physical'/>
				<country value='Netherlands'/>
			</address>
			<birthDate value='1992-02-06'/>
		</Patient>

		<Patient>
			<id value='id2'/>
			<address>
				<use value='home'/>
				<city value='Rome'/>
				<type value='physical'/>
				<country value='Italy'/>
			</address>
			<address>
				<use value='work'/>
				<city value='Stockholm'/>
				<type value='postal'/>
				<country value='Sweden'/>
			</address>
			<birthDate value='1980-05-23'/>
		</Patient>

		<Patient>
			<id value='id3.1'/>
			<id value='id3.2'/>
			<address>
				<use value='home'/>
				<city value='Berlin'/>
			</address>
			<address>
				<type value='postal'/>
				<country value='France'/>
			</address>
			<address>
				<use value='work'/>
				<city value='London'/>
				<type value='postal'/>
				<country value='England'/>
			</address>
			<birthDate value='1974-12-25'/>
		</Patient>

	</Bundle>"
)

#crack fhir resources
design <- list(Patients = list(resource = ".//Patient",
               style = list(brackets = c("[","]"),
               sep = "||")
               ))
dfs <- fhir_crack(bundles = list(bundle), design = design)

fhir_melt_all(dfs[[1]], brackets = c("[","]"), sep="||")
}
