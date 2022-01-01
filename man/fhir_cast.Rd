% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multiple_entries.R
\name{fhir_cast}
\alias{fhir_cast}
\title{Cast table with multiple entries
This function divides multiple entries in an indexed table as produced by \code{\link[=fhir_crack]{fhir_crack()}} into separate columns.}
\usage{
fhir_cast(indexed_df, brackets, sep, use_brackets = F, verbose = 1)
}
\arguments{
\item{indexed_df}{A data.frame/data.table with indexed multiple entries. Column names should reflect the XPath expression of the respective element.}

\item{brackets}{A character vector of length two, defining the brackets used for the indices.}

\item{sep}{A character vector of length one defining the separator that was used when pasting together multiple entries in \code{\link[=fhir_crack]{fhir_crack()}}.}

\item{use_brackets}{Put brackets around indices in the new column names? Defaults to \code{TRUE}.}

\item{verbose}{An integer vector of length one. If 0, nothing is printed, if 1, only general progress is printed, if > 1,
progress for each variable is printed. Defaults to 1.}
}
\description{
Every column containing multiple entries will be turned into multiple columns.
The number of columns created from a single column in the original table is determined by maximum number of
multiple entries occurring in this column. Rows with less than the maximally occurring number of entries will
be filled with NA values.
}
\details{
For \code{\link[=fhir_cast]{fhir_cast()}} to work properly, column names of the input data must reflect the Xpath to the corresponding resource element
with {.} as a separator, e.g. \code{code.coding.system}. These names are produced automatically by \code{\link[=fhir_crack]{fhir_crack()}}
when the names are not explicitly set in the \code{cols} element of the \code{\link[=fhir_table_description]{fhir_table_description()}}/\code{\link[=fhir_design]{fhir_design()}}.

In the names of the newly created columns the indices will be assigned to the respective elements of the column names.
See examples and the corresponding package vignette for a more detailed description.
}
\examples{

#unserialize example
bundles <- fhir_unserialize(bundles = example_bundles1)

#crack fhir resources
table_desc <- fhir_table_description(resource = "Patient",
                                     style = fhir_style(brackets = c("[","]"),
                                                        sep = " "))
df <- fhir_crack(bundles = bundles, design = table_desc)

#original df
df

#cast
fhir_cast(df, brackets=c("[","]"), sep=" ", verbose=0)

}
\seealso{
\code{\link[=fhir_crack]{fhir_crack()}}, \code{\link[=fhir_melt]{fhir_melt()}}, \code{\link[=fhir_build_bundle]{fhir_build_bundle()}}
}