#' A S4 class describing the form of data.frame produced by [fhir_crack()]
#'
#' A `fhir_table_description` is part of a `fhir_design` and holds the information [fhir_crack()] needs to flatten (aka crack)
#' FHIR resources from a FHIR bundle and is created with [fhir_table_description()].
#' There should be one `fhir_table_description` per resource type as [fhir_crack()] will create one data.frame/data.table
#' per resource type. See Details.
#'
#' @details
#' A `fhir_table_description` consists of
#' the following elements:
#'
#' - The resource element: Defines the resource type (e.g. `Patient` or `Observation`). See [fhir_resource_type()].
#' - The cols element: Contains the column names and XPath expressions defining the columns to extract.
#' If this element is empty, [fhir_crack()] will extract all available elements of the resource and name the
#' columns automatically. See [fhir_columns()].
#' - The style element: Defines how to deal with multiple entries to the same element and whether empty columns are
#' removed. See [fhir_style()].
#'
#' A full `fhir_table_description` looks for example like this:
#' ```
#' fhir_resource_type: Patient
#'
#' fhir_columns:
#' column name | xpath expression
#' ------------------------
#' name        | name/family
#' gender      | gender
#' id          | id
#'
#' fhir_style:
#' sep: ||
#' brackets: '[' ']'
#' rm_empty_cols: FALSE
#' ```
#'
#' @slot resource An object of class [fhir_resource_type-class] defining the resource type that
#' should be extracted.
#' @slot cols An object of class [fhir_columns-class] describing which columns should be created and how.
#' If this is an empty [fhir_columns-class] object, the call to [fhir_crack()] will extract all available
#' elements and put them in automatically named columns.
#' @slot style An object of class [fhir_style-class] describing how to deal with multiple entries and emtpy columns.
#' @include fhir_resource_type.R fhir_style.R fhir_columns.R
#' @seealso [fhir_resource_type()],[fhir_columns()],[fhir_style()], [fhir_design()], [fhir_crack()]
#' @export
setClass(
	Class = "fhir_table_description",
	slots = c(
		resource       = "fhir_resource_type",
		cols           = "fhir_columns",
		sep            = "character",
		brackets       = "character",
		rm_empty_cols  = "logical",
		format         = "character",
		keep_attr      = "logical",
		style          = "fhir_style"
	)
)

#' Create [fhir_table_description-class] object
#'
#' A `fhir_table_description` is part of a `fhir_design` and holds the information [fhir_crack()] needs to flatten (aka crack)
#' FHIR resources from a FHIR bundle. There should be one `fhir_table_description` per resource type as
#' [fhir_crack()] will create one data.frame/data.table per resource type. See Details.
#'
#' @details
#' A `fhir_table_description` consists of
#' the following elements:
#'
#' - The resource element: Defines the resource type (e.g. `Patient` or `Observation`). See `?fhir_resource`.
#' - The cols element: Contains the column names and XPath expressions defining the columns to extract.
#' If this element is empty, [fhir_crack()] will extract all available elements of the resource and name the
#' columns automatically. See `?fhir_columns`.
#' - The style element: Defines how to deal with multiple entries to the same element and whether empty columns are
#' removed. See `?fhir_style`
#'
#' A full `fhir_table_description` looks for example like this:
#' ```
#' fhir_resource_type: Patient
#'
#' fhir_columns:
#' column name | xpath expression
#' ------------------------
#' name        | name/family
#' gender      | gender
#' id          | id
#'
#' fhir_style:
#' sep: ||
#' brackets: '[' ']'
#' rm_empty_cols: FALSE
#' ```
#' @param resource A character vector of length one or [fhir_resource_type-class] object
#' indicating which resource type should be extracted.
#' @param cols Optional. A [fhir_columns-class] object or something that can be coerced to one,
#' like a (named) character vector, a (named) list containing xpath expressions,
#' or a [fhir_xpath_expression-class] object. See [fhir_columns()] and the examples.
#' If this argument is omitted, an empty [fhir_columns-class] object will be supplied.
#' This means that in the call to [fhir_crack()], all available elements are extracted in put
#' in automatically named columns.
#' @param style Optional. A [fhir_style-class] object, as created by [fhir_style()].
#' If this argument is omitted, default values will be assumed, see [fhir_style()].
#'
#' @return An object of class [fhir_table_description-class].
#'
#' @examples
#' #named character for cols
#' fhir_table_description(resource = "Patient",
#'                        cols = c(name = "name/family",
#'                                 gender = "gender",
#'                                 id = "id"),
#'                        style = fhir_style(sep = "||",
#'                                           brackets = c("[", "]"),
#'                                           rm_empty_cols = FALSE)
#' )
#'
#' #If style is ommitted, default values are assumed
#' fhir_table_description(resource = "Patient",
#'                        cols = c(name = "name/family",
#'                                 gender = "gender",
#'                                 id = "id")
#' )
#'
#' #named list for cols
#' fhir_table_description(resource = "Patient",
#'                        cols = list(name = "name/family",
#'                                    gender = "gender",
#'                                    id = "id")
#' )
#'
#' #unnamed character for cols, colnames are generated automatically
#' fhir_table_description(resource = "Patient",
#'                        cols = c("name/family",
#'                                 "gender",
#'                                 "id")
#' )
#' @export
fhir_table_description <- function(
	resource,
	cols           = fhir_columns(),
	sep            = ":::",
	brackets       = c("<|", "|>"),
	rm_empty_cols  = "logical",
	format         = "character",
	keep_attr      = "logical",
	style          = fhir_style()) {

	resource <- fhir_resource_type(string = resource)
	if(class(cols) != "fhir_columns") {cols <- fhir_columns(xpaths = cols)}
	new(
		Class         = "fhir_table_description",
		resource      = resource,
		cols          = cols,
		sep           = sep,
		brackets      = brackets,
		rm_empty_cols = rm_empty_cols,
		format        = format,
		keep_attr     = keep_attr,
		style         = style)
}

setMethod(
	f = "show",
	signature = "fhir_table_description",
	function(object) {
		cat("A fhir_table_description with the following elements: \n\n")
		cat(paste0("fhir_resource_type: ", as.character(object@resource), "\n\n"))
		cat("fhir_columns: \n"); show(object@cols)
		cat("fhir_columns: \n"); show(object@cols)
		cat("fhir_columns: \n"); show(object@cols)
		cat("\n\nfhir_style: \n");	show(object@style)
		cat("\n")
	}
)
