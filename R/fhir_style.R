#DEPRECATED#
#This class is deprecated since fhircrackr 2.0.0 and will be removed eventually.
#All information stored in the fhir_design have now been moved to fhir_table_description


#Class definition
#' An S4 class to represent a design for cracking FHIR resources
#'
#'
#' @slot sep A string to separate pasted multiple entries. Defaults to `":::"`.
#' @slot brackets  A character vector of length two defining the brackets surrounding indices for multiple entries,
#' e.g. `c("<|", "|>")`. If this is empty (i.e. character of length 0, the default), no indices will be added to multiple entries.
#' Empty strings (`""`) are not allowed.
#' @slot rm_empty_cols Logical scalar. Remove empty columns? Defaults to FALSE.
#' @export
#'
setClass(
	Class = "fhir_style",
	slots = c(
		sep = "character",
		brackets = "character",
		rm_empty_cols = "logical"
	)
)

#Validity check
setValidity(
	Class = "fhir_style",
	method = function(object) {

		messages <- c()
		if(1 < length(object@sep)) {messages <- c(messages, "sep must be character of length one")}
		if(!length(object@brackets) %in% c(0, 2)) {messages <- c(messages, "brackets must be character of length two or empty")}
		if("" %in% object@brackets) {messages <- c(messages, "You cannot use \"\" for brackets.")}
		if(1 < length(object@rm_empty_cols)) {messages <- c(messages, "remove_empty_columns must be logical of length one")}
		if(0 < length(messages)) {messages} else {TRUE}
	}
)

#Constructor

#' Create fhir_style object
#'
#' This function creates an object of class `fhir_style`.
#' It contains the three elements `sep`, `brackets` and `rm_empty_cols`. See Details.
#'
#' @details
#' A `fhir_style` object is part of a [fhir_table_description-class] which in turn is part of a [fhir_design-class] and
#' ultimately used in [fhir_crack()]. A `fhir_style` object contains three elements:
#'
#' - `sep`: A string defining the separator used to separate multiple entries for the same element in a FHIR resource,
#' e.g. multiple `address/city` elements in a Patient resource.
#' - `brackets`: A character vector of length two defining the brackets surrounding indices for multiple entries, e.g. `c("<|", "|>")`.
#' If this is empty (i.e. character of length 0, the default), no indices will be added to multiple entries.
#' Empty strings (`""`) are not allowed.
#' - `rm_empty_cols`: A logical scalar defining whether or not to remove empty columns after cracking. Empty columns arise when you
#' try to extract an element that doesn't appear in any of the resources.
#' A `fhir_style` object looks for example like this:
#'
#' ```
#' sep: ':::'
#' brackets: '<|' '|>'
#' rm_empty_cols: FALSE
#' ```
#'
#' @param sep A character vector of length one to separate pasted multiple entries. Defaults to `":::"`
#' @param brackets  A character vector of length two defining the brackets surrounding indices for multiple entries, e.g. `c("<|", "|>")`.
#' If this is empty (i.e. character of length zero, the default) or 'NULL', no indices will be added to multiple entries. If it is a character
#' vector of length one, it will be recycled to length two, i.e. `"|"` will become `c("|", "|")`.
#' Empty strings (`""`) are not allowed.
#' @param rm_empty_cols A logical vector of length one. Remove empty columns? Defaults to `FALSE`.
#' @return A fhir_style object
#' @examples
#' fhir_style(sep = ":::",
#'            brackets = c("<|", "|>"),
#'            rm_empty_cols = TRUE)
#'
#' @export


fhir_style <- function(sep = ":::", brackets = character(), rm_empty_cols = FALSE) {
	warning("fhir_style is deprecated since fhircrackr 2.0.0. Please don't use it anymore.\n",
			"The elements 'sep', 'brackets' and 'rm_empty_cols' should be moved directly into the fhir_table_description (see ?fhir_table_description).\n")
	if(is.null(brackets)) {brackets <- character()}
	if(any(is.na(brackets))) {stop("You cannot use NA in brackets.")}
	brackets <- fix_brackets(brackets = brackets)
	new(Class = "fhir_style", sep = sep, brackets = brackets, rm_empty_cols = rm_empty_cols)
}

#methods
setMethod(
	f = "show",
	signature = "fhir_style",
	function(object) {

		sep <- if(length(object@sep) == 0) {"character(0)"} else {paste0("'", object@sep, "'")}

		brackets <- if(length(object@brackets) == 0) {
			"character(0)"
		} else {
			paste0("'", object@brackets[1], "' '", object@brackets[2], "'")
		}

		rm_empty_cols <- if(length(object@rm_empty_cols) == 0) {"logical(0)"} else {object@rm_empty_cols}
		cat(paste0("sep: ", sep, "\nbrackets: ", brackets, "\nrm_empty_cols: ", rm_empty_cols))
	}
)