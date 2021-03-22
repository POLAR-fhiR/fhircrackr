#Class definition
#' An S4 class to represent a design for cracking FHIR resources
#'
#'
#' @slot sep A string to separate pasted multiple entries.
#' @slot brackets  A character vector of length two defining the brackets surrounding indices for multiple entries,
#' e.g. `c( "<", ">")`. If this is empty (i.e. character of length 0), no indices will be added to multiple entries.
#' @slot rm_empty_cols Logical scalar. Remove empty columns?
#'
setClass(
	"fhir_style",
	slots = c(
		sep = "character",
		brackets = "character",
		rm_empty_cols = "logical"
	)
)

#Validity check
setValidity(
	"fhir_style",
	method = function(object) {
		messages <- c()
		if(1 < length(object@sep)) messages <- c(messages, "sep must be character of length 1")
		if(!length(object@brackets) %in% c(0, 2)) messages <- c(messages, "brackets must be character of length 2 or empty")
		if(1 < length(object@rm_empty_cols)) messages <- c(messages, "remove_empty_columns must be logical of length 1")
		if(0 < length(messages)) messages else TRUE
	}
)

#Constructor

#' Create fhir_style object
#'
#' This function creates an object of class `fhir_style`. It contains the three elements `sep`, `brackets` and `rm_empty_cols`

#' @param sep A string to separate pasted multiple entries. Defaults to `" "`
#' @param brackets  A character vector of length two defining the brackets surrounding indices for multiple entries, e.g. `c( "<", ">")`.
#' If this is empty (i.e. character of length 0, the default), no indices will be added to multiple entries.
#' @param rm_empty_cols Logical scalar. Remove empty columns? Defaults to `TRUE`.
#' @return A fhir_style object
#' @examples
#' fhir_style(sep = " ", brackets = c("[", "]"), rm_empty_cols = FALSE)


fhir_style <- function(sep=" ", brackets=character(), rm_empty_cols=TRUE) {
	new("fhir_style", sep=sep, brackets=brackets, rm_empty_cols=rm_empty_cols)
}


#methods
setMethod(
	"show",
	signature = "fhir_style",
	function(object) {
		sep <- if(object@sep==" ") paste0("'", object@sep, "'") else object@sep
		brackets <- if(length(object@brackets)==0) "character(0)" else paste0("'", object@brackets[1], "' '", object@brackets[2], "'")
		cat(paste0("fhir_style object with the following elements:\n\nsep: ", sep, "\nbrackets: ", brackets, "\nrm_empty_cols: ", object@rm_empty_cols))
	}
)