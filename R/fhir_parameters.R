#definition

#' An S4 class to represent parameters for FHIR search
#'
#' Objects of this class should always be created with a call to [fhir_parameters()]
#'
#' @slot names The names (keys) of the search parameters.


setClass(
	"fhir_parameters",
	contains = "character",
	slots = c(names = "character")
)

#validity
setValidity(
	"fhir_parameters",
	function(object) {
		messages <- c()
		if(length(names(object))==0) {
			messages <- c(messages, "fhir_parameters has to be a *named* character.")
		}
		if(0 < length(messages)) {messages} else {TRUE}
	}
)

setMethod(
	"initialize", "fhir_parameters",
	function(.Object,...) {
		args <- list(...)
		args <- unlist(args, recursive = F)
		#remove leading/trailing whitespace
		names <- stringr::str_trim(names(args))
		args <- stringr::str_trim(args)

		#url encode
		for(i in 1: length(args)){
			args[i] <- utils::URLencode(args[i], reserved = TRUE, repeated = FALSE)
		}
		callNextMethod(.Object, args, names = names)
	}
)


#' Create [fhir_parameters-class] object
#'
#' A [fhir_parameters-class] object can be created in three ways:
#'  - You provide a length 1 character with all the search parameters pasted together properly
#'  - You provide a named character vector with names representing the keys and values
#'  representing the values of the search parameters
#'  - You provide a named list of strings with names representing the keys and values
#'  representing the values of the search parameters
#'
#' @param parameters Either a length 1 character containing properly formatted FHIR search parameters, e.g.
#' `"gender=male&_summary=count"` or a named list or named character vector e.g. `list(gender="male", "_summary"="count")`
#' or `c(gender="male", "_summary"="count")`. Note that parameters beginning with `_` have to be put in quotation marks!
#'
#' @examples
#' #two ways to create the same fhir_parameters object
#'
#' #using a string
#' fhir_parameters("gender=male&birthdate=le2000-01-01&_summary=count")
#'
#' #using a character vector
#' fhir_parameters(c(gender="male", birthdate="le2000-01-01", "_summary"="count"))
#'
#' #using a list
#' fhir_parameters(list(gender="male", birthdate="le2000-01-01", "_summary"="count"))


setGeneric(
	"fhir_parameters",
	function(parameters){
		standardGeneric("fhir_parameters")
	}
)


setMethod(
	"fhir_parameters",
	signature = c(parameters = "character"),
	function(parameters) {
		if(length(parameters)==1 && grepl("=", parameters)) {
			pairs <- strsplit(parameters, "&", fixed=TRUE)[[1]]
			pairs <- strsplit(pairs, "=")
			keys <- sapply(pairs, function(x) {x[1]})
			values <- sapply(pairs, function(x) {x[2]})
			new("fhir_parameters", structure(values, names=keys))
		}else{
	  		if(is.null(names(parameters))){
	  			stop("A character vector has to be named to create parameters from it.")
	  		}
			new("fhir_parameters", parameters)

	  	}

	}
)

setMethod(
	"fhir_parameters",
	signature = c(parameters = "list"),
	function(parameters){
	  	if(any(!sapply(parameters, function(x) {is.character(x)}))) {
	  		stop("The provided list must have elements of type character")
	  	}
		if(is.null(names(parameters))) {
				stop("Please provide a named list.")
		}
		keys <- names(parameters)
		values <- unlist(parameters)
		new("fhir_parameters", structure(values, names=keys))
	}
)

#show method
setMethod(
	"show",
	"fhir_parameters",
	function(object){

		if(length(object)==0){cat("An empty fhir_parameters object"); return()}

		pairs <- paste(names(object), object, sep = "=")
		string <- paste(pairs, collapse = "&")
		colwidth1 <- max(stringr::str_length(names(object))) + 1
		colwidth2 <- max(stringr::str_length(object)) + 1
		header <- paste(
			stringr::str_pad("key", colwidth1 - 1, side="right"),
			"| value", "\n",
			paste(rep("-", colwidth1 + colwidth2), collapse=""),
			"\n",
			collapse = ""
		)
		cat(
			paste0(
				"A fhir parameters object:\n\n",
				header,
				paste(
					paste0(stringr::str_pad(names(object), colwidth1, side="right"), "| ", object),
					collapse = "\n"
				),
				"\n\nURL-encoded parameter string for FHIR search:\n",
				string
			)
		)
	}
)
