#definition

#' An S4 class to represent parameters for FHIR search
#'
#' Objects of this class should always be created with a call to [fhir_parameters()]
#'
#' @slot params A list of fhir_key_value_pair objects
#'

setClass(
	"fhir_parameters",
	contains = "character"
)

#validity
setValidity(
	"fhir_parameters",
	function(object) {
		messages <- c()
		if(is.null(names(object))) {
			messages <- c(messages, "fhir_parameters has to be a *named* character.")
		}
		if(0 < length(messages)) {messages} else {TRUE}
	}
)

setMethod(
	"initialize", "fhir_parameters",
	function(.Object,...) {
		.Object <- callNextMethod()


		#remove leading/trailing whitespace
		.Object <- stringr::str_trim(.Object)
		names(.Object) <- stringr::str_trim(names(.Object))

		#url encode
		for(i in 1: length(.Object)){
			.Object[i] <- utils::URLencode(.Object[i], reserved = TRUE, repeated = FALSE)
		}
		.Object
	}
)

#constructor for user
#Generic method to allow for different input types

#' Create [fhir_parameters-class] object
#'
#' A [fhir_parameters-class] object can be created in two ways:
#'  - You provide a length 1 character with all the search parameters pasted together properly
#'  - You provide a list with length two character vectors containing key value pairs.
#'  See examples.
#'
#' @param parameters Either a length 1 character containing properly formatted FHIR search parameters, e.g.
#' `"gender=male&_summary=count"` or list of length 2 character vectors each representing one key value pair,
#' with the first element as the key and the second element as the value, e.g. `list(c("gender", "male"), c("_summary", "count"))`
#'
#' @examples
#' #two ways to create the same fhir_parameters object
#'
#' #using a string
#' fhir_parameters("gender=male&birthdate=le2000-01-01&_summary=count")
#'
#' #using a list
#' fhir_parameters(list(c("gender", "male"),
#'                      c("birthdate", "le2000-01-01"),
#'                      c("_summary", "count")
#'                  )
#'  )


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
		if(1 < length(parameters)) {
	  		stop("You can only provide a character vector of lenth 1 to this function.")
	  	}
	  	pairs <- strsplit(parameters, "&", fixed=TRUE)[[1]]
	  	pairs <- strsplit(pairs, "=")
	  	keys <- sapply(pairs, function(x) {x[1]})
	  	values <- sapply(pairs, function(x) {x[2]})
	  	structure(values, names= keys, class="fhir_parameters")
	}
)

setMethod(
	"fhir_parameters",
	signature = c(parameters = "list"),
	function(parameters){
	  	if(any(!sapply(parameters, function(x) {is.character(x)}))) {
	  		stop("The provided list must have elements of type character")
	  	}
		if(any(sapply(parameters, length) != 2)) {
				stop("All list elements must be exactly of length two")
		}
		keys <- sapply(parameters, function(x) {x[1]})
		values <- sapply(parameters, function(x) {x[2]})
		structure(values, names= keys, class="fhir_parameters")
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

setMethod("print", signature = "fhir_parameters", function(x){show(x)})

