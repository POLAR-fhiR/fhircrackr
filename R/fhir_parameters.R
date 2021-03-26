#definition

#' An S4 class to represent parameters for FHIR search
#'
#' Objects of this class should always be created with a call to [fhir_parameters()]
#'
#' @slot params A list of fhir_key_value_pair objects
#'
#' @include fhir_key_value_pair.R

setClass(
	"fhir_parameters",
	slots = c(param_pairs = "list")
)

#validity
setValidity(
	"fhir_parameters",
	function(object) {
		messages <- c()
		if(any(!sapply(object@param_pairs, function(x) {class(x) == "fhir_key_value_pair"}))) {
			messages <- c(messages, "fhir_parameters can only contain fhir_key_value_pair objects.")
		}
		if(0 < length(messages)) {messages} else {TRUE}
	}
)


#constructor for user
#Generic method to allow for different input types

#' Create [fhir_parameters-class] object
#'
#' A [fhir_parameters-class] object can be created in three ways:
#'  - You provide a length 1 character with all the search parameters pasted together properly
#'  - You provide a list with length two character vectors containing key value pairs.
#'  - You provide a list of fhir_key_value_pair objects to the function.
#'  - You pass a number of fhir_key_value_pair objects to the function (not as a list).
#'  See examples.
#'
#' @param ... Either a length 1 character containing properly formatted FHIR search parameters, e.g.
#' `"gender=male&_summary=count"` or list of length 2 character vectors each representing one key value pair,
#' with the first element as the key and the second element as the value, e.g.
#' `list(c("gender", "male"), c("_summary", "count"))`or one or multiple fhir_key_value_pair objects
#' (can be wrapped) in a list.
#'
#' @examples
#' #Four ways to create the same fhir_parameters object
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
#' # using key value pairs
#' fhir_parameters(fhir_key_value_pair(key="gender", value="male"),
#'                 fhir_key_value_pair(key="birthdate", value="le2000-01-01"),
#'                 fhir_key_value_pair(key="_summary", value="count"))
#'
#' # using a list of key value pairs
#' fhir_parameters(list(
#'                 fhir_key_value_pair(key="gender", value="male"),
#'                 fhir_key_value_pair(key="birthdate", value="le2000-01-01"),
#'                 fhir_key_value_pair(key="_summary", value="count")
#'                 )
#' )

setGeneric(
	"fhir_parameters",
	function(...){
		standardGeneric("fhir_parameters")
	}
)

setMethod(
	"fhir_parameters",
	signature = c(...="fhir_key_value_pair"),
	function(...){
		l <- list(...)
		classes <- sapply(l, class)
		if(1 < length(unique(classes))) {stop("You cannot mix input classes")}
		new("fhir_parameters", param_pairs = l)
	}
)

setMethod(
	"fhir_parameters",
	signature = c(... = "character"),
	function(...) {
		l <- list(...)
		classes <- sapply(l, class)
		if(length(unique(classes))>1) {stop("You cannot mix input classes")}
		if(1 < length(l)) {
	  		stop("You can only provide one character to this function.")
	  	}
		l <- unlist(l)
		if(1 < length(l)) {
			stop("If you provide a character to this function, it has to be of length 1.")
		}
	  	pairs <- strsplit(l, "&", fixed=TRUE)[[1]]
	  	pairs <- strsplit(pairs, "=")
	  	list <- lapply(pairs, function(x) {new("fhir_key_value_pair", key=x[1], value=x[2])})
	  	new("fhir_parameters", param_pairs=list)
	}
)

setMethod(
	"fhir_parameters",
	signature = c(... = "list"),
	function(...){
		l <- list(...)
		classes <- sapply(l, class)
		if(1 < length(unique(classes))) {stop("You cannot mix input classes")}
		if(1 < length(l)) {
			stop("You can only provide one list to this function.")
		}
		l <- unlist(l, recursive = FALSE)
	  	if(any(!sapply(l, function(x) {is.character(x)|class(x)=="fhir_key_value_pair"}))) {
	  		stop("The provided list must have elements be of type character or of class fhir_key_value_pair")
	  	}
		if(is.character(l[[1]])) {
			if(any(sapply(l, length) != 2)) {
				stop("All list elements must be exactly of length two")
			}
			list <- lapply(l, function(x){new("fhir_key_value_pair", key=x[1], value=x[2])})
		} else {
			list <- l
		}
	  	new("fhir_parameters", param_pairs = list)
	}
)

#show method
setMethod(
	"show",
	"fhir_parameters",
	function(object){
		keys <- sapply(object@param_pairs, function(x)x@key)
		values <- sapply(object@param_pairs, function(x)x@value)
		pairs <- paste(keys, values, sep = "=")
		string <- paste(pairs, collapse = "&")
		colwidth1 <- max(stringr::str_length(keys)) + 1
		colwidth2 <- max(stringr::str_length(values)) + 1
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
					paste0(stringr::str_pad(keys, colwidth1, side="right"), "| ", values),
					collapse = "\n"
				),
				"\n\nURL-encoded parameter string for FHIR search:\n",
				string
			)
		)
	}
)



