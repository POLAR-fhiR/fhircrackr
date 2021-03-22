#definition

#' An S4 class to represent parameters for FHIR search
#'
#' Objects of this class should always be created with a call to [fhir_parameters()]
#'
#' @slot params A list of fhir_key_value_pair objects
#'

setClass(
	"fhir_parameters",
	slots = c(param_pairs="list", param_string = "character")
)

#validity
setValidity(
	"fhir_parameters",
	function(object){
		messages <- c()

		if(any(!sapply(object@param_pairs, function(x){class(x)=="fhir_key_value_pair"}))){
			messages <- c(messages, "fhir_parameters can only contain fhir_key_value_pair objects.")
		}

		if(length(object@param_string)>1){
			messages <- c(messages, "param_string must be of length one")
		}

		if(length(messages)>0){messages}else{TRUE}
	}
)


#Initialize function
#only for internal use, creates param_string
setMethod(
	"initialize",
	"fhir_parameters",
	function(.Object,...){
	  	.Object <- callNextMethod()

	  	#paste
	  	pairs <- lapply(.Object@param_pairs, function(x){paste(x@key, x@value, sep="=")})
	  	.Object@param_string <- paste(pairs, collapse = "&")

	  	.Object
	 }
)


#constructor for user
#Generic method to allow for different input types

#' Create [fhir_parameters-class] object
#'
#' A [fhir_parameters-class] object can be created in three ways:
#'  - You provide a length 1 character with all the search parameters pasted together properly
#'  - You provide a list with length two character vectors containing key value pairs.
#'  - You pass a number of fhir_key_value_pair objects to the function.
#'  See examples.
#'
#' @param ... Either a length 1 character containing properly formatted FHIR search parameters, e.g.
#' `"gender=male&_summary=count"` or list of length 2 character vectors each representing one key value pair,
#' with the first element as the key and the second element as the value, e.g.
#' `list(c("gender", "male"), c("_summary", "count"))`or one or multiple fhir_key_value_pair objects
#'
#' @examples
#' #Three ways to create the same fhir_parameters object
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

		if(length(unique(classes))>1){stop("You cannot mix input classes")}

		new("fhir_parameters", param_pairs = l)
	}
)

setMethod(
	"fhir_parameters",
	signature = c(...= "character"),
	function(...){
		l <- list(...)
		classes <- sapply(l, class)

		if(length(unique(classes))>1){stop("You cannot mix input classes")}

		if(length(l)>1){
	  		stop("You can only provide one character to this function.")
	  	}
		l <- unlist(l)
		if(length(l)>1){
			stop("If you provide a character to this function, it has to be of length 1.")
		}
	  	pairs <- strsplit(l, "&", fixed=T)[[1]]
	  	pairs <- strsplit(pairs, "=")

	  	list <- lapply(pairs, function(x){new("fhir_key_value_pair", key= x[1], value=x[2])})

	  	new("fhir_parameters", param_pairs=list)
	}
)

setMethod(
	"fhir_parameters",
	signature = c(...="list"),
	function(...){
		l <- list(...)
		classes <- sapply(l, class)

		if(length(unique(classes))>1){stop("You cannot mix input classes")}

		if(length(l)>1){
			stop("You can only provide one list to this function.")
		}

		l <- unlist(l, recursive = F)

	  	if(any(!sapply(l, is.character))){
	  		stop("All list elements of the provided list must be of type character")
	  	}

	  	if(any(sapply(l, length)!=2)){
	  		stop("All list elements must be exactly of length two")
	  	}

	  	list <- lapply(l, function(x){new("fhir_key_value_pair", key= x[1], value=x[2])})

	  	new("fhir_parameters", param_pairs = list)
	}
)

#show method
setMethod(
	"show",
	"fhir_parameters",
	function(object){
		cat(paste0(
			"URL-encoded parameter string for FHIR search:\n", object@param_string,"\n\n"		  		)
		)
	}
)



