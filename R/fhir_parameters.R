#definition

#' An S4 class to represent parameters for FHIR search
#'
#' Objects of this class should always be created with a call to [fhir_parameters()]
#'
#' @slot params A list of fhir_key_value_pair objects
#'

setClass("fhir_parameters",
		 slots = c(param_pairs="list", param_string = "character"))

#validity
setValidity("fhir_parameters",
			function(object){
			messages <- c()

			if(any(!sapply(object@param_pairs, function(x){class(x)=="fhir_key_value_pair"}))){
				messages <- c(messages, "fhir_parameters can only contain fhir_key_value_pair objects.")
			}

			if(length(object@param_string)>1){
				messages <- c(messages, "param_string must be of length one")
			}

			if(length(messages)>0){messages}else{TRUE}
			})


#Initialize function
#only for internal use, creates param_string
setMethod("initialize", "fhir_parameters",
		  function(.Object,...){
		  	.Object <- callNextMethod()

		  	#paste
		  	pairs <- lapply(.Object@param_pairs, function(x){paste(x@key, x@value, sep="=")})
		  	.Object@param_string <- paste(pairs, collapse = "&")

		  	.Object
		  })


#constructor for user
#Generic method to allow for different input types

#' Create [fhir_parameters-class] object
#'
#' A [fhir_parameters-class] object can be created in two different ways: Either you provide
#' a length 1 character with all the search parameters pasted together properly.
#' Or you provide a list with length two character vectors containing key value pairs.See examples.
#'
#' @param params Either a length 1 character containing properly formatted FHIR search parameters, e.g.
#' `"gender=male&_summary=count"` or list of length 2 character vectors each representing one key value pair,
#' with the first element as the key and the second element as the value, e.g.
#' `list(c("gender", "male"), c("_summary", "count"))`
#'
#' @examples
#' #Two ways to create the same fhir_parameters object
#'
#' #using a string
#' fhir_parameters(params = "gender=male&birthdate=le2000-01-01&_summary=count")
#'
#' #using a list
#' fhir_parameters(params = list(c("gender", "male"),
#'                                   c("birthdate", "le2000-01-01"),
#'                                   c("_summary", "count")))
#'

setGeneric("fhir_parameters", function(params){
	standardGeneric("fhir_parameters")
})

setMethod("fhir_parameters", signature = c(params= "character"),
		  function(params){
		  	if(length(params)>1){
		  		stop("When using a character, argument params has to be of length one.")
		  	}

		  	pairs <- strsplit(params, "&", fixed=T)[[1]]
		  	pairs <- strsplit(pairs, "=")

		  	list <- lapply(pairs, function(x){new("fhir_key_value_pair", key= x[1], value=x[2])})

		  	new("fhir_parameters", param_pairs=list)
		  })

setMethod("fhir_parameters", signature = c(params= "list"),
		  function(params){

		  	if(any(!sapply(params, is.character))){
		  		stop("All list elements for argument params must be of type character")
		  	}

		  	if(any(sapply(params, length)!=2)){
		  		stop("All list elements for argument params must have exactly length two")
		  	}

		  	list <- lapply(params, function(x){new("fhir_key_value_pair", key= x[1], value=x[2])})

		  	new("fhir_parameters", param_pairs = list)
		  })



#show method
setMethod("show", "fhir_parameters",
		  function(object){
		  	cat(paste0(
		  		"URL-encoded parameter string for FHIR search:\n", object@param_string,"\n\n"		  		)
		  	)
		  })



