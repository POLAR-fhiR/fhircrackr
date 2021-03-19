#definition

#' An S4 class to represent parameters for FHIR search
#'
#' Objects of this class should always be created with a call to [fhir_parameters()]
#'
#' @slot keys A character vector defining the parameter keys
#' @slot values A character vector defining the parameter values
#' @slot paramstring A length 1 character containing properly formatted FHIR search parameters.
#' This slot is never set explicitly but is created automatically from the other slots.
#'
setClass("fhir_parameters",
		 slots = c(keys="character", values="character", paramstring="character"))

#validity
setValidity("fhir_parameters",
			function(object){
			messages <- c()

			if(length(object@keys) != length(object@values)){
				messages <- c(messages, "keys and values have to be the same length")
			}

			if(length(object@paramstring)>1){
				messages <- c(messages, "paramstring must be of length one")
			}

			if(length(messages)>0){messages}else{TRUE}
			})


#Initialize function
#only for internal use, creates paramstring from keys and values
setMethod("initialize", "fhir_parameters",
		  function(.Object,...){
		  	.Object <- callNextMethod()

		  	#create paramstring slot
		  	#remove leading/trailing whitespace
		  	keys <- stringr::str_trim(.Object@keys)
		  	values <- stringr::str_trim(.Object@values)

		  	#url encode
	  		for(i in 1:length(keys)){
	  			keys[i] <- utils::URLencode(keys[i], reserved = TRUE, repeated = FALSE)
	  			values[i] <- utils::URLencode(values[i], reserved = TRUE, repeated = FALSE)
	  		}

		  	#paste
		  	pairs <- paste0(keys, "=", values)
		  	.Object@paramstring <- paste(pairs, collapse = "&")

		  	.Object
		  })


#constructor for user
#Generic method to allow for different input types

#' Create [fhir_parameters-class] object
#'
#' A [fhir_parameters-class] object can be created in three different ways: Either you provide
#' one length 1 character or a list with length two character vectors containing all the parameters
#' in the argument `params`. Or you provide one character vector to the argument `keys` and one character
#' vector to the argument`values`. See examples.
#'
#' @param params Either a length 1 character containing properly formatted FHIR search parameters, e.g.
#' `"gender=male&_summary=count"` or list of length 2 character vectors each representing one key value pair,
#' with the first element as the key and the second element as the value, e.g.
#' `list(c("gender", "male"), c("_summary", "count"))`
#'
#' @param keys A character vector containing only keys for FHIR search parameters, e.g. `c("gender", "_summary")`
#' @param values A character vector containing only values for FHIR search parameters, e.g. `c("male", "count")`.
#' `values` must be the same length and order as `keys`
#'
#' @examples
#' #Three ways to create the same fhir_parameters object
#'
#' #using one string
#' fhir_parameters(params = "gender=male&birthdate=le2000-01-01&_summary=count")
#'
#' #using one list
#' fhir_parameters(params = list(c("gender", "male"),
#'                                   c("birthdate", "le2000-01-01"),
#'                                   c("_summary", "count")))
#' #using keys and values
#' fhir_parameters(keys = c("gender", "birthdate", "_summary"),
#'                 values = c("male", "le2000-01-01", "count"))
#'

setGeneric("fhir_parameters", function(params, keys, values){
	standardGeneric("fhir_parameters")
})

setMethod("fhir_parameters", signature = c(params= "missing", keys="character",
										   values="character"),
		  function(keys, values){
		  	new("fhir_parameters", keys=keys, values = values)
})

setMethod("fhir_parameters", signature = c(params= "character", keys="missing",
										   values="missing"),
		  function(params){
		  	if(length(params)>1){
		  		stop("When using a character, argument params has to be of length one.")
		  	}

		  	pairs <- strsplit(params, "&", fixed=T)[[1]]
		  	pairs <- strsplit(pairs, "=")
		  	keys <- sapply(pairs, function(x){x[1]})
		  	values <- sapply(pairs, function(x){x[2]})

		  	new("fhir_parameters", keys=keys, values = values)
		  })

setMethod("fhir_parameters", signature = c(params= "list", keys="missing",
										   values="missing"),
		  function(params){

		  	if(any(!sapply(params, is.character))){
		  		stop("All list elements for argument params must be of type character")
		  	}

		  	if(any(sapply(params, length)!=2)){
		  		stop("All list elements for argument params must have exactly length two")
		  	}

		  	keys <- sapply(params, function(x){x[1]})
		  	values <- sapply(params, function(x){x[2]})

		  	new("fhir_parameters", keys=keys, values = values)
		  })



#show method
setMethod("show", "fhir_parameters",
		  function(object){
		  	cat(paste0(
		  		"URL-encoded parameter string for FHIR search:\n", object@paramstring,"\n\n",
		  		"Keys: ", paste(object@keys, collapse = ", "), "\n",
		  		"Values: ", paste(object@values, collapse = ", ")
		  		)
		  	)
		  })



