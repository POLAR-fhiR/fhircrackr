#This class should not be exported

#' An S4 class to represent a key value pair of a FHIR search parameter
#'#'
#' @slot keys A length 1 character defining the parameter key
#' @slot values A length 1 character defining the parameter value
#'
#'
#'
setClass("fhir_key_value_pair",
		 slots = c(key="character", value="character"))

#validity
setValidity("fhir_key_value_pair",
			function(object){
				messages <- c()

				if(length(object@key) > 1){
					messages <- c(messages, "The key has to be a character of length 1.")
				}

				if(length(object@value) > 1){
					messages <- c(messages, "The value has to be a character of length 1.")
				}


				if(length(messages)>0){messages}else{TRUE}
			})

setMethod("initialize", "fhir_key_value_pair",
		  function(.Object,...){
		  	.Object <- callNextMethod()

		  	#remove leading/trailing whitespace
		  	.Object@key <- stringr::str_trim(.Object@key)
		  	.Object@value <- stringr::str_trim(.Object@value)

		  	#url encode
		  	.Object@key <- utils::URLencode(.Object@key, reserved = TRUE, repeated = FALSE)
	  		.Object@value <- utils::URLencode(.Object@value, reserved = TRUE, repeated = FALSE)

			.Object

		  })

#constructor for user

#' Create [fhir_key_value_pair-class] object
#'
#' This function creates an URL encoded key value pair for FHIR search parameters
#'
#' @param key A length 1 character specifying a search parameter key, e.g `"gender"` or `"_summary"`
#' @param value A length 1 character specifying a search parameter value, e.g `"male"` or `"count"`
#' @examples
#' fhir_key_value_pair(key="gender", value="male")
#' fhir_key_value_pair(key="_summary", value="count")

fhir_key_value_pair <- function(key, value){
	if(is.numeric(value)){value <- as.character(value)}
	new("fhir_key_value_pair", key=key, value=value)
}

setMethod(
	"show",
	"fhir_key_value_pair",
	function(object){
		cat(
			paste0(
				object@key, "=", object@value
			)

		)
	})

