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
