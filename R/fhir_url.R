####base class####
#Class definition
setClass(
	"fhir_url",
	contains = "VIRTUAL",
	slots = c(base = "character")
)

#Validity check
setValidity(
	"fhir_url",
	method = function(object){
		messages <- c()
		if(1 < length(object@base)) {
			messages <- c(messages, paste0("The base for a fhir_url has to be a character of length 1."))
		}
		if(0 < length(messages)) {messages} else {TRUE}
	}
)

####derived classes####
#' An S4 class to represent a URL for a FHIR search request
#'
#' Objects of this class should always be created with a call to [fhir_search_url()]
#'
#' @slot base A character vector of length 1 containing the base url to the FHIR server
#' @slot resource An object of type [fhir_resource_type-class] defining the resource type that is searched
#' @slot parameters An object of type [fhir_parameters-class] defining search parameters
#'
#' @include fhir_resource_type.R fhir_parameters.R

setClass(
	"fhir_search_url",
	slots = c(
		resource = "fhir_resource_type",
		parameters = "fhir_parameters"
	),
	contains = "fhir_url"
)

#' Create URL for FHIR search
#'
#' This function creates an object of class [fhir_search_url-class] which represents a URL for
#' a FHIR search request. A valid URL must contain a base URL and a resource type and can contain additional
#' search parameters. For more info on FHIR search see https://www.hl7.org/fhir/search.html
#'
#' @param base A character of length 1 with the base URL to the FHIR server, e.g. `"http://hapi.fhir.org/baseR4"`
#' @param resource A character of length 1 with the resource type to be searched, e.g. `"Patient"`
#' @param parameters Optional. An object of type [fhir_parameters-class] or a string or list that can be
#' converted into an [fhir_parameters-class] object. See the help for [fhir_parameters()] for more info.
#'
#' @examples
#'
#' fhir_search_url(
#'    base = "http://hapi.fhir.org/baseR4",
#'    resource = "Patient"
#'  )
#'
#' fhir_search_url(
#'    base = "http://hapi.fhir.org/baseR4",
#'    resource = "Patient",
#'    parameters = "gender=male&_summary=count"
#'  )
#'
#' fhir_search_url(
#'    base = "http://hapi.fhir.org/baseR4",
#'    resource = "Patient",
#'    parameters = list(c("gender", "male"),
#'                      c("_summary", "count")
#'                 )
#'  )
#'


setGeneric(
	"fhir_search_url",
	function(base, resource, parameters){
		standardGeneric("fhir_search_url")
	}
)

setMethod("fhir_search_url",
		  signature = c(base = "character", resource = "character", parameters = "fhir_parameters"),
		  function(base, resource, parameters){
		  	new("fhir_search_url", base=base, resource = fhir_resource_type(resource), parameters=parameters)
		  }
)

setMethod("fhir_search_url",
		  signature = c(base = "character", resource = "character", parameters = "missing"),
		  function(base, resource){
		  	new("fhir_search_url", base=base, resource = fhir_resource_type(resource))
		  }
)

setMethod("fhir_search_url",
		  signature = c(base = "character", resource = "character", parameters = "ANY"),
		  function(base, resource, parameters){
		  	new("fhir_search_url", base=base, resource = fhir_resource_type(resource), parameters = fhir_parameters(parameters))
		  }
)

setMethod(
	"show", "fhir_search_url",
	 function(object){
	 	keys <- names(object@parameters)
	 	values <- object@parameters
	 	pairs <- paste(keys, values, sep = "=")
	 	string <- paste(pairs, collapse = "&")
	 	if(string!=""){string <- paste0("?", string)}
		 cat(paste0(
		 	"A fhir_search_url object:\n",
		 	object@base, "/",
		 	object@resource,
		 	string, "\n"
		 ))
	 }
)
