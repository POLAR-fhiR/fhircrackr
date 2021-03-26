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
#' An S4 class to represent a URL for FHIR search via GET
#'
#' Objects of this class should always be created with a call to [fhir_GETsearch_url()]
#'
#' @slot base A character vector of length 1 containing the base url to the FHIR server
#' @slot resource An object of type [fhir_resource_type-class] defining the resource type that is searched
#' @slot parameters An object of type [fhir_parameters-class] defining search parameters
#'
#' @include fhir_resource_type.R fhir_parameters.R

setClass(
	"fhir_GETsearch_url",
	slots = c(
		resource = "fhir_resource_type",
		parameters = "fhir_parameters"
	),
	contains = "fhir_url"
)

#' TODO: Document

setGeneric(
	"fhir_GETsearch_url",
	function(base, resource, parameters){
		standardGeneric("fhir_GETsearch_url")
	}
)

setMethod("fhir_GETsearch_url",
		  signature = c(base = "character", resource = "character", parameters = "fhir_parameters"),
		  function(base, resource, parameters){
		  	new("fhir_GETsearch_url", base=base, resource = fhir_resource_type(resource), parameters=parameters)
		  }
)

setMethod("fhir_GETsearch_url",
		  signature = c(base = "character", resource = "character", parameters = "missing"),
		  function(base, resource){
		  	new("fhir_GETsearch_url", base=base, resource = fhir_resource_type(resource))
		  }
)

setMethod("fhir_GETsearch_url",
		  signature = c(base = "character", resource = "character", parameters = "ANY"),
		  function(base, resource, parameters){
		  	new("fhir_GETsearch_url", base=base, resource = fhir_resource_type(resource), parameters = fhir_parameters(parameters))
		  }
)

setMethod(
	"show", "fhir_GETsearch_url",
	 function(object){
	 	keys <- sapply(object@parameters@param_pairs, function(x)x@key)
	 	values <- sapply(object@parameters@param_pairs, function(x)x@value)
	 	pairs <- paste(keys, values, sep = "=")
	 	string <- paste(pairs, collapse = "&")
	 	if(string!=""){string <- paste0("?", string)}
		 cat(paste0(
		 	"A fhir_GETsearch_url object:\n",
		 	object@base, "/",
		 	object@resource,
		 	string
		 ))
	 }
)

