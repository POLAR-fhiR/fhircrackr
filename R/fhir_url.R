
#' An S4 object to represent a URL for a FHIR server
#'
#' Objects of this class are basically strings (character vectors of length 1) representing
#' a URL. They are always url encoded. See `?fhir_url`.
#' @export
#'
setClass(
	"fhir_url",
	contains = "character"
)

#Validity check
setValidity(
	"fhir_url",
	method = function(object){
		messages <- c()
		if(1 < length(object)) {
			messages <- c(messages, paste0("A fhir_url has to be a character of length 1."))
		}
		if(0 < length(messages)) {messages} else {TRUE}
	}
)

#' Create FHIR URL
#'
#' This function creates an object of class [fhir_url-class] which mostly represents a URL-encoded URL for
#' a FHIR search request. A valid Search URL contains a base URL and a resource type and can contain additional
#' search parameters. For more info on FHIR search see https://www.hl7.org/fhir/search.html.
#'
#' You can use this function in two ways. If you provide just one string in the argument url with the full FHIR search request, this string
#' will be taken as a full FHIR search request. If you also provide the arguments `resource` and/or `parameters`, the string in `url` will be taken
#' as the base url of your FHIR server and the arguments will be concatenated appropriately to form the full request. See examples.
#'
#' Note that only the latter approach does a validity check on the resource type!
#'
#' @param url A character of length 1 specifying either the full search request,
#' e.g. `"http://hapi.fhir.org/baseR4/Patient?gender=male&_summary=count"`, or
#' the base URL to the FHIR server, e.g. `"http://hapi.fhir.org/baseR4"`.
#' @param resource A character of length 1 of [fhir_resource_type-class] object with the resource type to be searched, e.g. `"Patient"`.
#' @param parameters Optional. Either a length 1 character containing properly formatted FHIR search parameters, e.g.
#' `"gender=male&_summary=count"` or a named list or named character vector e.g. `list(gender="male", "_summary"="count")`
#' or `c(gender="male", "_summary"="count")`. Note that parameter names beginning with `_` have to be put in quotation marks!
#'
#' @return An object of class [fhir_url-class]
#' @docType methods
#' @rdname fhir_url-methods
#' @examples
#'
#' #provide full FHIR search request
#' fhir_url(url = "http://hapi.fhir.org/baseR4/Patient?gender=male&_summary=count")
#'
#' #provide base url and resource type
#' fhir_url(
#'    url = "http://hapi.fhir.org/baseR4",
#'    resource = "Patient"
#'  )
#'
#' #parameters in one string
#' fhir_url(
#'    url = "http://hapi.fhir.org/baseR4",
#'    resource = "Patient",
#'    parameters = "gender=male&_summary=count"
#'  )
#'
#' #parameters as a named character
#' fhir_url(
#'    url = "http://hapi.fhir.org/baseR4",
#'    resource = "Patient",
#'    parameters = c("gender" = "male", "_summary" = "count")
#'  )
#'
#' #parameters as a named list
#' fhir_url(
#'    url = "http://hapi.fhir.org/baseR4",
#'    resource = "Patient",
#'    parameters = list("gender" = "male", "_summary" = "count")
#'  )
#' @export

setGeneric(
	"fhir_url",
	function(url, resource, parameters){
		request <- standardGeneric("fhir_url")
		fhircrackr_env$current_request <- request
		request
	}
)

#' @aliases fhir_url,character,missing,missing-method
#' @rdname fhir_url-methods
setMethod(
	"fhir_url",
	signature = c(url="character", resource = "missing", parameters = "missing"),
	function(url){

		if(length(url)>0){url <- utils::URLencode(url)}

		new("fhir_url", url)

	}
)

#' @aliases fhir_url,character,character,missing-method
#' @rdname fhir_url-methods

setMethod(
	"fhir_url",
	signature = c(url = "character", resource = "character", parameters = "missing"),
	function(url, resource){

		resource <- fhir_resource_type(resource)

		request <- paste(url, resource, sep="/")

	 	new("fhir_url", utils::URLencode(request))
	}
)

#' @aliases fhir_url,character,character,character-method
#' @rdname fhir_url-methods

setMethod(
	"fhir_url",
	signature = c(url = "character", resource = "character", parameters = "character"),
	function(url, resource, parameters){

		resource <- fhir_resource_type(resource)
		request <- paste(url, resource, sep="/")


		if(length(parameters)==1 && grepl("=", parameters)){

			return(new("fhir_url", utils::URLencode(paste0(request, "?", parameters))))
		}

		if(is.null(names(parameters))){
			stop("A character vector has to be named to create parameters from it.")
		}

		keys <- names(parameters)
		pairs <- paste(keys, parameters, sep = "=")
		string <- paste(pairs, collapse = "&")

	 	new("fhir_url",  utils::URLencode(paste0(request, "?", string)))
	}

)

#' @aliases fhir_url,character,character,list-method
#' @rdname fhir_url-methods

setMethod(
	"fhir_url",
	signature = c(url = "character", resource = "character", parameters = "list"),
	function(url, resource, parameters){

		resource <- fhir_resource_type(resource)
		request <- paste(url, resource, sep="/")

		if(any(!sapply(parameters, function(x) {is.character(x)}))) {
			stop("The provided list must have elements of type character")
		}
		if(is.null(names(parameters))) {
			stop("Please provide a named list.")
		}

		keys <- names(parameters)
		values <- unlist(parameters)
		pairs <- paste(keys, values, sep = "=")
		string <- paste(pairs, collapse = "&")

		new("fhir_url",  utils::URLencode(paste0(request, "?", string)))
	}

)


