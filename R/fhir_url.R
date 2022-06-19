#' build_request
#'
#' @param url A character of length 1.
#' @param resource A character of length 1.
#'
#' @return A character of length 1.
#' @noRd
#' @examples
#' build_request('https://server/', 'patient')
#' build_request('https://server', 'Patient')
build_request <- function(url, resource) {

	paste(
		url,
		fhir_resource_type(string = resource),
		sep = if(stringr::str_sub(string = url, start = -1) == '/') '' else '/'
	)
}

#' url_encode
#'
#' @param url A character of length 1.
#' @param url_enc A logical  of length 1, indicating whether encoding should be applied. Defaults to TRUE.
#'
#' @return A character of length 1.
#' @noRd
#' @examples
#' url_encode('https://server/Patient?id=https://codesys1.com|005,https://codesys2.com|AbD')
#' url_encode('https://server/Patient?id=https://codesys1.com|005,https://codesys2.com|AbD', FALSE)
url_encode <- function(url, url_enc = TRUE) {

	if(url_enc) {
		utils::URLencode(URL = url)
	} else {
		url
	}
}

#' An S4 object to represent a URL for a FHIR server
#'
#' Objects of this class are basically strings (character vectors of length one) representing
#' a URL. They are usually url encoded. See [fhir_url()] for how to build them.
#' @export
#'
setClass(
	Class = "fhir_url",
	contains = "character"
)

#Validity check
setValidity(
	Class = "fhir_url",
	method = function(object) {
		messages <- c()
		if(1 < length(object)) {
			messages <- c(messages, paste0("A fhir_url has to be a character of length one."))
		}
		if(0 < length(messages)) {messages} else {TRUE}
	}
)

#' Create FHIR URL
#'
#' This function creates an object of class [fhir_url-class] which mostly represents a URL-encoded URL for
#' a FHIR search request. A valid Search URL contains a base URL and a resource type and may contain additional
#' search parameters. For more info on FHIR search see https://www.hl7.org/fhir/search.html.
#'
#' You can use this function in two ways. If you provide just one string in the argument url with the full FHIR search request, this string
#' will be taken as a full FHIR search request. If you also provide the arguments `resource` and/or `parameters`, the string in `url` will be taken
#' as the base url of your FHIR server and the arguments will be concatenated appropriately to form the full request. See examples.
#'
#' Note that only the latter approach does a validity check on the resource type!
#'
#' You can disable URL-encoding by setting `url_enc=FALSE`.
#'
#' @param url A character of length one specifying either the full search request,
#' e.g. `"http://hapi.fhir.org/baseR4/Patient?gender=male&_summary=count"`, or
#' the base URL to the FHIR server, e.g. `"http://hapi.fhir.org/baseR4"`.
#' @param resource A character of length one or [fhir_resource_type-class] object with the resource type to be searched, e.g. `"Patient"`.
#' @param parameters Optional. Either a length 1 character containing properly formatted FHIR search parameters, e.g.
#' `"gender=male&_summary=count"` or a named list or named character vector e.g. `list(gender="male", "_summary"="count")`
#' or `c(gender="male", "_summary"="count")`. Note that parameter names beginning with `_` have to be put in quotation marks!
#' @param url_enc Should the url be URL-encoded? Defaults to `TRUE`.
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
#'    url      = "http://hapi.fhir.org/baseR4",
#'    resource = "Patient"
#'  )
#'
#' #parameters in one string
#' fhir_url(
#'    url        = "http://hapi.fhir.org/baseR4",
#'    resource   = "Patient",
#'    parameters = "gender=male&_summary=count"
#'  )
#'
#' #parameters as a named character
#' fhir_url(
#'    url        = "http://hapi.fhir.org/baseR4",
#'    resource   = "Patient",
#'    parameters = c("gender" = "male", "_summary" = "count")
#'  )
#'
#' #parameters as a named list
#' fhir_url(
#'    url        = "http://hapi.fhir.org/baseR4",
#'    resource   = "Patient",
#'    parameters = list("gender" = "male", "_summary" = "count")
#'  )
#' @export

setGeneric(
	name = "fhir_url",
	def = function(url, resource, parameters, url_enc = TRUE) {
		request <- standardGeneric("fhir_url")
		fhircrackr_env$current_request <- request
		request
	}
)

#' @aliases fhir_url,character,missing,missing-method
#' @rdname fhir_url-methods
setMethod(
	f = "fhir_url",
	signature = c(url = "character", resource = "missing", parameters = "missing"),
	definition = function(url, url_enc = TRUE) {

		new(
			Class = "fhir_url",
			url_encode(
				url     = url,
				url_enc = url_enc && 1 == length(url)
			)
		)
	}
)

#' @aliases fhir_url,character,character,missing-method
#' @rdname fhir_url-methods

setMethod(
	f = "fhir_url",
	signature = c(url = "character", resource = "character", parameters = "missing"),
	function(url, resource, url_enc = TRUE) {

	 	new(
	 		Class = "fhir_url",
	 		url_encode(
	 			url     = build_request(url = url, resource = resource),
	 			url_enc = url_enc
	 		)
	 	)
	}
)

#' @aliases fhir_url,character,character,character-method
#' @rdname fhir_url-methods

setMethod(
	f = "fhir_url",
	signature = c(url = "character", resource = "character", parameters = "character"),
	definition = function(url, resource, parameters, url_enc = TRUE) {

		request <- build_request(url = url, resource = resource)

		if(length(parameters) == 1 && grepl("=", parameters)) {
			request <- paste0(request, "?", parameters)
			return(new(
				Class = "fhir_url",
				url_encode(url = request, url_enc = url_enc)
			))
		}

		keys <- names(parameters)

		if(is.null(keys)) {
			stop("A character vector has to be named to create parameters from it.")
		}

		if("" %in% keys) {
			stop(
				"All elements in the parameter vector must have names. \n",
				"Please provide a name for the following parameters: \"",
				paste(parameters[keys == ""], collapse = "\", \""),
				"\"."
			)
		}

		pairs   <- paste(keys, parameters, sep = "=")
		string  <- paste(pairs, collapse = "&")
		request <- paste0(request, "?", string)

	 	new(
	 		Class = "fhir_url",
	 		url_encode(url = request, url_enc = url_enc)
	 	)
	}
)

#' @aliases fhir_url,character,character,list-method
#' @rdname fhir_url-methods

setMethod(
	f = "fhir_url",
	signature = c(url = "character", resource = "character", parameters = "list"),
	function(url, resource, parameters, url_enc = TRUE) {

		if(any(!sapply(parameters, is.character))) {
			stop("The provided list must have elements of type character")
		}

		keys <- names(parameters)

		if(is.null(keys)) {
			stop("Please provide a named list.")
		}

		request <- build_request(url = url, resource = resource)

		values  <- unlist(parameters)
		pairs   <- paste(keys, values, sep = "=")
		string  <- paste(pairs, collapse = "&")
		request <- paste0(request, "?", string)

		new(
			Class = "fhir_url",
			url_encode(url = request, url_enc = url_enc)
		)
	}
)

#' fhir_request
#' @description A Wrapper for fhir_url
#'
#' @param url The same as for `fhir_url()`.
#' @param resource The same as for `fhir_url()`. Defaults to NULL.
#' @param parameters The same as for `fhir_url()`. Defaults to NULL.
#' @param url_enc The same as for `fhir_url()`. Defaults to TRUE.
#'
#' @return The same as for `fhir_url()`.
#' @export
#'
#' @examples See `fhir_url()`!
fhir_request <- function(url, resource = NULL, parameters = NULL, url_enc = TRUE) {

	if(is.null(resource)) {
		if(is.null(parameters)) {
			fhir_url(url = url, url_enc = url_enc)
		} else {
			stop('\'resource\' has to be given in fhir_request(), if parameters shall be added.')
		}
	} else {
		if(is.null(parameters)) {
			fhir_url(url = url, resource = resource, url_enc = url_enc)
		} else {
			fhir_url(url = url, resource = resource, parameters = parameters, url_enc = url_enc)
		}
	}
}
