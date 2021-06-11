#definition

#' An s4 class to represent a body for a POST to a FHIR server
#'
#' Objects of this class should always be created with a call to the function [fhir_body()]
#' @slot content a length 1 character representing the body for the post
#' @slot type a length 1 character defining the type of the body e.g. `"application/x-www-form-urlencoded"` or `"xml"`
#' @export

setClass(
	Class = "fhir_body",
	slots = c(content = "character", type = "character")
)

#validity
setValidity(
	Class = "fhir_body",
	method = function(object) {

		messages <- c()

		if(1 < length(object@type)) {
			messages <- c(messages, "the type of a fhir_body must have length 1")
		}

		if(1 < length(object@content)) {
			messages <- c(messages, "the content of a fhir_body must have length 1")
		}

		if(0 < length(messages)){messages} else {TRUE}
	}
)

#constructor
#generic method to allow for different input types

#' Create [fhir_body-class] object
#'
#' @param content A string representing the body for the post in the format specified in `type`.
#' If you provide a named list here, it will be taken as key value pairs of FHIR search parameters
#' and will be concatenated appropriately. In this case the `type` will automatically be set to
#' `"application/x-www-form-urlencoded"`. See examples.
#' @param type A string defining the type of the body e.g. `"application/x-www-form-urlencoded"` or `"xml"`.
#'
#' @return An object of type [fhir_body-class]
#' @export
#' @docType methods
#' @rdname fhir_body-methods
#' @examples
#' #body that could be used in a FHIR search request POSTed to an URL like baseurl/Patient/_search
#' fhir_body(content = "gender=female&_summary=count", type="application/x-www-form-urlencoded")
#' fhir_body(content = list("gender" = "female", "_summary" = "count"))
setGeneric(
	name = "fhir_body",
	def = function(content, type){
		standardGeneric("fhir_body")
	}
)

#' @rdname fhir_body-methods
#' @aliases fhir_body,list,missing-methods
setMethod(
	f = "fhir_body",
	signature = c(content = "list", type = "missing"),
	definition = function(content){

		if(any(!sapply(content, function(x) {is.character(x)}))) {
			stop("The provided list must have elements of type character")
		}

		if(is.null(names(content))) {
			stop("Please provide a named list.")
		}

		keys <- names(content)
		values <- unlist(content)
		pairs <- paste(keys, values, sep = "=")
		string <- paste(pairs, collapse = "&")

		new(Class = "fhir_body", content = string, type = "application/x-www-form-urlencoded")
	}
)


#' @rdname fhir_body-methods
#' @aliases fhir_body,list,character-methods
setMethod(
	f = "fhir_body",
	signature = c(content = "list", type = "character"),
	definition = function(content, type) {

		message("When content is a list, the type you provided will be overwritten with 'application/x-www-form-urlencoded'")

		if(any(!sapply(content, function(x) {is.character(x)}))) {
			stop("The provided list must have elements of type character")
		}

		if(is.null(names(content))) {
			stop("Please provide a named list.")
		}

		keys <- names(content)
		values <- unlist(content)
		pairs <- paste(keys, values, sep = "=")
		string <- paste(pairs, collapse = "&")

		new(Class = "fhir_body", content = string, type = "application/x-www-form-urlencoded")
	}
)
#' @rdname fhir_body-methods
#' @aliases fhir_body,character,character-methods
setMethod(
	f = "fhir_body",
	signature = c(content = "character", type = "character"),
	definition = function(content, type) {
		new(Class = "fhir_body", content = content, type = type)
	}
)

#show
setMethod(
	f = "show",
	signature = "fhir_body",
	definition = function(object) {
		cat(paste0("content:\n", object@content, "\n\ntype: ", object@type))
	}
)
