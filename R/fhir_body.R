#definition

#'an s4 class to represent a body for a post to a fhir server
#'
#'objects of this class should always be created with a call to the function [fhir_body()]
#' @slot content a length 1 character representing the body for the post
#' @slot type a length 1 character defining the type of the body e.g. `"application/x-www-form-urlencoded"` or `"xml"`

setClass(
	"fhir_body",
	slots = c(content = "character", type="character")
)

#validity
setValidity(
	"fhir_body",
	function(object){
		messages <- c()
		if(length(object@type)>1){
			messages <- c(messages, "the type of a fhir_body must have length 1")
		}
		if(length(object@content)>1){
			messages <- c(messages, "the content of a fhir_body must have length 1")
		}

		if(length(messages)>0){messages}else{TRUE}
	}
)

#constructor
#generic method to allow for different input types

#' create [fhir_body-class] object
#'
#' @param content A string representing the body for the post in the format specified in `type`.
#' If you provide a named list here, it will be taken as key value pairs of FHIR search parameters
#' and will be concatenated appropriately, in which case the `type` will automatically be set to
#' `"application/x-www-form-urlencoded"`. See examples.
#'
#' @param type A string defining the type of the body e.g. `"application/x-www-form-urlencoded"` or `"xml"`.
#'
#' @return An object of type [fhir_body-class]
#'
#' @examples
#'  #body that could be used in a FHIR seach request POSTed to an URL like baseurl/Patient/_search
#' fhir_body(content = "gender=female&_summary=count", type="application/x-www-form-urlencoded")
#' fhir_body(content = list("gender" = "female", "_summary" = "count"))


setGeneric(
	"fhir_body",
	function(content, type){
		standardGeneric("fhir_body")
	}
)

setMethod(
	"fhir_body",
	c(content = "list", type="missing"),
	function(content){
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

		new("fhir_body", content = string, type="application/x-www-form-urlencoded")
	}
)



setMethod(
	"fhir_body",
	c(content = "list", type="character"),
	function(content, type){
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

		new("fhir_body", content = string, type="application/x-www-form-urlencoded")
	}
)

setMethod(
	"fhir_body",
	c(content = "character", type="character"),
	function(content, type){

		new("fhir_body", content=content, type=type)
	}
)

#show
setMethod(
	"show",
	"fhir_body",
	function(object){
		cat(
			paste0(
				"content:\n", object@content,"\n\n",
				"type: ", object@type
			)
		)
	}
)



