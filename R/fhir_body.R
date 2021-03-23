



## Ich hab erst hinterher gemerkt, dass es sinnvoll wäre hier fhir_parameters im Konstruktor zuzulassen
## Deshalb lässt sich diese Klasse erst in Gänze testen, wenn sie mit den anderen Level 1 Klassen in den S4 Branch
## gemergt wurde.


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
#' @param content a string representing the body for the post in the format specified in `type` or
#' a [fhir_params-class] object. In the latter case the `type` argument should be omitted
#' and is automatically set to `"application/x-www-form-urlencoded"`
#'
#' @param type a string defining the type of the body e.g. `"application/x-www-form-urlencoded"` or `"xml"`
#'
#' @return an object of type [fhir_body-class]
#'
#' @examples
#'  #body that could be used in a FHIR seach request POSTed to an URL like baseurl/Patient/_search
#' fhir_body(content = "gender=female&_summary=count", type="application/x-www-form-urlencoded")
#'
#' #same request providing a fhir_parameters object
#' fhir_body(content = fhir_parameters(list(c("gender", "female"), c("_summary", "count"))))


setGeneric(
	"fhir_body",
	function(content, type){
		standardGeneric("fhir_body")
	}
)

setMethod(
	"fhir_body",
	c(content = "fhir_parameters", type="missing"),
	function(content){
		new("fhir_body", content = utils::URLdecode(content@paramstring), type="application/x-www-form-urlencoded")
	}
)

setMethod(
	"fhir_body",
	c(content = "fhir_parameters", type="character"),
	function(content, type){
		message("When body is a fhir_parameters object, the type you provided ",
				"will be overwritten with 'application/x-www-form-urlencoded'")
		new("fhir_body", content = utils::URLdecode(content@paramstring), type="application/x-www-form-urlencoded")
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
				object@content,"\n\n",
				"type: ", object@type
			)
		)
	}
)


