
#Class definition

#' A representation of a FHIR resource type
setClass(
	"fhir_resource_type",
	contains = "character"
)

#Validity check
setValidity(
	"fhir_resource_type",
	method = function(object) {
		messages <- c()
		if(1 < length(object)) {
			messages <- c(messages, paste0("Please provide only a single string to define the FHIR resource."))
		}
		if(0 < length(messages)) {messages} else {TRUE}
	}
)

#' Create [fhir_resource_type-class] object
#'
#' This function creates an object of class [fhir_resource_type_class]. It checks the resource type against the list
#' of resource types provided at https://hl7.org/FHIR/resourcelist.html and throws a warning if it cannot be found there.
#'
#' @param string A length one character vector containing the resource type. Will mostly be one of the official FHIR resource
#' types listed at https://hl7.org/FHIR/resourcelist.html
#' @return An fhir_resource object
#' @examples fhir_resource_type("MedicationAdministration")
#' @export
#'
fhir_resource_type <- function(string) {

	if(!is.character(string)){stop("string must be of type character")}
	if(length(string)>1){stop("string must be of length 1")}

	if(!string %in% existing_resource_types){
		warning("The string you provided doesn't match any of the resource types under https://hl7.org/FHIR/resourcelist.html. ",
				"Case matters! If you are sure the resource type is correct anyway, you can ignore this warning.")
	}

	new("fhir_resource_type", string)
}

