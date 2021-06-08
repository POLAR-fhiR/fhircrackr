
#Class definition

#' A representation of a FHIR resource type
#'
#' An object of class `fhir_resource_type` is a string containing a FHIR resource type.
#' It is part of a `fhir_table_description` which in turn is part of a `fhir_design` and used in
#' [fhir_crack()].
#' @export
#'
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
#' This function creates an object of class [fhir_resource_type-class]. It checks the resource type against the list
#' of resource types provided at https://hl7.org/FHIR/resourcelist.html, corrects wrong cases (which can be disabled with `fix_capitalization = FALSE`)
#' and throws a warning if the resource cannot be found at hl7.org.
#'
#' @param string A length one character vector containing the resource type. Will usually be one of the official FHIR resource
#' types listed at https://hl7.org/FHIR/resourcelist.html
#' @param fix_capitalization Correct wrong capitalization for known resource types? E.g. `patients -> Patients` or
#' `medicationstatement -> MedicationStatement`. Defaults to TRUE.
#' @return An [fhir_resource_type-class] object
#' @examples
#' fhir_resource_type("Patient")
#' fhir_resource_type("medicationadministration")
#' @export
#'
fhir_resource_type <- function(string, fix_capitalization = TRUE) {

	if(length(string)>1){stop("Please provide only a single string to define the FHIR resource.")}

	#convert to correct case and check for validity
	if(tolower(string) %in% tolower(existing_resource_types)&& fix_capitalization){
		if(!string %in% existing_resource_types){
			corrected <- existing_resource_types[tolower(string) == tolower(existing_resource_types)]
			message("Changing resource type \"", string, "\" into \"", corrected, "\".")
			string <- existing_resource_types[tolower(string) == tolower(existing_resource_types)]
		}
	}else{
		warning(
			"You gave \"", string, "\" as the resource type.\n",
			"This doesn't match any of the resource types defined under https://hl7.org/FHIR/resourcelist.html. ",
			"If you are sure the resource type is correct anyway, you can ignore this warning.\n")
	}

	result <- new("fhir_resource_type", string)

	result
}


setMethod(
	"show",
	"fhir_resource_type",
	function(object){
		cat("A fhir_resource_type object: ")
		cat(object)
		cat("\n")
	}
)