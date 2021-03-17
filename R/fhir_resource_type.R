#Class definition
setClass("fhir_resource_type",
		 contains = "character")

#Validity check
setValidity("fhir_resource_type",
			method = function(object){
				messages <- c()

				if(length(object) > 1 ){
					messages <- c(messages, paste0("Please provide only a single string to define the FHIR resource."))
				}

				if(length(messages)>0){messages}else{TRUE}

			}
)

#' Create fhir_resource object
#'
#' This function creates an object of class `fhir_resource_type` by taking a string defining a FHIR resource type
#' and formating it correctly,i.e. removing white space and slashes. It also checks the resource against the list
#' of resources provided at https://hl7.org/FHIR/resourcelist.html and converts cases appropriately
#' if the resource is found in that list.
#'
#' @param string A string containing the resource type. Will mostly be one of the official FHIR resource
#' types listed at https://hl7.org/FHIR/resourcelist.html
#' @return An fhir_resource object
#' @examples fhir_resource("MedicationAdministration")
#' @export
#'
fhir_resource_type <- function(string){

	#first time: run for validity checks
	new("fhir_resource_type", string)

	#remove / and white space
	string <- stringr::str_remove_all(string, "/| ")

	#fix capitals
	if(tolower(string) %in% tolower(existing_resource_types)){
		string <- existing_resource_types[tolower(string) == tolower(existing_resource_types)]
	}

	#second time: run with corrected input
	new("fhir_resource_type", string)
}
fhir_resource_type("patient")
