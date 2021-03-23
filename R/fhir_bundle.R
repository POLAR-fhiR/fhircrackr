#Class definition
setClass(
	"fhir_bundle",
	contains = "VIRTUAL"
)


#' An S4 class to represent a FHIR bundle in xml form
#'
setClass(
	"fhir_bundle_xml",
	 contains = c("fhir_bundle", "xml_document", "xml_node")
)

setValidity(
	"fhir_bundle_xml",
	function(object){
		messages <- c()

		if(xml2::xml_name(object)!="Bundle"){
			messages <- c(messages,
						  "This xml doesn't seem to represent a bundle, its name is not 'Bundle'. Use xml2::xml_name() to check.")
		}

	if(length(messages)>0){messages}else{TRUE}
	}
)

#constructor
#' Create [fhir_bundle_xml-class] object
#'
#' @param bundle A xml-object representing a FHIR bundle
#'
#' @examples
#' fhir_bundle_xml(xml2::xml_unserialize(patient_bundles[[1]]))
#'
fhir_bundle_xml <- function(bundle){
	new("fhir_bundle_xml", bundle)
}

setMethod(
	"show",
	"fhir_bundle_xml",
	function(object){
		cat(paste0("A fhir_bundle_xml object with ", length(xml2::xml_find_all(object, "entry")), " entries:\n\n"))
		print(object)
    }
)

#' An S4 class to represent a FHIR bundle in serialized form

setClass(
	"fhir_bundle_serialized",
	contains = c("fhir_bundle", "raw")
)

#' Create [fhir_bundle_xml-class] object
#'
#' @param bundle A serialized xml object representing a FHIR bundle
#'
#' @examples
#' fhir_bundle_serialized(patient_bundles[[1]])

#constructor
fhir_bundle_serialized <- function(bundle){
	new("fhir_bundle_serialized", bundle)
}
