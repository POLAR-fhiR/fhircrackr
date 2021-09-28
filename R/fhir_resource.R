#Class definition
#' An S4 class to represent FHIR resources
setClass(
	Class = "fhir_resource",
	contains = "VIRTUAL"
)

setOldClass(Classes = "xml_node")
setOldClass(Classes = "xml_document")

#' An S4 class to represent a FHIR resource in xml form
#'
#' A `fhir_resource_xml` is an xml representation of a FHIR resource (https://www.hl7.org/fhir/resourcelist.html).
#'
#' @export
#'
setClass(
	Class = "fhir_resource_xml",
	contains = c("fhir_resource", "xml_node", "xml_document"),
	prototype = prototype(xml2::read_xml(x = "<Basic></Basic>"))
)


#constructor
#' Create [fhir_resource_xml-class] object
#'
#' @param resource A xml-object representing a FHIR resource
#'
#' @rdname fhir_resource_xml-methods
#' @docType methods
#'
#' @examples
#' fhir_resource_xml(resource = xml2::read_xml("<Patient><id value = '1'/></Patient>"))
#'
#' @export
#'

setGeneric(
	name = "fhir_resource_xml",
	def = function(resource) {
		standardGeneric("fhir_resource_xml")
	}
)

#' @rdname fhir_resource_xml-methods
#' @aliases fhir_resource_xml,xml_document-method
setMethod(
	f = "fhir_resource_xml",
	signature = "xml_document",
	definition = function(resource) {
		new(Class = "fhir_resource_xml", resource)
	}
)


#' @rdname fhir_resource_xml-methods
#' @aliases fhir_resource_xml,xml_node-method
setMethod(
	f = "fhir_resource_xml",
	signature = "xml_node",
	definition = function(resource) {
		new(Class = "fhir_resource_xml", resource)
	}
)

#' @rdname fhir_resource_xml-methods
#' @aliases fhir_resource_xml,character-method
setMethod(
	f = "fhir_resource_xml",
	signature = "character",
	definition = function(resource) {
		new(Class = "fhir_resource_xml", xml2::read_xml(resource))
	}
)

setMethod(
	f = "show",
	signature = "fhir_resource_xml",
	definition = function(object) {
		cat("A fhir_resource object\n")
		cat(toString(object))
	}
)

#' An S4 class to represent a FHIR resource in serialized form
#'
#' A `fhir_resource_serialized` is a `fhir_resource_xml` that has been serialized using [fhir_serialize()]. In this form, the
#' resource cannot be used in any meaningful way, but it can be saved and loaded as an `.RData` or `.rds` object without breaking the
#' external pointers in the xml. See `?fhir_serialize` and `?fhir_unserialize`.
#' @export

setClass(
	Class = "fhir_resource_serialized",
	contains = c("fhir_resource", "raw")
)

#' Create [fhir_resource_serialized-class] object
#'
#'Only for internal use
#'
#' @param resource A serialized xml object representing a FHIR bundle
#'
#' @examples
#' fhir_resource_serialized(serialize(xml2::read_xml("<Patient><id value = '1'/></Patient>")))
#' @noRd

#constructor
fhir_resource_serialized <- function(resource) {
	new(Class = "fhir_resource_serialized", resource)
}
