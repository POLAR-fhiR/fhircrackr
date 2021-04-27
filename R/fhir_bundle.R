#Class definition
#' An S4 class ro represent FHIR bundles
#' @include fhir_url.R
setClass(
	"fhir_bundle",
	contains = "VIRTUAL"
)


setOldClass("xml_node")


#' An S4 class to represent a FHIR bundle in xml form
#' @slot next_link A [fhir_url-class] pointing to the next bundle on the server
#' @slot self_link A [fhir_url-class] pointing to this bundle on the server
setClass(
	"fhir_bundle_xml",
	contains = c("fhir_bundle", "xml_node"),
	slots = c(next_link = "fhir_url",
			  self_link = "fhir_url"),
	prototype = prototype(xml2::read_xml("<Bundle></Bundle>"))
)

setValidity(
	"fhir_bundle_xml",
	function(object){
		messages <- c()
		if(xml2::xml_name(object) != "Bundle") {
			messages <- c(
				messages,
				"This xml doesn't seem to represent a bundle, its name is not 'Bundle'. Use xml2::xml_name() to check."
			)
		}
		if(0 < length(messages)) {messages} else {TRUE}
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
fhir_bundle_xml <- function(bundle) {

	xml2::xml_ns_strip(bundle)
	links <- xml2::xml_find_all(bundle, "link")
	rels.nxt <-	xml2::xml_text(xml2::xml_find_first(links, "./relation/@value")) == "next"
	rels.self <- xml2::xml_text(xml2::xml_find_first(links, "./relation/@value")) == "self"
	urls <- xml2::xml_attr(xml2::xml_find_all(links, "url"), "value")

	new("fhir_bundle_xml", bundle, next_link = fhir_url(urls[rels.nxt]), self_link = fhir_url(urls[rels.self]))
}

setMethod(
	"show",
	"fhir_bundle_xml",
	function(object) {
		cat(paste0("A fhir_bundle_xml object\n",
				   "No. of entries : ", length(xml2::xml_find_all(object, "entry")), "\n",
				   "Self Link: ", object@self_link, "\n",
				   "Next Link: ", object@next_link), "\n\n")
		print(object)
    }
)

#' An S4 class to represent a FHIR bundle in serialized form

setClass(
	"fhir_bundle_serialized",
	contains = c("fhir_bundle", "raw")
)

#' Create [fhir_bundle_serialized-class] object
#'
#' @param bundle A serialized xml object representing a FHIR bundle
#'
#' @examples
#' fhir_bundle_serialized(patient_bundles[[1]])

#constructor
fhir_bundle_serialized <- function(bundle) {
	new("fhir_bundle_serialized", bundle)
}
