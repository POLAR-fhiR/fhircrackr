#Class definition
#' An S4 class to represent FHIR bundles
#' @include fhir_url.R
setClass(
	Class = "fhir_bundle",
	contains = "VIRTUAL"
)

setOldClass(Classes = "xml_node")


#' An S4 class to represent a FHIR bundle in xml form
#'
#' A `fhir_bundle_xml` is an xml representation of a FHIR bundle (https://www.hl7.org/fhir/bundle.html).
#' It is usually found inside a `fhir_bundle_list` which is returned by a call to [fhir_search()].
#'
#' @slot next_link A [fhir_url-class] pointing to the next bundle on the server.
#' @slot self_link A [fhir_url-class] pointing to this bundle on the server.
#' @export
#'
setClass(
	Class = "fhir_bundle_xml",
	contains = c("fhir_bundle", "xml_node"),
	slots = c(next_link = "fhir_url", self_link = "fhir_url"),
	prototype = prototype(xml2::read_xml(x = "<Bundle></Bundle>"))
)

setValidity(
	Class = "fhir_bundle_xml",
	method = function(object) {

		messages <- c()

		if(xml2::xml_name(x = object) != "Bundle") {
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
#' This should only be used if you want to create small examples. Usually, a `fhir_bundle_xml` will
#' be returned by [fhir_search()].
#'
#' @param bundle A xml-object representing a FHIR bundle
#' @examples
#' fhir_bundle_xml(bundle = xml2::xml_unserialize(patient_bundles[[1]]))
#'
#' @export
#'
fhir_bundle_xml <- function(bundle) {
	bundle <- fhir_ns_strip(xml = bundle)
	links <- xml2::xml_find_all(x = bundle, xpath = "link")
	rels.nxt <-	xml2::xml_text(x = xml2::xml_find_first(x = links, xpath = "./relation/@value")) == "next"
	rels.self <- xml2::xml_text(x = xml2::xml_find_first(x = links,xpath = "./relation/@value")) == "self"
	urls <- xml2::xml_attr(x = xml2::xml_find_all(x = links, xpath = "url"), attr = "value")
	new(Class = "fhir_bundle_xml", bundle, next_link = fhir_url(url = urls[rels.nxt]), self_link = fhir_url(url = urls[rels.self]))
}

setMethod(
	f = "show",
	signature = "fhir_bundle_xml",
	definition = function(object) {
		cat(
			paste0(
				"A fhir_bundle_xml object\n",
				"No. of entries : ", length(xml2::xml_find_all(object, "entry")), "\n",
				"Self Link: ", object@self_link, "\n",
				"Next Link: ", object@next_link
			), "\n\n"
		)
		print(object)
    }
)

#' An S4 class to represent a FHIR bundle in serialized form
#'
#' A `fhir_bundle_serialized` is a `fhir_bundle_xml` that has been serialized using [fhir_serialize()]. In this form, the
#' bundle cannot be used in any meaningful way, but it can be saved and loaded as an `.RData` or `.rds` object without breaking the
#' external pointers in the xml. See `?fhir_serialize` and `?fhir_unserialize`.
#' @export

setClass(
	Class = "fhir_bundle_serialized",
	contains = c("fhir_bundle", "raw")
)

#' Create [fhir_bundle_serialized-class] object
#'
#'Only for internal use
#'
#' @param bundle A serialized xml object representing a FHIR bundle
#'
#' @examples
#' fhir_bundle_serialized(patient_bundles[[1]])
#' @noRd

#constructor
fhir_bundle_serialized <- function(bundle) {
	new(Class = "fhir_bundle_serialized", bundle)
}
