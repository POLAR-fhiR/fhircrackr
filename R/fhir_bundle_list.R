
#' S4 class to represent a list of FHIR bundles
#'
#' A fhir_bundle_list is a list of fhir_bundle_xml or fhir_bundle_serialized objects. It should
#'  not be created by the user but returned by a call to [fhir_search()].
#'
#' @include fhir_bundle.R
#' @export
#'
setClass(
	Class = "fhir_bundle_list",
	contains = "list"
)

setValidity(
	Class = "fhir_bundle_list",
	method = function(object) {

		messages <- c()

		if(any(!sapply(object, is, "fhir_bundle"))) {
			messages <- c(messages, "All elements of a fhir_bundle_list must be fhir bundles." )
		}

		if(1 < length(unique(sapply(object, class)))) {
			messages <- c(messages, "You cannot mix bundle types in a fhir_bundle_list.")
		}

		if(0 < length(messages)) {messages} else {TRUE}
	}
)

#' Create [fhir_bundle_list-class] object
#'
#' A fhir_bundle_list is a list of fhir_bundle_xml or fhir_bundle_serialized objects. It is
#' usually returned by a call to [fhir_search()].
#'
#' The only scenario where one would use this constructor function is when several [fhir_bundle-class]
#' or [fhir_bundle_list-class] objects should be merged into one big [fhir_bundle_list-class] before cracking (see examples).
#'
#' @param bundles A list of xml_nodes/fhir_bundle_xml objects or of raw/fhir_bundle_serialized objects
#' @export
#' @examples
#'
#' #unserialize example bundles
#' bundles1 <- fhir_unserialize(example_bundles1)
#' bundles2 <- fhir_unserialize(example_bundles2)
#'
#' #bind them together in one fhir_bundle_list
#' bound_bundles <- fhir_bundle_list(c(bundles1, bundles2))
#' class(bound_bundles)
#'
#' #bound list contains bundles from both original lists
#' length(bundles1)
#' length(bundles2)
#' length(bound_bundles)
#'
#'
#'
#' #Create fhir_bundle list from xml objects
#' b1 <- xml2::read_xml("<Bundle><Resource><item value='1'/></Resource></Bundle>")
#' b2 <- xml2::read_xml("<Bundle><Resource><item value='2'/></Resource></Bundle>")
#'
#' fhir_bundle_list(bundles = list(b1, b2))
#' fhir_bundle_list(bundles = list(fhir_bundle_xml(b1), fhir_bundle_xml(b2)))
#'
#' r1 <- xml2::xml_serialize(object = b1, connection= NULL)
#' r2 <- xml2::xml_serialize(object = b2, connection= NULL)
#'
#' fhir_bundle_list(bundles = list(r1, r2))

fhir_bundle_list <- function(bundles) {

	if(!is.list(bundles)) {
		stop("You have to provide a list to fhir_bundle_list()")
	}

	if(all(sapply(bundles, is, "raw"))) {
		bundles <- lapply(bundles, fhir_bundle_serialized)
	} else if (all(sapply(bundles, is, "xml_node"))) {
		bundles <- lapply(bundles, fhir_bundle_xml)
	} else {
		stop(
			"The bundles you provide must all be of the same type,",
			" either xml_node/fhir_bundle_xml or raw/fhir_bundle_serialized"
		)
	}

	new(Class = "fhir_bundle_list", bundles)
}
