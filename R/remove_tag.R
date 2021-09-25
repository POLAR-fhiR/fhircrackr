#' Remove a certain Tag from a String
#'
#' @param string A character vector containing an xml-strings.
#' @param tag A character of length 1 containing the tag tha should be removed.
#'
#' @return A character vector containing the xml without the tag.
#' @export
#'
#' @examples
#' fhir_remove_tag(c("Hallo<div><p>Remove Me</p></div> World!", "A<div><div><p>B</p></div>C</div>D"), "p")
fhir_remove_tag <- function(string, tag = "div") {
	if(1 < length(string)) return(lapply(string, fhir_remove_tag, tag = tag))
	tOn <- paste0("(<", tag, ")((>)|( +[^/]*?>))")
	tOff <- paste0("</", tag, ">")
	tagOn  <- as.data.table(stringr::str_locate_all(string = string, tOn)[[1]])
	tagOff <- as.data.table(stringr::str_locate_all(string = string, tOff)[[1]])
	tagOn[,(c("type", "id")) := .(1, seq_len(nrow(tagOn)))]
	tagOff[,(c("type", "id")) := .(-1, seq_len(nrow(tagOff)))]
	tags <- rbindlist(list(tagOn, tagOff))
	setorder(tags, start)
	if(0 < nrow(tags)) {
		if(tags$type[1] == -1) {
			stop("Tag closes before it opens.")
		}
		if(1 < tags$start[1]) {
			tags <- rbindlist(list(data.table(start = 1, end = tags$start[1] - 1, type = 0, id = 1), tags))
		}
		if(tags$end[nrow(tags)] < nchar(string)) {
			tags <- rbindlist(list(tags, data.table(start = tags$end[nrow(tags)] + 1, end = nchar(string), type = 0, id = 2)))
		}
		tags[,s:=sapply(seq_len(nrow(tags)),function(x) sum(type[seq_len(x)]))]
		tags[,text:=substr(string, start, end), by = start]
		text <- list()
		for(i in seq_len(nrow(tags))) {
			part <- if(tags$type[i] == 0) {
				#tags$text[i]
				substr(string, tags$start[i], tags$end[i])
			} else if(tags$type[i] == -1 && tags$s[i] == 0 && i < nrow(tags)) {
				substr(string, tags$end[i] + 1, tags$start[i + 1] - 1)
			}
			if(!is.null(part)) {
				if(substr(part, 1, 1) == "\n") {
					part <- if(nchar(part) < 2) "" else substr(part, 2, nchar(part))
				}
				if(part != "\n" && part != ""){
					text <- c(text, part)
				}
			}
		}
		paste0(text, collapse = "")
	} else {
		string
	}
}

#' Remove a certain Tag from a XML-Bundle
#'
#' @param bundle A fhir_bundle_xml object.
#' @param tag A character of length 1 containing the tag that should be removed.
#'
#' @return A fhir_bundle_xml containing xmls without the tag.
#' @export
#'
#' @examples
#' b1 <- fhir_unserialize(patient_bundles)
#' b2 <- fhir_bundle_list(lapply(b1, fhir_remove_tag_from_bundle, tag = "p"))
#' (sb1 <- sum(sapply(b1, function(x)object.size(toString(x)))))
#' (sb2 <- sum(sapply(b2, function(x)object.size(toString(x)))))
#' #div makes about 29 percent of patient bundles
#' 100 * (sb1 - sb2) / sb1
#' cat(toString(b1[[1]]))
#' cat(toString(b2[[1]]))
fhir_remove_tag_from_bundle <- function(bundle, tag = "div") {
	fhircrackr::fhir_bundle_xml(bundle = xml2::read_xml(fhir_remove_tag(string = toString(bundle), tag = tag)))
}

#' Remove a certain Tag from a XML-Bundle
#'
#' @param bundle_list A fhir_bundle_list object containing xml-bundles from where the Tag should be removed.
#' @param tag A character of length 1 containing the Tag to remove.
#'
#' @return A fhir_bundle_list object containing xml-bundles from where the Tag is removed.
#' @export
#'
#' @examples
#' b1 <- fhir_unserialize(patient_bundles)
#' b2 <- fhir_remove_tag_from_bundle_list(b1)
#' (sb1 <- sum(sapply(b1, function(x)object.size(toString(x)))))
#' (sb2 <- sum(sapply(b2, function(x)object.size(toString(x)))))
#' #div makes about 29 percent of patient bundles
#' 100 * (sb1 - sb2) / sb1
#' cat(toString(b1[[1]]))
#' cat(toString(b2[[1]]))
fhir_remove_tag_from_bundle_list <- function(bundle_list, tag = "div") {
	fhir_bundle_list(
		lapply(
			bundle_list,
			function(bundle) {
				fhir_remove_tag_from_bundle(bundle = bundle, tag = tag)
			}
		)
	)
}

#' Remove <div>-Blocks from a String
#'
#' @param string A character vector containing xml.
#'
#' @return A character vector containing xml without <div>-Blocks.
#' @export
#'
#' @examples
#' fhir_remove_div(c("Hallo<div><p>Remove Me</p></div> World!", "A<div><div>B</div>C</div>D"))
fhir_remove_div <- function(string) {
	fhir_remove_tag(string, "div")
}

#' Remove <div>-Blocks from a XML-Bundle
#'
#' @param bundle A fhir_bundle_xml object.
#'
#' @return A fhir_bundle_xml object without <div>-Blocks
#' @export
#'
#' @examples
#' b1 <- fhir_unserialize(patient_bundles)
#' b2 <- fhir_bundle_list(lapply(b1, fhir_remove_div_from_bundle))
#' (sb1 <- sum(sapply(b1, function(x)object.size(toString(x)))))
#' (sb2 <- sum(sapply(b2, function(x)object.size(toString(x)))))
#' #div makes about 29 percent of patient bundles
#' 100 * (sb1 - sb2) / sb1
#' cat(toString(b1[[1]]))
#' cat(toString(b2[[1]]))
fhir_remove_div_from_bundle <- function(bundle) {
	fhir_remove_tag_from_bundle(bundle, "div")
}

fhir_remove_div_from_bundle_list <- function(bundle_list) {
	fhir_remove_tag_from_bundle_list(bundle_list, "div")
}
