## This file contains all functions that didn't fit any category##
## Exported functions are on top, internal functions below ##

#environment variables
fhircrackr_env <- new.env(parent = emptyenv())
assign(x = "last_next_link", value = new(Class = "fhir_url"), envir = fhircrackr_env)
assign(x = "canonical_design", value = NULL, envir = fhircrackr_env)
assign(x = "current_request", value = new(Class = "fhir_url"), envir = fhircrackr_env)
assign(x = "recent_http_error", value = NULL, envir= fhircrackr_env)

#imports
#' @import data.table
#' @import methods

#to ensure data.table version of d[] is called, even though it is not explicitly stated in
#import section of NAMESPACE file (https://cran.r-project.org/web/packages/data.table/vignettes/datatable-importing.html)
.datatable.aware = TRUE

#To stop devtools::check( from warning about no visible global function definition)
globalVariables(".")

#' Remove a certain xml tag
#'
#' Removes a given xml tag from xml objects represented in a [fhir_bundle_xml-class], [fhir_bundle_list-class]
#' or character vector.
#'
#' In the example `Hello<div>Please<p>Remove Me</p></div>World!` one could for example remove the
#' tag `p`, resulting in `Hello<div>Please</div>World!`or remove the `div` tag resulting in
#' `Hello World!`.
#'
#' @param x A [fhir_bundle_xml-class] or [fhir_bundle_list-class] object or a character vector
#' containing xml objects.
#' @param tag A character vector of length 1 containing the tag that should be removed, e.g. `"div"`.
#'
#' @return An object of the same class as `x` where all tags matching the `tag` argument are removed.
#' @export
#' @rdname fhir_rm_tag-methods
#' @docType methods
#' @include fhir_bundle.R fhir_bundle_list.R
#' @seealso [fhir_rm_div()]
#'
#' @examples
#'
#' #Example 1: Remove tag from xmls in a character vector
#' string <- c("Hello<div>Please<p>Remove Me</p></div> World!",
#'             "A<div><div><p>B</p></div>C</div>D")
#'
#' fhir_rm_tag(x = string, tag = "p")
#'
#'
#'
#'
#' #Example 2: Remove div tags in a single fhir bundle
#' bundle <- fhir_unserialize(patient_bundles)[[1]]
#'
#' #example bundle contains html parts in div tags:
#' cat(toString(bundle))
#'
#' #remove html parts
#' bundle_cleaned <- fhir_rm_tag(x = bundle, tag = "div")
#'
#' #have a look at the result
#' cat(toString(bundle_cleaned))
#'
#'
#'
#'
#'
#' #Example 3: Remove div tags in a list of fhir bundles
#' bundle_list <- fhir_unserialize(patient_bundles)
#'
#'
#' #remove html parts
#' bundle_list_cleaned <- fhir_rm_tag(x = bundle_list, tag = "div")
#'
#' #check out how much the size of the bundle list is reduced by removing html
#' size_with_html <- sum(sapply(bundle_list, function(x)object.size(toString(x))))
#' size_without_html <- sum(sapply(bundle_list_cleaned, function(x)object.size(toString(x))))
#'
#' size_without_html/size_with_html
#'
setGeneric(
	name = "fhir_rm_tag",
	def = function(
		x,
		tag
	) {
		standardGeneric("fhir_rm_tag")
	}
)

#' @rdname fhir_rm_tag-methods
#' @aliases fhir_rm_tag,character-method

setMethod(
	f = "fhir_rm_tag",
	signature = c(x = "character"),
	definition = function(
		x,
		tag
	) {
		if(1 < length(x)) {
			return(sapply(x, fhir_rm_tag, tag = tag, USE.NAMES = F))
		}
		start <- end <- s <- type <- NULL #just to shut up CRAN check about undefined global variables
		tOn <- paste0("(<", tag, ")((>)|( +[^/]*?>))")
		tOff <- paste0("</", tag, ">")
		tagOn  <- as.data.table(stringr::str_locate_all(string = x, tOn)[[1]])
		tagOff <- as.data.table(stringr::str_locate_all(string = x, tOff)[[1]])
		tagOn[,(c("type", "id")) := .(1, seq_len(nrow(tagOn)))]
		tagOff[,(c("type", "id")) := .(-1, seq_len(nrow(tagOff)))]
		tags <- rbindlist(list(tagOn, tagOff))
		data.table::setorder(tags, start)
		if(0 < nrow(tags)) {
			if(tags$type[1] == -1) {
				stop("Tag closes before it opens.")
			}
			if(1 < tags$start[1]) {
				tags <- data.table::rbindlist(list(data.table(start = 1, end = tags$start[1] - 1, type = 0, id = 1), tags))
			}
			if(tags$end[nrow(tags)] < nchar(x)) {
				tags <- data.table::rbindlist(list(tags, data.table(start = tags$end[nrow(tags)] + 1, end = nchar(x), type = 0, id = 2)))
			}
			tags[,s:=sapply(seq_len(nrow(tags)),function(x) sum(type[seq_len(x)]))]
			tags[,text:=substr(x, start, end), by = start]
			text <- list()
			for(i in seq_len(nrow(tags))) {
				part <- if(tags$type[i] == 0) {
					#tags$text[i]
					substr(x, tags$start[i], tags$end[i])
				} else if(tags$type[i] == -1 && tags$s[i] == 0 && i < nrow(tags)) {
					substr(x, tags$end[i] + 1, tags$start[i + 1] - 1)
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
			x
		}
	}
)


#' @rdname fhir_rm_tag-methods
#' @aliases fhir_rm_tag,fhir_bundle_xml-method

setMethod(
	f = "fhir_rm_tag",
	signature = c(x = "fhir_bundle_xml"),
	definition = function(
		x,
		tag
	) {
		fhir_bundle_xml(bundle = xml2::read_xml(fhir_rm_tag(x = toString(x), tag = tag)))
	}
)

#' @rdname fhir_rm_tag-methods
#' @aliases fhir_rm_tag,fhir_bundle_list-method

setMethod(
	f = "fhir_rm_tag",
	signature = c(x = "fhir_bundle_list"),
	definition = function(
		x,
		tag
	) {
		fhir_bundle_list(
			lapply(
				x,
				function(bundle) {
					fhir_rm_tag(x = bundle, tag = tag)
				}
			)
		)
	}
)

#' Remove html elements
#'
#' This function is a convenience wrapper for [fhir_rm_tag()] that removes all `<div> </div>` elements from an xml.
#' `div` tags in FHIR resources contain html code, which is often server generated and in most cases neither relevant nor
#' usable for data analysis.
#'
#' @param x A [fhir_bundle_xml-class] or [fhir_bundle_list-class] object or a character vector
#' containing xml objects.
#'
#' @return An object of the same class as `x` where all tags matching the `tag` argument are removed.
#' @export
#' @include fhir_bundle.R fhir_bundle_list.R
#' @seealso [fhir_rm_tag()]
#'
#' @examples
#'
#' #Example 1: Remove div tags from xmls in a character vector
#' string <- c("Hallo<div>Please<p>Remove Me</p></div> World!",
#'             "A<div><div><p>B</p></div>C</div>D")
#'
#' fhir_rm_div(x = string)
#'
#'
#'
#'
#' #Example 2: Remove div tags in a single fhir bundle
#' bundle <- fhir_unserialize(patient_bundles)[[1]]
#'
#' #example bundle contains html parts in div tags:
#' cat(toString(bundle))
#'
#' #remove html parts
#' bundle_cleaned <- fhir_rm_div(x = bundle)
#'
#' #have a look at the result
#' cat(toString(bundle_cleaned))
#'
#'
#'
#'
#'
#' #Example 3: Remove div tags in a list of fhir bundles
#' bundle_list <- fhir_unserialize(patient_bundles)
#'
#'
#' #remove html parts
#' bundle_list_cleaned <- fhir_rm_div(x = bundle_list)
#'
#' #check out how much the size of the bundle list is reduced by removing html
#' size_with_html <- sum(sapply(bundle_list, function(x)object.size(toString(x))))
#' size_without_html <- sum(sapply(bundle_list_cleaned, function(x)object.size(toString(x))))
#'
#' size_without_html/size_with_html
#'

fhir_rm_div <- function(x){
	fhir_rm_tag(x, tag = "div")
}




#' Concatenate paths
#' @description Concatenates two strings to a path string correctly.
#'
#' @param path1 A a character vector of length one specifying the left hand part of the resulting path.
#' @param path2 A a character vector of length one specifying the right hand part of the resulting path.
#' @param os A a character vector of length one specifying the operating system you're operating on: windows or linux.
#'
#' @return A a character vector of length one containing the concatenated path.
#' @export
#'
#' @examples
#' paste_paths(path1 = "data", path2 = "patients")
#' paste_paths(path1 = "/data", path2 = "patients")
#' paste_paths(path1 = "/data/", path2 = "patients")
#' paste_paths(path1 = "/data", path2 = "/patients")
#' paste_paths(path1 = "/data/", path2 = "/patients/")
#' paste_paths(path1 = "data", path2 = "patients", os = "windows")

paste_paths <- function(path1 = "w", path2 = "d", os = "LiNuX") {
	os <- tolower(substr(os, 1, 1))
	if(os == "w") {
		return(
			paste0(
				sub(pattern = "\\\\$" , replacement = "", x = path1),
				"\\",
				sub(pattern = "^\\\\", replacement = "", x = path2)
			)
		)
	}
	paste0(sub(pattern = "/$" , replacement = "", x = path1), "/", sub(pattern = "^/", replacement = "", x = path2))
}


fhir_ns_strip <- function(xml) {
	# cat("\nis(xml):\n")
	# print(is(xml))
	# (xml2::xml_ns_strip(xml))
	# cat("\nis(xml2):\n")
	# print(is(xml))
	# return(xml)
	xml_2 <- if(inherits(xml, "fhir_bundle_list")) {
		fhir_bundle_list(
			lapply(
				xml,
				function(bundle) {
					xml2::read_xml(gsub(" xmlns=\"[^\"]+\"", "", toString(bundle)))
				}
			)
		)
	}
	else if(inherits(xml, "fhir_bundle_xml")) {
		fhir_bundle_xml(xml2::read_xml(gsub(" xmlns=\"[^\"]+\"", "", toString(xml))))
	} else {
		xml2::read_xml(gsub(" xmlns=\"[^\"]+\"", "", toString(xml)))
	}

	# cat("\nis(xml):\n")
	# print(is(xml))
	# cat("\nis(xml2):\n")
	# print(is(xml_2))
	xml_2
}

##### Documentation for medication_bundles data set ######

#' Exemplary FHIR bundles
#'
#' @description
#' These data examples can be used to explore some of the functions from the fhircrackr package
#' when direct access to a FHIR server is not possible.
#'
#' All example data sets are [fhir_bundle_list-class]s containing [fhir_bundle_serialized-class] objects
#' representing FHIR bundles as returned by [fhir_search()]. They have to be unserialized (once per R session),
#' before you can work with them!
#'
#' @details
#' `medication_bundles` contains 3 bundles with MedicationStatement resources representing Medications with Snomed CT code
#' 429374003 and the respective Patient resources that are linked to these MedicationStatements.
#'
#' @examples
#' #unserialize xml objects before doing anything else with them!
#' fhir_unserialize(bundles = medication_bundles)
#'
#' @rdname datasets_real
#'
#' @source The data sets are generated by the following code:
#'
#' **medication_bundles** (*Downloaded 10-05-21*)
#'
#' ```
#' search_request  <- fhir_url(url = "https://hapi.fhir.org/baseR4",
#' 							resource = "MedicationStatement",
#' 							parameters = c("code" = "http://snomed.info/ct|429374003",
#' 										   "_include" = "MedicationStatement:subject"))
#'
#' bundles <- fhir_search(request = search_request, max_bundles = 3)
#'
#' medication_bundles <- fhir_serialize(bundles = bundles)
#' ```
#'
#'
#'

"medication_bundles"

##### Documentation for patient_bundles data set ######

#' @details
#' `patient_bundles` contains 2 bundles with Patient resources.
#'
#' @examples
#' #unserialize xml objects before doing anything else with them!
#' fhir_unserialize(bundles = patient_bundles)
#' @aliases patient_bundles
#' @rdname datasets_real
#' @source
#' **patient_bundles** (*Downloaded 10-05-21*)
#'
#' ```
#' bundles <- fhir_search(request="https://hapi.fhir.org/baseR4/Patient",
#'                        max_bundles=2,
#'                        verbose = 0)
#'
#' patient_bundles <- fhir_serialize(bundles = bundles)
#' ```
#'
#'
#'

"patient_bundles"

##### Documentation for example_bundles1 data set ######
#' Toy example bundles for mulitple entries
#'
#' @description These data examples are bundles that contain very few very simple Patient resources
#' that have multiple entries and can be used for demonstration purposes. See **Source** for how the
#' xml versions look.
#'
#' @details
#' `example_bundles1` contains 1 bundle with 2 Patient resources.
#'
#' @examples
#' #unserialize xml objects before doing anything else with them!
#' fhir_unserialize(bundles = example_bundles1)
#' @rdname datasets_selfmade
#'
#' @source
#' **example_bundles1**
#'
#'```
#'<Bundle>
#'  <type value='searchset'/>
#'  <entry>
#'     <Patient>
#'        <id value='id1'/>
#' 	      <address>
#' 	         <use value='home'/>
#' 	         <city value='Amsterdam'/>
#' 	         <type value='physical'/>
#' 	         <country value='Netherlands'/>
#' 	      </address>
#' 	      <name>
#' 	         <given value='Marie'/>
#' 	      </name>
#'     </Patient>
#'  </entry>
#'
#'  <entry>
#'     <Patient>
#'        <id value='id3'/>
#' 	      <address>
#' 	         <use value='home'/>
#' 	         <city value='Berlin'/>
#' 	      </address>
#'        <address>
#'           <type value='postal'/>
#'           <country value='France'/>
#'        </address>
#' 	      <address>
#' 	         <use value='work'/>
#' 	         <city value='London'/>
#' 	         <type value='postal'/>
#' 	         <country value='England'/>
#' 	      </address>
#'        <name>
#'           <given value='Frank'/>
#'        </name>
#'        <name>
#'           <given value='Max'/>
#'        </name>
#'     </Patient>
#'  </entry>
#'</Bundle>
#'```
#'

"example_bundles1"


##### Documentation for example_bundles2 data set ######
#' @details
#' `example_bundles2` contains 1 bundle with 3 Patient resources.
#'
#' @examples
#' #unserialize xml objects before doing anything else with them!
#' fhir_unserialize(example_bundles2)
#' @rdname datasets_selfmade
#' @source
#' **example_bundles2**
#'
#'```
#'<Bundle>
#'  <type value='searchset'/>
#'  <entry>
#'      <Patient>
#'         <id value='id1'/>
#'         <address>
#'            <use value='home'/>
#'            <city value='Amsterdam'/>
#'            <type value='physical'/>
#'            <country value='Netherlands'/>
#'         </address>
#'         <name>
#'            <given value='Marie'/>
#'         </name>
#'      </Patient>
#'  </entry>
#'
#'  <entry>
#'     <Patient>
#'         <id value='id2'/>
#'         <address>
#'            <use value='home'/>
#'            <city value='Rome'/>
#'            <type value='physical'/>
#'            <country value='Italy'/>
#'         </address>
#'         <address>
#'            <use value='work'/>
#'            <city value='Stockholm'/>
#' 	          <type value='postal'/>
#'            <country value='Sweden'/>
#'         </address>
#'         <name>
#'            <given value='Susie'/>
#'        </name>
#'     </Patient>
#'  </entry>
#'
#'  <entry>
#'     <Patient>
#'        <id value='id3'/>
#'        <address>
#'           <use value='home'/>
#'           <city value='Berlin'/>
#'        </address>
#'        <address>
#'           <type value='postal'/>
#'           <country value='France'/>
#'        </address>
#'        <address>
#'           <use value='work'/>
#'           <city value='London'/>
#'           <type value='postal'/>
#'           <country value='England'/>
#'        </address>
#'        <name>
#'           <given value='Frank'/>
#'       </name>
#'       <name>
#'           <given value='Max'/>
#'        </name>
#'     </Patient>
#'  </entry>
#'</Bundle>
#'```
#'

"example_bundles2"

##### Documentation for example_bundles3 data set ######
#' @details
#' `example_bundles3` contains 1 bundle with 3 Patient resources and 1 Observation resource.
#'
#' @examples
#' #unserialize xml objects before doing anything else with them!
#' fhir_unserialize(bundles = example_bundles2)
#' @rdname datasets_selfmade
#' @source
#' **example_bundles3**
#'
#'```
#' <Bundle>
#'  <type value='searchset'/>
#'  <entry>
#'     <Patient>
#'        <id value='id1'/>
#'        <address>
#'           <use value='home'/>
#'           <city value='Amsterdam'/>
#'           <type value='physical'/>
#'           <country value='Netherlands'/>
#'        </address>
#'        <name>
#'           <given value='Marie'/>
#'        </name>
#'     </Patient>
#'  </entry>
#'
#'  <entry>
#'     <Patient>
#'        <id value='id2'/>
#'        <address>
#'           <use value='home'/>
#'           <city value='Rome'/>
#'           <type value='physical'/>
#'           <country value='Italy'/>
#'        </address>
#'        <address>
#'           <use value='work'/>
#'           <city value='Stockholm'/>
#'           <type value='postal'/>
#'           <country value='Sweden'/>
#'        </address>
#'        <name>
#'           <given value='Susie'/>
#'        </name>
#'     </Patient>
#'  </entry>
#'
#'  <entry>
#'     <Patient>
#'        <id value='id3'/>
#'        <address>
#'           <use value='home'/>
#'           <city value='Berlin'/>
#'        </address>
#'        <address>
#'           <type value='postal'/>
#'           <country value='France'/>
#'        </address>
#'        <address>
#'           <use value='work'/>
#'           <city value='London'/>
#'           <type value='postal'/>
#'           <country value='England'/>
#'        </address>
#'        <name>
#'           <given value='Frank'/>
#'        </name>
#'        <name>
#'           <given value='Max'/>
#'        </name>
#'     </Patient>
#'  </entry>
#'
#'  <entry>
#'     <Observation>
#'        <id value = 'obs1'/>
#'        <code>
#'           <coding>
#'              <system value='http://loinc.org'/>
#'              <code value='29463-7'/>
#'              <display value='Body Weight'/>
#'           </coding>
#'           <coding>
#'              <system value='http://snomed.info/sct'/>
#'              <code value='27113001'/>
#'              <display value='Body weight'/>
#'           </coding>
#'        </code>
#'        <subject>
#'           <reference value='Patient/id3'/>
#'        </subject>
#'     </Observation>
#'  </entry>
#' </Bundle>"
#'
#'```
#'

"example_bundles3"


##### Documentation for transaction_bundle_example data set ######
#' Toy examples to POST/PUT on a server
#'
#' @description These data examples are simple examples to try out POSTing/PUTing resources
#' to a server. See **Source** for how the xml versions look.
#'
#' @details
#' `transaction_bundle_example` contains 1 transaction bundle with 2 Patient resources.
#'
#' @examples
#' #unserialize xml objects before doing anything else with them!
#' fhir_unserialize(bundles = transaction_bundle_example)
#' @rdname datasets_for_post
#'
#' @source
#' **transaction_bundle_example**
#'
#'```
#' <Bundle>
#'    <type value='transaction'/>
#' 	  <entry>
#' 	     <resource>
#' 	        <Patient>
#' 	           <id value='id1'/>
#' 	           <address>
#' 	              <use value='home'/>
#' 	              <city value='Amsterdam'/>
#' 	              <type value='physical'/>
#' 	              <country value='Netherlands'/>
#' 	           </address>
#' 	           <name>
#' 	              <given value='Marie'/>
#' 	           </name>
#' 	        </Patient>
#' 	     </resource>
#' 	     <request>
#' 	        <method value='POST'/>
#' 	        <url value='Patient'/>
#' 	     </request>
#' 	  </entry>
#' 	  <entry>
#' 	      <resource>
#' 	         <Patient>
#' 	            <id value='id3'/>
#' 	            <address>
#' 	               <use value='home'/>
#' 	               <city value='Berlin'/>
#' 	            </address>
#' 	            <address>
#' 	               <use value='work'/>
#' 	               <city value='London'/>
#' 	               <type value='postal'/>
#' 	               <country value='England'/>
#'              </address>
#' 	            <address>
#' 	               <type value='postal'/>
#' 	               <country value='France'/>
#' 	            </address>
#' 	            <name>
#' 	               <given value='Frank'/>
#' 	            </name>
#' 	            <name>
#' 	               <given value='Max'/>
#' 	            </name>
#' 	        </Patient>
#' 	     </resource>
#' 	     <request>
#' 	        <method value='POST'/>
#' 	        <url value='Patient'/>
#' 	     </request>
#' 	  </entry>
#' </Bundle>
#'```
#'

"transaction_bundle_example"

##### Documentation for example_resource1 ######
#' Toy examples to POST on a server
#'
#' @details
#' `example_resource1` contains 1 patient resource without id for POSTing
#'
#' @examples
#' #unserialize xml objects before doing anything else with them!
#' fhir_unserialize(example_resource1)

#' @rdname datasets_for_post
#' @source
#' **example_resource1**
#'
#'```
#' <Patient>
#'    <name>
#' 	     <given value = 'Marie'/>
#' 	  </name>
#' 	  <gender value = 'female'/>
#' 	  <birthDate value = '1970-01-01'/>
#' </Patient>
#'```
#'

"example_resource1"

##### Documentation for example_resource2 ######
#' Toy examples to POST on a server
#'
#' @details
#' `example_resource2` contains 1 patient resource with id for PUTing
#'
#' @examples
#' #unserialize xml objects before doing anything else with them!
#' fhir_unserialize(example_resource2)

#' @rdname datasets_for_post
#' @source
#' **example_resource2**
#'
#'```
#' <Patient>
#'    <id value = '1a2b3c'/>
#'    <name>
#' 	     <given value = 'Marie'/>
#' 	  </name>
#' 	  <gender value = 'female'/>
#' 	  <birthDate value = '1970-01-01'/>
#' </Patient>
#'```
#'

"example_resource2"


#################################################################################
#################################################################################
#' Transform vector to named list
#' @description Transforms a vector of items to a named list. The names are created with a prefix and a suffix surrounding the items.
#'
#' @param ... A vector that can be coerced to a character.
#' @param prefix A a character vector of length one taken as the prefix for the names of the list elements.
#' @param suffix A a character vector of length one taken as the suffix for the names of the list elements.
#'
#' @return A named list, where the names are the content surrounded by a prefix and a suffix.
#' @noRd
#' @examples
#' fhircrackr:::lst(letters[1:5], prefix="--", suffix="+")
lst <- function(..., prefix = NULL, suffix = NULL) {
	v <- as.list(c(...))
	names(v) <- paste0(prefix, v, suffix)
	v
}


#' Escape special characters
#' @param s A character vector in which the characters should be escaped.
#' @return A character vector with all special characters escaped.
#' @example esc(s = c("(",")"))
#' @noRd
#'
esc <- function(s) {
	gsub("([\\.|\\^|\\$|\\*|\\+|\\?|\\(|\\)|\\[|\\{|\\\\\\|\\|])", "\\\\\\1", s)
}

## determine operating system
get_os <- function() {
	sysinf <- Sys.info()
	if (!is.null(sysinf)) {
		os <- sysinf[['sysname']]
		if (os == 'Darwin') os <- "osx"
	} else { ## mystery machine
		os <- .Platform$OS.type
		if (grepl("^darwin", R.version$os))
			os <- "osx"
		if (grepl("linux-gnu", R.version$os))
			os <- "linux"
	}
	tolower(os)
}

get_ncores <- function(os) {
	if(os %in% c("linux", "osx")) parallel::detectCores() else 1
}

#' Escape characters reserved in xml
#' #' @param s A character vector in which the characters should be escaped.
#' @return A character vector of with all special characters escaped.
#' @example esc_xml(s = c("<","&"))
#' @noRd
#'
esc_xml <- function(s) {
	gsub("\"", "&quot;",
		 gsub("'", "&apos;",
		 	 gsub("<", "&lt;",
		 	 	 gsub(">", "&gt;",
		 	 	 	 gsub("&", "&amp;", s, fixed = TRUE), fixed = TRUE
		 	 	 ), fixed = TRUE
		 	 ), fixed = TRUE
		 ), fixed = TRUE
	)
}


#' Unescape characters reserved in xml
#' @param s A character vector of with all special characters escaped.
#' @return The unescaped version
#' @example esc_xml(s = c("&quot;","&amp;"))
#' @noRd
#'
#'
desc_xml <- function(s) {
	gsub("&quot;", "\"",
		 gsub("&apos;", "'",
		 	 gsub("&lt;", "<",
		 	 	 gsub("&gt;", ">",
		 	 	 	 gsub("&amp;", "&", s, fixed = TRUE), fixed = TRUE
		 	 	 ), fixed = TRUE
		 	 ), fixed = TRUE
		 ), fixed = TRUE
	)
}

#' Increment tab
#' @param tab A character vector with strings to increment
#' @param add The string to use for incrementation
#' @return The incremented tab
#' @noRd
inc_tab <- function(tab, add = "....") {paste0(tab, add)}

#' Decrement tab
#' @param tab A character vector with strings to dencrement
#' @param add The string to remove
#' @return The decremented tab
#' @noRd
dec_tab <- function(tab, sub = "....") {substr(tab, 1, nchar(tab) - nchar(sub))}


#' Create value list
#' This function creates a list that has values as attributes.
#' Can be used to create trees that are digesteable by xml2.
#' @param .value The value for the attribure
#' @param ... other vlists
#' @return A value list
#' @noRd
vlist <- function(.value, ...) {
	list <- list(...)
	attr(list, "value") <- .value
	list
}

#' Wrap string in frame
#' Creates a string that is wrapped in a frame for nicer printing.
#' Has to be used with `cat()`
#' @param text The string to wrap
#' @param pos Horizontal alignment within the frame
#' @param edge Single character which will be the corner of the frame
#' @param hori String making up the horizontal edge of the frame
#' @param vert String making up the vertical edge of the frame
#' @return A String ready for `cat()`
#' @examples
#' cat(frame_string(text="Some\ntest\n       text", pos="right", edge="*"))
#' @noRd
frame_string <- function(text = "\nHello !!!\n\n\nIs\nthere\n\nA N Y O N E\n\nout\nthere\n???\n ",
						 pos = c("left", "center", "right")[1],
						 edge = " ",
						 hori = "-",
						 vert = "|") {

	strpad <- function(string, width, pos = c("left", "right"), pad) {
		n_chars <- function(char, count) paste0(rep_len(char, count), collapse = "")
		w <- nchar(string)
		if(pos == "left") {
			paste0(string, n_chars(pad, width - w))
		} else if(pos == "right") {
			paste0(n_chars(pad, width - w), string)
		} else {
			paste0(n_chars(pad, (width - w) %/% 2), string, n_chars(pad, width - w - (width - w) %/% 2))
		}
	}
	edge <- rep_len(strsplit(edge, "")[[1]], 4)[1 : 4]
	r <- ""
	s <- strsplit(text, "\n")[[1]]
	h <- length(s)
	w <- max(sapply(s, nchar))
	hbt <- paste0(edge[1], paste0(rep_len(hori, w + 2), collapse = ""), edge[2], "\n")
	hbb <- paste0(edge[3], paste0(rep_len(hori, w + 2), collapse = ""), edge[4], "\n")
	r <- hbt
	for(s_ in s) {
		r <- paste0(r, vert, " ", strpad(string = s_, width = w, pos = pos, pad = " "), " ", vert, "\n")
	}
	r <- paste0(r, hbb)
	r
}


#' Prepare authentication info for fhir_search/fhir_post/fhir_put
#' @param username The username for basic auth
#' @param password The password for basic auth
#' @param token The token for token based auth
#' @noRd
auth_helper <- function(username, password, token){

	#prepare token authorization
	if(!is.null(token)) {
		if(!is.null(username) || !is.null(password)) {
			warning(
				"You provided username or password as well as a token for authentication.\n",
				"Ignoring username and password, trying to authorize with token."
			)
			username <- NULL
			password <- NULL
		}

		if(is(token, "Token")) {
			token <- token$credentials$access_token
		}
		if(1 < length(token)) {stop("token must be of length one.")}
			bearerToken <- paste0("Bearer ", token)
		} else {
			bearerToken <- NULL
		}

	if(!is.null(username) && !is.null(password)) {
		auth <- httr::authenticate(user = username, password = password)
	}else{
		auth <- NULL
	}


	list(basicAuth = auth, token = bearerToken)
}
