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
#'
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
#'   <Patient>
#' 	    <id value='id1'/>
#' 	    <address>
#' 	       <use value='home'/>
#' 	       <city value='Amsterdam'/>
#' 	       <type value='physical'/>
#' 	       <country value='Netherlands'/>
#' 	    </address>
#' 	    <name>
#' 	       <given value='Marie'/>
#' 	    </name>
#' 	 </Patient>
#' 	 <Patient>
#' 	    <id value='id3'/>
#' 	    <address>
#' 	       <use value='home'/>
#' 	       <city value='Berlin'/>
#' 	    </address>
#' 	    <address>
#'         <type value='postal'/>
#' 	       <country value='France'/>
#' 	    </address>
#' 	    <address>
#' 	       <use value='work'/>
#' 	       <city value='London'/>
#' 	       <type value='postal'/>
#' 	       <country value='England'/>
#' 	    </address>
#' 	    <name>
#' 	       <given value='Frank'/>
#' 	    </name>
#' 	    <name>
#'	       <given value='Max'/>
#' 	    </name>
#'   </Patient>
#' </Bundle>
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
#'
#' @source
#' **example_bundles2**
#'
#'```
#'<Bundle>
#' 	<Patient>
#' 		<id value='id1'/>
#' 		<address>
#' 			<use value='home'/>
#' 			<city value='Amsterdam'/>
#' 			<type value='physical'/>
#' 			<country value='Netherlands'/>
#' 		</address>
#' 		<name>
#'			<given value='Marie'/>
#' 		</name>
#' 	</Patient>
#'
#' 	<Patient>
#' 		<id value='id2'/>
#' 		<address>
#' 			<use value='home'/>
#' 			<city value='Rome'/>
#' 			<type value='physical'/>
#' 			<country value='Italy'/>
#' 		</address>
#' 		<address>
#' 			<use value='work'/>
#' 			<city value='Stockholm'/>
#' 			<type value='postal'/>
#' 			<country value='Sweden'/>
#' 		</address>
#' 		<name>
#' 			<given value='Susie'/>
#' 		</name>
#' 	</Patient>
#'
#' 	<Patient>
#' 		<id value='id3'/>
#' 		<address>
#' 			<use value='home'/>
#' 			<city value='Berlin'/>
#' 		</address>
#' 		<address>
#' 			<type value='postal'/>
#' 			<country value='France'/>
#' 		</address>
#' 		<address>
#' 			<use value='work'/>
#' 			<city value='London'/>
#' 			<type value='postal'/>
#' 			<country value='England'/>
#' 		</address>
#' 		<name>
#' 			<given value='Frank'/>
#' 		</name>
#' 		<name>
#' 			<given value='Max'/>
#' 		</name>
#' 	</Patient>
#'
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
#'
#' @source
#' **example_bundles3**
#'
#'```
#' <Bundle>
#'
#' 	<Patient>
#' 		<id value='id1'/>
#' 		<address>
#' 			<use value='home'/>
#' 			<city value='Amsterdam'/>
#' 			<type value='physical'/>
#' 			<country value='Netherlands'/>
#' 		</address>
#' 		<name>
#'			<given value='Marie'/>
#' 		</name>
#' 	</Patient>
#'
#' 	<Patient>
#' 		<id value='id2'/>
#' 		<address>
#' 			<use value='home'/>
#' 			<city value='Rome'/>
#' 			<type value='physical'/>
#' 			<country value='Italy'/>
#' 		</address>
#' 		<address>
#' 			<use value='work'/>
#' 			<city value='Stockholm'/>
#' 			<type value='postal'/>
#' 			<country value='Sweden'/>
#' 		</address>
#' 		<name>
#' 			<given value='Susie'/>
#' 		</name>
#' 	</Patient>
#'
#' 	<Patient>
#' 		<id value='id3'/>
#' 		<address>
#' 			<use value='home'/>
#' 			<city value='Berlin'/>
#' 		</address>
#' 		<address>
#' 			<type value='postal'/>
#' 			<country value='France'/>
#' 		</address>
#' 		<address>
#' 			<use value='work'/>
#' 			<city value='London'/>
#' 			<type value='postal'/>
#' 			<country value='England'/>
#' 		</address>
#' 		<name>
#' 			<given value='Frank'/>
#' 		</name>
#' 		<name>
#' 			<given value='Max'/>
#' 		</name>
#' 	</Patient>
#'
#' 	<Observation>
#' 	   <id value = '1'/>
#'	   <code>
#'        <coding>
#'		     <system value='http://loinc.org'/>
#'		     <code value='29463-7'/>
#'		     <display value='Body Weight'/>
#'		  </coding>
#'	      <coding>
#'		     <system value='http://snomed.info/sct'/>
#'		     <code value='27113001'/>
#'		     <display value='Body weight'/>
#'	      </coding>
#'	   </code>
#' 	 </Observation>
#'
#' 	</Bundle>"
#'
#'```
#'

"example_bundles3"
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
#'
#' @noRd
#' @examples
#' fhircrackr:::lst(letters[1:5], prefix="--", suffix="+")
lst <- function(..., prefix = NULL, suffix = NULL) {
	v <- as.list(c(...))
	names(v) <- paste0(prefix, v, suffix)
	v
}


#' Escape special characters
#' @param s A a character vector of length one in which the characters should be escaped.
#' @return A a character vector of length one with all special characters escaped.
#' @example esc(s = c("(",")"))
#' @noRd
#'
esc <- function(s) {
	gsub("([\\.|\\^|\\$|\\*|\\+|\\?|\\(|\\)|\\[|\\{|\\\\\\|\\|])", "\\\\\\1", s)
}
