## This file contains all functions for downloading/loading/saving resources##
## Exported functions are on top, internal functions below ##



#' Download FHIR search result
#' @description Downloads all FHIR bundles of a FHIR search request from a FHIR server.
#'
#' @param request A string containing the full FHIR search request. Defaults to \code{\link{fhir_current_request}}
#' @param username A string containing the username for basic authentication. Defaults to NULL, meaning no authentication.
#' @param password A string containing the password for basic authentication. Defaults to NULL, meaning no authentication.
#' @param max_bundles Maximal number of bundles to get. Defaults to Inf meaning all available bundles are downloaded.
#' @param verbose An Integer Scalar.  If 0, nothings is printed, if 1, only finishing message is printed, if > 1,
#' downloading progress will be printed. Defaults to 2.
#' @param max_attempts A numeric scalar. The maximal number of attempts to send a request, defaults to 5.
#' @param delay_between_attempts A numeric scalar specifying the delay in seconds between two attempts. Defaults to 10.
#' @param log_errors Takes values 0, 1 or 2. Controls the logging of errors. 1 and 2 will write a file to the current working directory.
#'
#' 0: no logging of errors,
#'
#' 1: tabulate http response and write to csv-file
#'
#' 2: write http response as to xml-file
#'
#' @param save_to_disc A logical scalar. If TRUE the bundles are saved as numerated xml-files into the directory specified
#' in the argument \code{directory} and not returned as a bundle list in the R session. This is useful when a lot of
#' bundles are to be downloaded and keeping them all in one R session might overburden working memory. When download
#' is complete, the bundles can be loaded into R using \code{\link{fhir_load}}. Defaults to FALSE, i.e. bundles are
#' returned as a list within the R session.
#'
#' @param directory The directory the bundles are saved to when \code{save_to_disc} is TRUE. Defaults to creating a
#' time-stamped directory into the current working directory.
#'
#' @param delay_between_pages A numeric scalar specifying a time in seconds to wait between pages of the search result,
#' i.e. between downloading the current bundle and the next bundle. This can be used to avoid choking a weak server with
#' too many requests to quickly. Defaults to zero.
#'
#' @return A list of bundles in xml format when \code{save_to_disc = FALSE} (the default),  else NULL.
#' @export
#'
#' @examples
#' \donttest{bundles <- fhir_search("https://hapi.fhir.org/baseR4/Medication?", max_bundles=3)}

fhir_search <-
	function(request = fhir_current_request(),
			 username = NULL,
			 password = NULL,
			 max_bundles = Inf,
			 verbose = 1,
			 max_attempts = 5,
			 delay_between_attempts = 10,
			 log_errors = 0,
			 save_to_disc = FALSE,
			 delay_between_pages = 0,
			 directory = paste0("FHIR_bundles_", gsub("-| |:","", Sys.time()))) {


		bundles <- list()

		if(is.null(request)){
			stop("You have not provided a FHIR search request and there is no ",
				 "current search request fhir_search() can fall back to. See documentation ",
				 "for fhir_current_request()")
		}


		addr <- request


		if (0 < verbose) {
			message(
				paste0(
					"Starting download of ",
					if (max_bundles < Inf)
						max_bundles
					else
						"ALL!",
					" bundles of resource type ",
					gsub("(^.+/)(.+)(\\?).*$", "\\2", request, perl = TRUE),
					" from FHIR endpoint ",
					gsub("(^.+)(/.+\\?).*$", "\\1", request, perl = TRUE),
					".\n"
				)
			)

			if (9 < max_bundles)
				message("This may take a while...")
		}

		cnt <- 0

		repeat {
			cnt <- cnt + 1

			if (1 < verbose) {
				cat(paste0("bundle[", cnt, "]"))
			}

			bundle <-
				get_bundle(
					request = addr,
					username = username,
					password = password,
					verbose = verbose,
					max_attempts = max_attempts,
					delay_between_attempts = delay_between_attempts,
					log_errors = log_errors
				)

			if (is.null(bundle)) {
				if (0 < verbose) {
					message("Download interrupted.\n")
				}

				break
			}

			xml2::xml_ns_strip(bundle)


			if(save_to_disc){

				if (!dir.exists(directory)){
					dir.create(directory, recursive = TRUE)
				}

				xml2::write_xml(
					bundle,
					paste_paths(directory, paste0(cnt, ".xml")))

			}else{
				bundles[[addr]] <- bundle
			}

			links <- xml2::xml_find_all(bundle, "link")

			rels.nxt <-
				xml2::xml_text(xml2::xml_find_first(links, "./relation/@value")) == "next"

			if (cnt == max_bundles) {
				if (0 < verbose) {
					if (any(!is.na(rels.nxt) & rels.nxt)) {
						message(
							"\nDownload completed. Number of downloaded bundles was limited to ",
							cnt,
							" bundles, this is less than the total number of bundles available.\n"
						)

						urls <- xml2::xml_attr(xml2::xml_find_first(links, "./url"), "value")

						assign(x = "last_next_link", value = urls[rels.nxt][1], envir = fhircrackr_env)
					}
					else {
						message("\nDownload completed. All available bundles were downloaded.\n")
					}
				}

				break
			}
			else {
				assign(x = "last_next_link", value = NULL, envir = fhircrackr_env)
			}

			if (!any(!is.na(rels.nxt) & rels.nxt)) {
				if (0 < verbose) {
					message("\nDownload completed. All available bundles were downloaded.\n")
				}

				break
			}

			urls  <-
				xml2::xml_attr(xml2::xml_find_first(links, "./url"), "value")

			addr <- urls[rels.nxt][1]

			if (is.null(addr) ||
				is.na(addr) || length(addr) < 1 || addr == "") {
				if (0 < verbose) {
					message("\nDownload completed. All available bundles were downloaded.\n")
				}

				break
			}
			Sys.sleep(delay_between_pages)
		}

		fhircrackr_env$current_request <- request

		if(save_to_disc){

			return(NULL)

		}else{

			return(bundles)

		}
	}


####Build FHIR Search####


#' Format FHIR base url
#'
#' Takes an url string and removes leading/trailing white space and unnecessary slashes.
#' Is supposed to be used with \code{\link{fhir_build_request}}.
#' @param url A string containing the base URL of the FHIR server, e.g.  "http://hapi.fhir.org/baseR4"
#' @return The formatted url in a named character vector
#' @examples fhir_base(" http://hapi.fhir.org/baseR4/")
#' @export

fhir_base <- function(url){

	#remove leading/trailing white space
	url <- stringr::str_trim(url, side="both")


	#remove trailing /
	if(stringr::str_sub(url, -1) =="/"){

		url <- stringr::str_sub(url, 1,-2)

	}

	return(c(base=url))
}

#' Check and format FHIR resource type for FHIR search
#'
#' This function takes a string defining a FHIR resource type and formats it correctly,
#' removing white space and slashes. It also checks the resource against the list
#' of resources provided at https://hl7.org/FHIR/resourcelist.html and throws a warning
#' if the resource doesn't match. \code{fhir_resource} is supposed to be used with
#' \code{\link{fhir_build_request}}.
#'
#' @param resource A string containing the resource type for the FHIR search.
#' Should be one of the official FHIR resource types listed at https://hl7.org/FHIR/resourcelist.html
#' @return A named character vector with the checked and formatted resource type
#' @examples fhir_resource("patient")
#' @export
#'
fhir_resource <- function(resource){

	#remove / and white space
	resource <- stringr::str_remove_all(resource, "/| ")

	#convert to correct case and check for validity
	if(tolower(resource) %in% tolower(existing_resource_types)){
		resource <- existing_resource_types[tolower(resource) == tolower(existing_resource_types)]
	}else{
		warning("It seems that the resource you provided is not one of the official resource types from https://hl7.org/FHIR/resourcelist.html. ",
				"If you are sure this resource exists on your server you can ignore this warning.")
	}


	stringr::str_sub(resource,1,1) <- stringr::str_to_upper(stringr::str_sub(resource,1,1))


	return(c(resource=resource))
}

#' Build key value pairs for FHIR search
#'
#' Takes two strings representing a key value pair for a FHIR search parameter and
#' encodes the pair properly.  \code{fhir_key_value} is supposed to be used with
#' \code{\link{fhir_build_request}}
#'
#' @param key The name of the search parameter, e.g. "_include", "gender" or "_summary".
#' For a general overview see https://www.hl7.org/fhir/search.html
#' and also check out the paragraph on search parameters for the respective resource,
#' e.g. http://www.hl7.org/fhir/patient.html#search
#' @param value The name of the respective value for the parameter, e.g. "Observation:patient",
#' "female" or "count".
#' @param url_enc URL encode key value pairs? Defaults to TRUE, which is advisable in most cases.
#' @return A string with the appropriately encoded key value pairs
#' @export
#' @examples
#' fhir_key_value(key = "gender", value = "female")
#' fhir_key_value(key = "category", value = "http://snomed.info/sct|116223007")



fhir_key_value <-function(key, value, url_enc = TRUE){

	#remove leading/trailing whitespace
	key <- stringr::str_trim(key)
	value <- stringr::str_trim(value)

	#url encode
	if(url_enc){
		key <- utils::URLencode(key, reserved = TRUE, repeated = FALSE)
		value <- utils::URLencode(value, reserved = TRUE, repeated = FALSE)
	}

	#paste
	result <- paste0(key, "=", value)

	return(c(keyval = result))

}

#' Build FHIR search request from base url, resource type and search parameters
#'
#' This function takes its arguments from the functions \code{\link{fhir_base}},
#' \code{\link{fhir_resource}} and \code{\link{fhir_key_value}}
#' You must provide exactly one call to \code{\link{fhir_base}}, and one call to
#' \code{\link{fhir_resource}}. You can provide none, one or multiple calls
#' to \code{\link{fhir_key_value}} (See examples).
#'
#' Apart from returning the string the function saves the url as the current request.
#' It can be accessed with \code{\link{fhir_current_request}}
#'
#' @param ... Calls to  \code{\link{fhir_base}}, \code{\link{fhir_resource}} and \code{\link{fhir_key_value}}
#' @return A string containing a FHIR search request ready for use
#' @export
#' @examples
#'
#' #Look for all MedicationAdministration resources
#'
#' fhir_build_request(fhir_base(url = "http://hapi.fhir.org/baseR4"),
#'                fhir_resource(resource = "MedicationAdministration")
#'                )
#'
#' #current search request is updated to this url:
#' fhir_current_request()
#'
#' #Look for all Condition resources,
#' #include Patient resources they refer to
#'
#' fhir_build_request(fhir_base(url = "http://hapi.fhir.org/baseR4"),
#'                fhir_resource(resource = "Condition"),
#'                fhir_key_value(key = "_include", value = "Condition:patient")
#'                )
#'
#' #Look for all Patient resources of Patients born before 1980,
#' #sort by death date
#'
#' fhir_build_request(fhir_base("http://hapi.fhir.org/baseR4"),
#'                fhir_resource("Patient"),
#'                fhir_key_value("birthdate", "lt1980-01-01"),
#'                fhir_key_value("_sort", "death-date")
#'                )

fhir_build_request <- function(...){

	args <- list(...)

	#unlist if arguments come from call to dissect_request
	if(is.list(args[[1]])){
		args <- args[[1]]
	}

	#process base url
	base <- args[sapply(args, function(x) names(x)=="base")]

	if(length(base) > 1){
		warning("You provided more than one base url, only the first is used.")
	}else if(length(base) < 1){
		stop("You need to provide a base url using fhir_base(<insert URL here>)")
	}

	base <- base[[1]]

	#process resource
	resource <- args[sapply(args, function(x) names(x)=="resource")]

	if(length(resource) > 1){
		warning("You provided more than one resource, only the first is used.")
	}else if(length(resource) < 1){
		stop("You need to provide a resource type, e.g. fhir_resource(\"Patient\")")
	}

	resource <- resource[[1]]

	keyvals <- paste(args[sapply(args, function(x) names(x)=="keyval")], collapse="&")

	if(keyvals != ""){

		result <- paste0(base, "/", resource, "?", keyvals)

	}else{

		result <- paste0(base, "/", resource)

	}

	fhircrackr_env$current_request <- result

	return(result)

}

#' Update the current FHIR search request
#'
#' Takes the current request (the search request URL from either the last call to
#' \code{\link{fhir_search}} or \code{\link{fhir_build_request}}) an updates the search
#' parameters with new calls to \code{\link{fhir_key_value}}. The updated request can be
#' accessed with \code{\link{fhir_current_request}}.
#'
#' @param ... calls to \code{\link{fhir_key_value}}
#' @param append Logical. Keep key value pairs from current search request?
#' Defaults to \code{FALSE}, meaning only base url and resource type from current request are kept.
#' If \code{TRUE}, the new key value pairs will be added to the existing ones.
#' @param return_request Logical. Return string with updated request? Defaults to \code{TRUE}.
#'
#' @examples
#' #build request
#' fhir_build_request(fhir_base("http://hapi.fhir.org/baseR4"),
#'                fhir_resource("Patient"),
#'                fhir_key_value(key = "gender", value = "female"))
#'
#' #access current request
#' fhir_current_request()
#'
#' #update and keep former key value pairs
#' fhir_update_request(fhir_key_value(key = "_count", value = "10"), append=TRUE)
#' fhir_current_request()
#'
#' #update and replace former key value pairs
#' fhir_update_request(fhir_key_value(key = "gender", value = "male"),
#' append = FALSE, return_request = TRUE)
#'
#' @export
#'
#' @return  A string with the updated FHIR search request or \code{NULL}.


fhir_update_request <- function(..., append = FALSE, return_request = TRUE){

	#newly provided key value pairs
	args <- list(...)

	#check validity
	if(any(sapply(args, function(x) names(x)!="keyval"))){
		stop("Please only use calls to fhir_key_value() inside this function.")
	}

	if(is.null(fhircrackr_env$current_request)){
		stop("It seems you haven't used fhir_search() or fhir_build_search_url()in this session yet. ",
			 "There is no search url to update.")
	}

	#get old search request elements
	old_elements <- dissect_request(fhircrackr_env$current_request)

	#Remove old key value pairs if new pairs should replace old ones
	if(!append){
		old_elements[sapply(old_elements, function(x) names(x)=="keyval")] <-NULL
	}

	#build new url
	fhircrackr_env$current_request <-
		fhir_build_request(c(old_elements[sapply(old_elements, function(x) names(x)=="base")],
							 old_elements[sapply(old_elements, function(x) names(x)=="resource")],
							 old_elements[sapply(old_elements, function(x) names(x)=="keyval")],
							 args))

	if(return_request){return(fhircrackr_env$current_request)}

}


#' Next Bundle's URL
#' @description fhir_next_bundle_url() gives the url of the next available bundle.
#' This is useful when you have not a lot of memory available or when a download of bundles was
#' interrupted for some reason. In case of small memory, you can use \code{fhir_next_bundle_url} together with the
#' \code{max_bundle} argument from \code{\link{fhir_search}} to download bundles in smaller batches in a loop.
#' See details in the example.
#'
#' @return A string containing an url to the next bundle available on the FHIR server of your last call to
#' \code{\link{fhir_search}} or NULL if no further bundle is available.
#' @export
#'
#' @examples
#' \donttest{
#' # workflow for small memory environments, downloading small batches of bundles
#' # for really small memory environments consider also using the _count option in
#' # your FHIR search request.
#' # You can iteratively download, crack and save the bundles until all bundles are processed or the
#' # desired number of bundles is reached.


#' url <- "http://hapi.fhir.org/baseR4/Observation"
#' count <- 0
#' while(!is.null(url) && count < 5){
#' 	bundles <- fhir_search(url, verbose = 2, max_bundles = 2)
#' 	tables <- fhir_crack(bundles, list(Obs=list(resource = "//Observation")))
#' 	save(tables, file = paste0(tempdir(),"/table_", count, ".RData"))
#' 	count <- count + 1
#' 	url <- fhir_next_bundle_url()
#' }
#'}
#'
fhir_next_bundle_url <- function() {

	fhircrackr_env$last_next_link
}

#' return FHIR search request used in last call to fhir_search
#' @export


fhir_current_request <- function() {

	fhircrackr_env$current_request
}


#' Get capability statement
#' @description Get the capability statement of a FHIR server.
#'
#' @param url The URL of the FHIR server endpoint.
#' @param username A string containing the username for basic authentication. Defaults to NULL, meaning no authentication.
#' @param password A string containing the password for basic authentication. Defaults to NULL, meaning no authentication.
#' @param sep A string to separate pasted multiple entries
#' @param remove_empty_columns Logical scalar. Remove empty columns?
#' @param brackets A character vector of length two defining the brackets surrounding indices for multiple entries, e.g. \code{c( "<", ">")}.
#' If \code{NULL}, no indices will be added to multiple entries. \code{NULL} means \code{brackets} is looked up in design, if it is \code{NULL} there too, no indices are added.
#' @param verbose An integer Scalar.  If 0, nothings is printed, if 1, only finishing message is printed, if > 1,
#' downloading/extraction progress will be printed. Defaults to 2.
#' @param add_indices Deprecated. This argument was used to control adding of indices for multiple entries. This is now
#' done via the brackets argument. If brackets is \code{NULL}, no indices are added, if brackets is not \code{NULL}, indices are added to multiple entries.
#'
#' @return A list of data frames containing the information from the statement
#' @export
#'
#' @examples
#' \donttest{cap <- fhir_capability_statement("https://hapi.fhir.org/baseR4")}
#'

fhir_capability_statement <-function(url = "https://hapi.fhir.org/baseR4",
									 username = NULL,
									 password = NULL,
									 sep = " ",
									 remove_empty_columns = TRUE,
									 brackets = NULL,
									 verbose = 2,
									 add_indices) {

	caps <-
		fhir_search(request = paste_paths(url, "/metadata?"),
					username = username,
					password = password,
					verbose = verbose)

	design <- list(
		META      = list(resource = "/CapabilityStatement", cols = "./*/@*"),
		REST.META = list(resource = "/CapabilityStatement/rest", cols = "./*/@*"),
		REST      = list(resource = "/CapabilityStatement/rest/resource")
	)

	fhir_crack(
		bundles = caps,
		design = design,
		sep = sep,
		remove_empty_columns = remove_empty_columns,
		brackets = brackets,
		verbose = verbose,
		add_indices = add_indices
	)
}

####Saving Bundles####

#' Save FHIR bundles as xml-files
#' @description Writes a list of FHIR bundles as numbered xml files into a directory.
#'
#' @param bundles A list of xml objects representing the FHIR bundles.
#' @param directory A string containing the path to the folder to store the data in.

#' @export
#'
#' @examples
#' #unserialize example bundle
#' bundles <- fhir_unserialize(medication_bundles)
#'
#' #save to temporary directory
#' fhir_save(bundles, directory = tempdir())


fhir_save <- function(bundles, directory = "result") {
	if (is_invalid_bundles_list(bundles)) {
		warning("Invalid bundle list format. No bundles have been saved")

		return(NULL)
	}

	w <- 1 + floor(log10(length(bundles)))

	if (!dir.exists(directory))

		dir.create(directory, recursive = TRUE)

	for (n in 1:length(bundles)) {
		xml2::write_xml(bundles[[n]], paste_paths(directory, paste0(
			stringr::str_pad(n, width = w, pad = "0"), ".xml"
		)))
	}
}



#' Load bundles from xml-files
#' @description Reads all bundles stored as xml files from a directory.
#'
#' @param directory A string containing the path to the folder were the files are stored.
#'
#' @return A list of bundles in xml format.
#' @export
#'
#' @examples
#' #unserialize example bundle
#' bundles <- fhir_unserialize(medication_bundles)
#'
#' #save to temporary directory
#' fhir_save(bundles, directory = tempdir())
#'
#' #load from temporary directory
#' loaded_bundles <- fhir_load(tempdir())

fhir_load <- function(directory) {

	if(!dir.exists(directory)){
		stop("Cannot find the specified directory.")
	}

	xml.files <- dir(directory, "*.xml")

	if(length(xml.files)==0){
		stop("Cannot find any xml-files in the specified directory.")
	}

	lapply(lst(xml.files), function(x)
		xml2::read_xml(paste_paths(directory, x)))
}



#' Serialize a FHIR Bundle list
#'
#' @description  Serializes a list of FHIR bundles to allow for saving in .rda or .RData format without losing integrity of pointers
#' @param bundles A list of xml objects representing FHIR bundles as returned by \code{\link{fhir_search}}
#' @return A list of serialized xml objects
#' @export
#' @examples
#' #example bundles are serialized, unserialize like this:
#' bundles <- fhir_unserialize(medication_bundles)
#'
#' #Serialize like this:
#' bundles_for_saving <- fhir_serialize(bundles)



fhir_serialize <- function(bundles) {
	if (is_invalid_bundles_list(bundles)) {
		return(NULL)
	}

	lapply(bundles, xml2::xml_serialize, connection = NULL)
}

#' Unserialize a FHIR Bundle list
#'
#' @description Unserializes a list of FHIR bundles that have been serialized to allow for saving in .rda or .RData format.
#' @param bundles A list of serialized xml objects representing FHIR bundles as returned by \code{\link{fhir_search}}
#' @return A list of unserialized xml objects
#' @export
#' @examples
#' bundles <- fhir_unserialize(medication_bundles)

fhir_unserialize <- function(bundles) {
	if (any(!sapply(bundles, is.raw))) {
		warning("The list you provided doesn't seem to contain serialized objects. Returing NULL")
		return(NULL)
	}

	lapply(bundles, xml2::xml_unserialize)
}


#################################################################################################
#################################################################################################


#' Dissect FHIR search request
#' @description Dissect FHIR search request into base, resource and  key value pairs
#' @param url The request as a string
#' @return A list containing the dissected request
#' @noRd

dissect_request <- function(request){

	#split base + resource from search parameters
	split0 <- strsplit(request, "?", fixed = T)[[1]]

	#split base from resource
	split1 <- strsplit(split0[1], "/", fixed=T)[[1]]

	base <- c(base=paste(split1[1:(length(split1)-1)], collapse = "/"))

	resource <- c(resource = split1[length(split1)])

	keyval <- as.list(strsplit(split0[2], "&", fixed=T)[[1]])
	keyval <- lapply(keyval, function(x){names(x)<-"keyval";x})


	c(list(base, resource), keyval)
}


#' Download single FHIR bundle
#' @description Download a single FHIR bundle via FHIR search request and return it as a xml object.
#'
#' @param request A string containing the full FHIR search request.
#' @param username A string containing the username for basic authentication. Defaults to NULL, meaning no authentication.
#' @param password A string containing the password for basic authentication. Defaults to NULL, meaning no authentication.
#' @param max_attempts A numeric scalar. The maximal number of attempts to send a request, defaults to 5.
#' @param verbose An integer scalar. If > 1,  Downloading progress is printed. Defaults to 2.
#' @param delay_between_attempts A numeric scalar specifying the delay in seconds between two attempts. Defaults to 10.
#' @param log_errors A numeric scalar. 0 means no error logging, 1: crack with fhir_crack and log to csv, 2 log xml directly
#'
#' @return The downloaded bundle in xml format.
#' @noRd
#'
#' @examples
#' bundle<-fhircrackr:::get_bundle(request = "https://hapi.fhir.org/baseR4/Patient?")

get_bundle <-
	function(request,
			 username = NULL,
			 password = NULL,
			 verbose = 2,
			 max_attempts = 5,
			 delay_between_attempts = 10,
			 log_errors = 0) {
		#dbg
		#request="https://hapi.fhir.org/baseR4/Medication?_format=xml"

		for (n in 1:max_attempts) {
			#dbg
			#n <- 1

			if (1 < verbose)
				cat(paste0("(", n, "): ", request, "\n"))

			auth <- if (!is.null(username) && !is.null(password)) {
				httr::authenticate(username, password)
			}

			response <- httr::GET(
				request,
				httr::add_headers(Accept = "application/fhir+xml"),
				auth
			)

			check_response(response, log_errors = log_errors)

			payload <-
				try(httr::content(response, as = "text", encoding = "UTF-8"),
					silent = TRUE)

			if (class(payload)[1] != "try-error") {
				xml <- try(xml2::read_xml(payload), silent = TRUE)

				if (class(xml)[1] != "try-error") {
					return(xml)
				}
			}

			Sys.sleep(delay_between_attempts)
		}

		NULL
	}

#'log the error message of a http response
#'
#' @param response A http response
#' @param log_errors A numeric scalar. 0 means no error logging, 1: crack with fhir_crack and log to csv, 2 log xml directly
#' @noRd
#'
#'
error_to_file <- function(response, log_errors) {
	payload <- httr::content(response, as = "text", encoding = "UTF-8")

	xml <- xml2::read_xml(payload)

	time <- gsub(" |-", "_", Sys.time())
	time <- gsub(":", "", time)

	if (log_errors == 1) {
		message <-
			fhir_crack(list(xml), list(error = list("//*")), verbose = 0)[[1]]
		utils::write.csv(message, paste0("error_message_", time, ".csv"))
	}

	if (log_errors == 2) {
		xml2::write_xml(xml, paste0("error_", time, ".xml"))
	}



}
#' Check http response
#'
#' Checks the http response and issues an error or warning if necessary
#'
#' @param response A http response
#' @param log_errors A numeric scalar. 0 means no error logging, 1: crack with fhir_crack and log to csv, 2 log xml directly
#' @noRd
#'
#'
check_response <- function(response, log_errors) {
	code <- response$status_code

	if (code != 200 && log_errors > 0) {
		error_to_file(response, log_errors)
	}

	if (code == 400) {
		if (log_errors > 0) {
			stop(
				"HTTP code 400 - This can be caused by an invalid FHIR search request or a server issue. For more information see the error file that has been generated in the working directory."
			)

		} else{
			stop(
				"HTTP code 400 - This can be caused by an invalid FHIR search request or a server issue. To print more detailed error information to a file, set argument log_errors to 1 or 2 and rerun fhir_search()."
			)

		}

	}

	if (code == 401) {
		if (log_errors > 0) {
			stop(
				"HTTP code 401 - Authentication needed. For more information see the error file that has been generated in the working directory."
			)

		} else{
			stop(
				"HTTP code 401 - Authentication needed. To print more detailed error information to a file, set argument log_errors to 1 or 2 and rerun fhir_search()."
			)

		}

	}

	if (code == 404) {
		if (log_errors > 0) {
			stop(
				"HTTP code 404 - Not found. Did you misspell the resource? For more information see the error file that has been generated in the working directory."
			)

		} else{
			stop(
				"HTTP code 404 - Not found. Did you misspell the resource? To print more detailed error information to a file, set argument log_errors to 1 or 2 and rerun fhir_search()."
			)

		}

	}

	if (code >= 300 && code < 400) {
		if (log_errors > 0) {
			warning(
				"Your request generated a HTTP code ",
				code,
				". For more information see the error file that has been generated in the working directory."
			)

		} else{
			warning(
				"Your request generated a HTTP code ",
				code,
				". To print more detailed information to a file, set argument log_errors to 1 or 2 and rerun fhir_search()."
			)

		}

	}

	if (code >= 400 & code < 500) {
		if (log_errors > 0) {
			stop(
				"Your request generated a client error, HTTP code ",
				code,
				". For more information see the error file that has been generated in the working directory."
			)

		} else{
			stop(
				"Your request generated a client error, HTTP code ",
				code,
				". To print more detailed information to a file, set argument log_errors to 1 or 2 and rerun fhir_search()."
			)

		}

	}

	if (code >= 500 && code < 600) {
		if (log_errors > 0) {
			stop(
				"Your request generated a server error, HTTP code ",
				code,
				". For more information see the error file that has been generated in the working directory."
			)

		} else{
			stop(
				"Your request generated a server error, HTTP code ",
				code,
				". To print more detailed error information to a file, set argument log_errors to 1 or 2 and rerun fhir_search()."
			)

		}

	}

}

