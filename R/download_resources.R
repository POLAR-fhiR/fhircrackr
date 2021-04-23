## This file contains all functions for downloading/loading/saving resources##
## Exported functions are on top, internal functions below ##

#' Download FHIR search result
#' @description Downloads all FHIR bundles of a FHIR search request from a FHIR server.
#'
#' @param request An object of class [fhir_url-class] or a string containing the full FHIR search request. It is
#' recommended to explicitly create the request via [fhir_url()] as this will do some validity checks and format the url properly.
#' TODO: Defaults to \code{\link{fhir_current_request}}
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
#' \donttest{
#'
#' #create fhir search url
#' request <- fhir_search_url(base = "https://hapi.fhir.org/baseR4",
#'                            resource = "Patient",
#'                            parameters = c(gender="female"))
#' #download bundles
#' bundles <- fhir_search(request, max_bundles=3)
#' }

fhir_search <- function(
	request = fhir_current_request(),
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


	addr <- fhir_url(request)

	#starting message
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
				" from FHIR base URL ",
				gsub("(^.+)(/.+\\?).*$", "\\1", request, perl = TRUE),
				".\n"
			)
		)

		if (9 < max_bundles)
			message("This may take a while...")
	}

	#download bundles
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


#' Next Bundle's URL
#' @description fhir_next_bundle_url() gives the url of the next available bundle.
#' This is useful when you have not a lot of memory available or when a download of bundles was
#' interrupted for some reason. In case of small memory, you can use \code{fhir_next_bundle_url} together with the
#' \code{max_bundle} argument from \code{\link{fhir_search}} to download bundles in smaller batches in a loop.
#' See details in the example.
#'
#' @return A [fhir_url-class] object referencing next bundle available on the FHIR server of your last call to
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

#TODO
#' url <- fhir_url("http://hapi.fhir.org/baseR4/Observation")
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

#' Return FHIR search request used in last call to [fhir_search()] or [fhir_search_url()]
#'
#' @return An object of class [fhir_search_url()]
#' @export


fhir_current_request <- function() {

	fhircrackr_env$current_request
}

#TODO
#' #' Get capability statement
#' #' @description Get the capability statement of a FHIR server.
#' #'
#' #' @param url The base URL of the FHIR server.
#' #' @param username A string containing the username for basic authentication. Defaults to NULL, meaning no authentication.
#' #' @param password A string containing the password for basic authentication. Defaults to NULL, meaning no authentication.
#' #' @param sep A string to separate pasted multiple entries
#' #' @param remove_empty_columns Logical scalar. Remove empty columns?
#' #' @param brackets A character vector of length two defining the brackets surrounding indices for multiple entries, e.g. \code{c( "<", ">")}.
#' #' If \code{NULL}, no indices will be added to multiple entries. \code{NULL} means \code{brackets} is looked up in design, if it is \code{NULL} there too, no indices are added.
#' #' @param verbose An integer Scalar.  If 0, nothings is printed, if 1, only finishing message is printed, if > 1,
#' #' downloading/extraction progress will be printed. Defaults to 2.
#' #' @param add_indices Deprecated. This argument was used to control adding of indices for multiple entries. This is now
#' #' done via the brackets argument. If brackets is \code{NULL}, no indices are added, if brackets is not \code{NULL}, indices are added to multiple entries.
#' #'
#' #' @return A list of data frames containing the information from the statement
#' #' @export
#' #'
#' #' @examples
#' #' \donttest{cap <- fhir_capability_statement("https://hapi.fhir.org/baseR4")}
#' #'
#'
#' fhir_capability_statement <-function(url = "https://hapi.fhir.org/baseR4",
#' 									 username = NULL,
#' 									 password = NULL,
#' 									 sep = " ",
#' 									 remove_empty_columns = TRUE,
#' 									 brackets = NULL,
#' 									 verbose = 2,
#' 									 add_indices) {
#'
#' 	caps <-
#' 		fhir_search(request = paste_paths(url, "/metadata?"),
#' 					username = username,
#' 					password = password,
#' 					verbose = verbose)
#'
#' 	design <- list(
#' 		META      = list(resource = "/CapabilityStatement", cols = "./*/@*"),
#' 		REST.META = list(resource = "/CapabilityStatement/rest", cols = "./*/@*"),
#' 		REST      = list(resource = "/CapabilityStatement/rest/resource")
#' 	)
#'
#' 	fhir_crack(
#' 		bundles = caps,
#' 		design = design,
#' 		sep = sep,
#' 		remove_empty_columns = remove_empty_columns,
#' 		brackets = brackets,
#' 		verbose = verbose,
#' 		add_indices = add_indices
#' 	)
#' }

####Saving Bundles####

#' Save FHIR bundles as xml-files
#' @description Writes a list of FHIR bundles as numbered xml files into a directory.
#'
#' @param bundles A list of xml objects representing the FHIR bundles.
#' @param directory A string containing the path to the folder to store the data in.

#' @export
#'
#' @examples
#' TODO
#' #unserialize example bundle
#' bundles <- fhir_unserialize(medication_bundles)
#'
#' #save to temporary directory
#' fhir_save(bundles, directory = tempdir())


fhir_save <- function(bundles, directory = "result") {

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
#' @return A [fhir_bundle_list-class].
#' @export
#'
#' @examples
#' TODO
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

	list <- lapply(lst(xml.files), function(x)
		xml2::read_xml(paste_paths(directory, x)))

	fhir_bundle_list(list)
}



#' Serialize a [fhir_bundle-class] or [fhir_bundle_list-class]
#'
#' @description Serializes FHIR bundles to allow for saving in .rda or .RData format without losing integrity of pointers
#' i.e. it turns a [fhir_bundle_xml-class] object into an [fhir_bundle_serialized-class] object.
#' @description
#' @param bundles A [fhir_bundle-class] or [fhir_bundle_list-class] object.
#' @return A  [fhir_bundle_xml-class] or [fhir_bundle_list-class] object.
#' @export
#' @examples
#'
#' TODO: update example
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

setGeneric(
	"fhir_serialize",
	function(bundles){
		standardGeneric("fhir_serialize")
	}
)

setMethod(
	"fhir_serialize",
	signature = c(bundles = "fhir_bundle_xml"),
	function(bundles){
		fhir_bundle_serialized(xml2::xml_serialize(bundles, connection = NULL))
	}
)

setMethod(
	"fhir_serialize",
	signature = c(bundles = "fhir_bundle_serialized"),
	function(bundles){
		bundles
	}
)

setMethod(
	"fhir_serialize",
	signature = c(bundles = "fhir_bundle_list"),
	function(bundles){
		if(is(bundles[[1]], "fhir_bundle_xml")){

			fhir_bundle_list(lapply(bundles, xml2::xml_serialize, connection=NULL))

		}else{
			bundles
		}
	}
)


#' Unserialize a [fhir_bundle-class] or [fhir_bundle_list-class]
#'
#' @description Unserializes FHIR bundles that have been serialized to allow for saving in .rda or .RData format,
#' i.e. it turns a [fhir_bundle_serialized-class] object into an [fhir_bundle_xml-class] object.
#' @param bundles A [fhir_bundle-class] or [fhir_bundle_list-class] object.
#' @return A  [fhir_bundle_serialized-class] or [fhir_bundle_list-class] object.
#' @export
#' @examples
#' TODO: update example bundles <- fhir_unserialize(medication_bundles)
#'
#' @include fhir_bundle.R fhir_bundle_list.R

setGeneric(
	"fhir_unserialize",
	function(bundles){
		standardGeneric("fhir_unserialize")
	}
)

setMethod(
	"fhir_unserialize",
	signature = c(bundles = "fhir_bundle_xml"),
	function(bundles){
		bundles
	}
)

setMethod(
	"fhir_unserialize",
	signature = c(bundles = "fhir_bundle_serialized"),
	function(bundles){
		b <- xml2::xml_unserialize(bundles)
		fhir_bundle_xml(b)
	}
)

setMethod(
	"fhir_unserialize",
	signature = c(bundles = "fhir_bundle_list"),
	function(bundles){
		if(is(bundles[[1]], "fhir_bundle_xml")){
			bundles
		}else{
			fhir_bundle_list(lapply(bundles, xml2::xml_unserialize))
		}
	}
)

#################################################################################################
#################################################################################################

#' Download single FHIR bundle
#' @description Download a single FHIR bundle via FHIR search request and return it as a xml object.
#'
#' @param request An object of class [fhir_search_url-class] or string containing the full FHIR search request.
#' @param username A string containing the username for basic authentication. Defaults to NULL, meaning no authentication.
#' @param password A string containing the password for basic authentication. Defaults to NULL, meaning no authentication.
#' @param max_attempts A numeric scalar. The maximal number of attempts to send a request, defaults to 5.
#' @param verbose An integer scalar. If > 1,  Downloading progress is printed. Defaults to 2.
#' @param delay_between_attempts A numeric scalar specifying the delay in seconds between two attempts. Defaults to 10.
#' @param log_errors Save http errors in an xml file? Defaults to FALSE.
#'
#' @return The downloaded bundle as an [fhir_bundle_xml-class].
#' @noRd
#'
#' @examples
#' bundle<-fhircrackr:::get_bundle(request = "https://hapi.fhir.org/baseR4/Patient?")

get_bundle <- function(
	request,
	username = NULL,
	password = NULL,
	verbose = 2,
	max_attempts = 5,
	delay_between_attempts = 10,
	log_errors = FALSE) {

	#download response
	for (n in 1:max_attempts) {

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

		#check for http errors
		check_response(response, log_errors = log_errors)

		#extract payload
		payload <-
			try(httr::content(response, as = "text", encoding = "UTF-8"),
				silent = TRUE)

		if (class(payload)[1] != "try-error") {
			xml <- try(xml2::read_xml(payload), silent = TRUE)

			if (class(xml)[1] != "try-error") {
				return(fhir_bundle_xml(xml))
			}
		}

		Sys.sleep(delay_between_attempts)
	}

	NULL
}

#'log the error message of a http response
#'
#' @param response A http response
#' @noRd
#'
error_to_file <- function(response) {
	payload <- httr::content(response, as = "text", encoding = "UTF-8")

	xml <- xml2::read_xml(payload)

	time <- gsub(" |-", "_", Sys.time())
	time <- gsub(":", "", time)

	xml2::write_xml(xml, paste0("error_", time, ".xml"))

}
#' Check http response
#'
#' Checks the http response and issues an error or warning if necessary
#'
#' @param response A http response
#' @param log_errors Save http errors in an xml file? Defaults to FALSE.
#' @noRd
#'
#'
check_response <- function(response, log_errors) {
	code <- response$status_code

	if (code != 200 && log_errors > 0) {
		error_to_file(response)
	}

	if (code == 400) {
		if (log_errors > 0) {
			stop(
				"HTTP code 400 - This can be caused by an invalid FHIR search request or a server issue. For more information see the error file that has been generated in the working directory."
			)

		} else{
			stop(
				"HTTP code 400 - This can be caused by an invalid FHIR search request or a server issue. To print more detailed error information to a file, set argument log_errors to TRUE and rerun fhir_search()."
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
				"HTTP code 401 - Authentication needed. To print more detailed error information to a file, set argument log_errors to TRUE and rerun fhir_search()."
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
				"HTTP code 404 - Not found. Did you misspell the resource? To print more detailed error information to a file, set argument log_errors to TRUE and rerun fhir_search()."
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
				". To print more detailed information to a file, set argument log_errors to TRUE and rerun fhir_search()."
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
				". To print more detailed information to a file, set argument log_errors to TRUE and rerun fhir_search()."
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
				". To print more detailed error information to a file, set argument log_errors to TRUE and rerun fhir_search()."
			)

		}

	}

}

