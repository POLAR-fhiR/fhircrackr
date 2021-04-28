## This file contains all functions for downloading/loading/saving resources##
## Exported functions are on top, internal functions below ##

#' Download FHIR search result
#' @description Downloads all FHIR bundles of a FHIR search request from a FHIR server.
#'
#' @param request An object of class [fhir_url-class] or a string containing the full FHIR search request. It is
#' recommended to explicitly create the request via [fhir_url()] as this will do some validity checks and format the url properly.
#' Defaults to [fhir_current_request()]
#' @param username A string containing the username for basic authentication. Defaults to NULL, meaning no authentication.
#' @param password A string containing the password for basic authentication. Defaults to NULL, meaning no authentication.
#' @param max_bundles Maximal number of bundles to get. Defaults to Inf meaning all available bundles are downloaded.
#' @param verbose An Integer Scalar.  If 0, nothings is printed, if 1, only finishing message is printed, if > 1,
#' downloading progress will be printed. Defaults to 2.
#' @param max_attempts A numeric scalar. The maximal number of attempts to send a request, defaults to 5.
#' @param delay_between_attempts A numeric scalar specifying the delay in seconds between two attempts. Defaults to 10.
#' @param log_errors Either `NULL` or a string indicating the name of a file in which to save the http errors.
#' `NULL` means no error logging. When a file name is provided, the errors are saved in the specified file. Defaults to `NULL`
#' @param save_to_disc Either `NULL` or a string indicating the name of a directory in which to save the bundles.
#' If a directory name is provided, the bundles are saved as numerated xml-files into the directory specified
#' and not returned as a bundle list in the R session. This is useful when a lot of bundles are to be downloaded and keeping them all
#' in one R session might overburden working memory. When the download is complete, the bundles can be loaded into R using [fhir_load()].
#' Defaults to `NULL`, i.e. bundles are returned as a list within the R session.
#' @param directory Deprecated. Please specify the directory directly in the `save_to_disc` argument.
#' @param delay_between_pages A numeric scalar specifying a time in seconds to wait between pages of the search result,
#' i.e. between downloading the current bundle and the next bundle. This can be used to avoid choking a weak server with
#' too many requests to quickly. Defaults to zero.
#'
#' @return A [fhir_bundle_list-class] when`save_to_disc = NULL` (the default),  else `NULL`.
#' @export
#'
#' @examples
#' \donttest{
#'
#' #create fhir search url
#' request <- fhir_url(url = "https://server.fire.ly",
#'                     resource = "Patient",
#'                     parameters = c(gender="female"))
#' #download bundles
#' bundles <- fhir_search(request, max_bundles = 5)
#' }

fhir_search <- function(
	request = fhir_current_request(),
	username = NULL,
	password = NULL,
	max_bundles = Inf,
	verbose = 1,
	max_attempts = 5,
	delay_between_attempts = 10,
	log_errors = NULL,
	save_to_disc = NULL,
	delay_between_pages = 0,
	directory = paste0("FHIR_bundles_", gsub("-| |:","", Sys.time()))) {



	####remove at some point####
	if(is.logical(save_to_disc)){
		warning("The use of TRUE/FALSE in the argument save_to_disc in combination with the argument directory is ",
				"deprecated. Please specify the directory name in the save_to_disc argument directly (see ?fhir_search).")
		if(save_to_disc){
			message("Setting save_to_disc to '", directory, "'.")
			save_to_disc <- directory
			}
	}

	if(is.numeric(log_errors)){
		warning("The use of numbers in log_errors is deprecated. Please specify a file name if you want to log erros and NULL if you don't.\n")
		if(log_errors>0){
			message("Setting log_errors to 'http_error_fhir_search.xml'.")
			log_errors <- "http_error_fhir_search.xml"
		}else{
			log_errors <- NULL
		}

	}
	#######################################################

	if(is.null(request)){
		stop("You have not provided a FHIR search request and there is no ",
			 "current search request fhir_search() can fall back to. See documentation ",
			 "for fhir_current_request()")
	}

	bundles <- list()

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

		if(!is.null(save_to_disc)){

			if (!dir.exists(save_to_disc)){
				dir.create(save_to_disc, recursive = TRUE)
			}

			xml2::write_xml(
				bundle,
				paste_paths(save_to_disc, paste0(cnt, ".xml")))

		}else{
			bundles[[length(bundles) + 1]] <- bundle
		}

		if (cnt == max_bundles) { #stop because max_bundles is reached
			if (0 < verbose) {
				if (length(bundle@next_link)>0) {
					message(
						"\nDownload completed. Number of downloaded bundles was limited to ",
						cnt,
						" bundles, this is less than the total number of bundles available.\n"
					)

					assign(x = "last_next_link", value = bundle@next_link, envir = fhircrackr_env)
				}
				else {
					message("\nDownload completed. All available bundles were downloaded.\n")
				}
			}

			break
		}
		else { #finished because there are no more bundles
			assign(x = "last_next_link", value = new("fhir_url"), envir = fhircrackr_env)
		}

		if (length(bundle@next_link)==0) {
			if (0 < verbose) {
				message("\nDownload completed. All available bundles were downloaded.\n")
			}

			break
		}

		addr <- bundle@next_link

		Sys.sleep(delay_between_pages)
	}

	fhircrackr_env$current_request <- request

	if(!is.null(save_to_disc)){

		return(NULL)

	}else{

		return(fhir_bundle_list(bundles))

	}
}


#' Next Bundle's URL
#' @description fhir_next_bundle_url() gives the link to the next available bundle, either of the bundle
#' you provided in the argument `bundle` or of the last call to [fhir_search()], if `bundle=NULL` (the default).
#'
#' This function is useful when you don't have a lot of memory available or when a download of bundles was
#' interrupted for some reason. In case of small memory, you can use `fhir_next_bundle_url` together with the
#' `max_bundle` argument from [fhir_search()] to download bundles in smaller batches in a loop.
#' See details in the example.
#'
#' @param bundle The bundle from which you wish to extract the next link. If this is `NULL` (the default), the function will
#' extract the next link from the last bundle that was downloaded in the most recent call to [fhir_search()].
#'
#' @return A [fhir_url-class] object referencing next bundle available on the FHIR server.
#' Empty [fhir_url-class] / character vector, if no further bundle is available.
#' @export
#'
#' @examples
#' \donttest{
#' # workflow for small memory environments, downloading small batches of bundles
#' # for really small memory environments consider also using the `_count` option in
#' # your FHIR search request.
#' # You can iteratively download, crack and save the bundles until all bundles are processed or the
#' # desired number of bundles is reached.
#' url <- fhir_url("https://server.fire.ly/Patient")
#' count <- 0
#' obs <- fhir_df_description(resource = "Patient")
#' design <- fhir_design(obs)
#' while(length(url)>0 && count < 5){
#' 	 bundles <- fhir_search(url, max_bundles = 2)
#' 	 tables <- fhir_crack(bundles, design)
#'   save(tables, file = paste0(tempdir(),"/table_", count, ".RData"))
#'   count <- count + 1
#'   url <- fhir_next_bundle_url()
#' }
#' #you can see the saved tables here:
#' dir(tempdir())
#'}
#'
fhir_next_bundle_url <- function(bundle=NULL) {

	if(!is.null(bundle)){
		if(!is(bundle, "fhir_bundle")){
			stop("bundle must be an object of type fhir_bundle")
		}
		if(is(bundle, "fhir_bundle_xml")){
			bundle@next_link
		}else{
			b <- fhir_unserialize(b)
			b@next_link
		}

	}else{
		fhircrackr_env$last_next_link
	}
}

#' Return FHIR search request used in last call to [fhir_search()] or [fhir_url()]
#'
#' @return An object of class [fhir_url()]
#'
#' @examples
#' \donttest{
#' request <- fhir_url(url = "https://hapi.fhir.org/baseR4", resource = "Patient")
#' fhir_current_request()
#'
#' fhir_search("https://hapi.fhir.org/baseR4/Medication")
#' fhir_current_request()
#' }
#'
#'
#' @export


fhir_current_request <- function() {

	fhircrackr_env$current_request
}

#' Get capability statement
#' @description Get the capability statement of a FHIR server.
#'
#' This function downloads a capability statement and creates three data.frames from it:
#' - `Meta` contains general information on the server
#' - `Rest` contains information on the Rest operations the server supports
#' - `Resources` contains information on the supported resource types
#'
#' When there is more than one piece of information regarding a variable in these data.frames,
#' they are divided by the string specified in `sep`. If `brackets` is not NULL, those entries
#' will also be assigned indices so you can melt them using [fhir_melt()].
#'
#' @param url The base URL of the FHIR server.
#' @param username A string containing the username for basic authentication. Defaults to NULL, meaning no authentication.
#' @param password A string containing the password for basic authentication. Defaults to NULL, meaning no authentication.
#' @param sep A string to separate pasted multiple entries
#' @param brackets A character vector of length two defining the brackets surrounding indices for multiple entries, e.g. `c( "<", ">")`.
#' If `NULL`, no indices will be added to multiple entries.
#' @param verbose An integer Scalar.  If 0, nothing is printed, if 1, only finishing message is printed, if > 1,
#' downloading/extraction progress will be printed. Defaults to 2.
#' @return A list of data frames containing the information from the statement
#' @export
#'
#' @examples
#' \donttest{
#' #without indices
#' cap <- fhir_capability_statement("https://hapi.fhir.org/baseR4")
#'
#' #with indices
#' cap <- fhir_capability_statement("https://hapi.fhir.org/baseR4", brackets = c("[","]"))
#'
#' #melt searchInclude variable
#' resources <- fhir_melt(cap$Resources,
#'                        columns = "searchInclude",
#'                        brackets = c("[", "]"), sep = " || ",
#'                        all_columns = TRUE)
#'
#' #remove indices
#' resources <- fhir_rm_indices(resources, brackets = c("[", "]"))
#'
#' View(resources)
#'}

fhir_capability_statement <-function(url = "https://hapi.fhir.org/baseR4",
									 username = NULL,
									 password = NULL,
									 brackets = NULL,
									 sep = " || ",
									 log_errors = NULL) {


	auth <- if (!is.null(username) && !is.null(password)) {
		httr::authenticate(username, password)
	}

	response <- httr::GET(
		paste_paths(url, "/metadata?"),
		httr::add_headers(Accept = "application/fhir+xml"),
		auth
	)

	#check for http errors
	check_response(response, log_errors = log_errors)

	#extract payload
	payload <- httr::content(response, as = "text", encoding = "UTF-8")
	xml <- xml2::read_xml(payload)
	xml2::xml_ns_strip(xml)

	xml_meta <- xml2::xml_new_root(xml, .copy = T)
	xml2::xml_remove(xml2::xml_find_all(xml_meta, "/CapabilityStatement/rest"))


	xml_rest <- xml2::xml_new_root(xml, .copy = T)
	xml_rest <- xml2::xml_find_all(xml_rest, "/CapabilityStatement/rest")
	xml2::xml_remove(xml2::xml_find_all(xml_rest, "/CapabilityStatement/rest/resource"))

	xml_resource <- xml2::xml_find_all(xml, "/CapabilityStatement/rest/resource")

	suppressWarnings({
		desc_meta <- fhir_df_description(resource = "/CapabilityStatement")
		desc_rest <- fhir_df_description(resource = "rest")
		desc_resource <- fhir_df_description(resource = "resource")
	})

	META <- fhir_crack(
		bundles = list(xml_meta),
		design = fhir_design(desc_meta),
		sep=sep, brackets = brackets
	)

	restBrackets <- if(is.null(brackets)){c("[", "]")}else{brackets}

	REST <- fhir_crack(
		bundles = list(xml_rest),
		design = fhir_design(desc_rest),
		sep=sep,brackets = restBrackets
	)

	rest <- fhir_melt(REST$desc_rest, brackets = restBrackets, sep = " || ",
					  columns = fhir_common_columns(REST$desc_rest, column_names_prefix = "operation"),
					  all_columns = T)

	rest$resource_identifier <- NULL
	if(is.null(brackets)){rest <- fhir_rm_indices(rest, brackets = restBrackets)}

	RESOURCE <- fhir_crack(
		bundles = list(xml_resource),
		design = fhir_design(desc_resource),
		sep=sep, brackets = brackets
	)

	list(Meta = META$desc_meta,
		 Rest = unique(rest),
		 Resources = RESOURCE$desc_resource
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
#' @param bundles A [fhir_bundle-class] or [fhir_bundle_list-class] object.
#' @return A  [fhir_bundle_xml-class] or [fhir_bundle_list-class] object.
#' @export
#' @examples
#'
#' #example bundles are serialized, unserialize like this:
#' bundles <- fhir_unserialize(medication_bundles)
#'
#' #Serialize like this:
#' bundles_for_saving <- fhir_serialize(bundles)
#'
#' #works also on single bundles
#' fhir_serialize(bundles[[1]])
#'

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
#'
#' #unserialize bundle list
#' fhir_unserialize(patient_bundles)
#'
#' #unserialize single bundle
#' fhir_unserialize(patient_bundles[[1]])
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
#' @param log_errors Either `NULL` or a string indicating the name of a file in which to save the http errors.
#' `NULL` means no error logging. When a file name is provided, the errors are saved in the specified file. Defaults to `NULL`
#'
#' @return The downloaded bundle as an [fhir_bundle_xml-class].
#' @noRd
#'
#' @examples
#' \donttest{
#' bundle<-fhircrackr:::get_bundle(request = "https://hapi.fhir.org/baseR4/Patient?")
#' }
#'

get_bundle <- function(
	request,
	username = NULL,
	password = NULL,
	verbose = 2,
	max_attempts = 5,
	delay_between_attempts = 10,
	log_errors = NULL) {

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
#' @param log_errors A string indicating the name of a file in which to save the http errors.
#' @noRd
#'
error_to_file <- function(response,log_errors) {
	payload <- httr::content(response, as = "text", encoding = "UTF-8")

	xml <- xml2::read_xml(payload)

	xml2::write_xml(xml, file = log_errors)

}
#' Check http response
#'
#' Checks the http response and issues an error or warning if necessary
#'
#' @param response A http response
#' @param log_errors Either `NULL` or a string indicating the name of a file in which to save the http errors.
#' `NULL` means no error logging. When a file name is provided, the errors are saved in the specified file.
#' @noRd
#'
#'
check_response <- function(response, log_errors) {
	code <- response$status_code

	if (code != 200 && !is.null(log_errors)) {
		error_to_file(response, log_errors)
	}

	if (code == 400) {
		if (!is.null(log_errors)) {
			stop(
				"HTTP code 400 - This can be caused by an invalid FHIR search request or a server issue. For more information see the generated error file."
			)

		} else{
			stop(
				"HTTP code 400 - This can be caused by an invalid FHIR search request or a server issue. To print more detailed error information to a file, set argument log_errors to a filename and rerun fhir_search()."
			)

		}

	}

	if (code == 401) {
		if (!is.null(log_errors)) {
			stop(
				"HTTP code 401 - Authentication needed. For more information see the generated error file."
			)

		} else{
			stop(
				"HTTP code 401 - Authentication needed. To print more detailed error information to a file, set argument log_errors to a filename and rerun fhir_search()."
			)

		}

	}

	if (code == 404) {
		if (!is.null(log_errors)) {
			stop(
				"HTTP code 404 - Not found. Did you misspell the resource? For more information see the generated error file."
			)

		} else{
			stop(
				"HTTP code 404 - Not found. Did you misspell the resource? To print more detailed error information to a file, set argument log_errors to a filename and rerun fhir_search()."
			)

		}

	}

	if (code >= 300 && code < 400) {
		if (!is.null(log_errors)) {
			warning(
				"Your request generated a HTTP code ",
				code,
				". For more information see the generated error file ."
			)

		} else{
			warning(
				"Your request generated a HTTP code ",
				code,
				". To print more detailed information to a file, set argument log_errors to a filename and rerun fhir_search()."
			)

		}

	}

	if (code >= 400 & code < 500) {
		if (!is.null(log_errors)) {
			stop(
				"Your request generated a client error, HTTP code ",
				code,
				". For more information see the generated error file."
			)

		} else{
			stop(
				"Your request generated a client error, HTTP code ",
				code,
				". To print more detailed information to a file, set argument log_errors to a filename and rerun fhir_search()."
			)

		}

	}

	if (code >= 500 && code < 600) {
		if (!is.null(log_errors)) {
			stop(
				"Your request generated a server error, HTTP code ",
				code,
				". For more information see the generated error file."
			)

		} else{
			stop(
				"Your request generated a server error, HTTP code ",
				code,
				". To print more detailed error information to a file, set argument log_errors to a filename and rerun fhir_search()."
			)

		}

	}

}

