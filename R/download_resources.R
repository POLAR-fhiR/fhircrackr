## This file contains all functions for downloading/loading/saving resources##
## Exported functions are on top, internal functions below ##

#' Download FHIR search result
#' @description Downloads all FHIR bundles of a FHIR search request from a FHIR server by iterating through the bundles. Search via GET
#' and POST is possible, see Details.
#'
#' @details
#' ## Request type
#' `fhir_search` allows for two types of search request:
#' 1. FHIR search via GET:
#' This is the more common approach. All information on which resources to download is contained in the URL
#' that is send to the server (`request` argument). This encompasses the base url of the server, the resource type and possible
#' search parameters to further qualify the search (see [fhir_url()]). The search via GET is the default and performed whenever
#' the argument `body` is NULL.
#'
#'  2. FHIR search via POST:
#'  This option should only be used when the parameters make the search URL so long the server might deny it
#'  because it exceeds the allowed length. In this case the search parameters (everything that would usually follow the resource type
#'  after the `?`) can be transferred to a body of type `"application/x-www-form-urlencoded"` and send via POST. If you provide a body in
#'  `fhir_search()`, the url in `request` should only contain the base URL and the resource type.
#'  The function will automatically amend it with `_search` and perform a POST.
#'
#' ## Authentication
#' There are several ways of authentication implemented in `fhir_search()`. If you don't need any authentication,
#' just leave the arguments described in the following at their default values of `NULL`.
#' 1. Basic Authentication: Provide the `username` and the `password` for basic authentication in the respective arguments.
#'
#' 2. Token Authentication: Provide a token in the argument `token`, either as a character vector of length one or as as an object of class
#' [httr::Token-class]. You can use the function [fhir_authenticate()] to create this object.
#'
#' 3. Cookie Authentication: Provide the key value pair(s) as a named character vector to the `cookies` argument, e.g.
#' `cookies = c(mycookie = "d385se12394j")`.
#'
#' ## HTML removal
#' FHIR resources can contain a considerable amount of html code (e.g. in a [narrative](https://www.hl7.org/fhir/narrative.html#xhtml) object),
#' which is often created by the server for example to provide a human-readable summary of the resource.
#' This data is usually not the aim of structured statistical analysis, so in the default setting [fhir_search()] will remove the html
#' parts immediately after download to reduce memory usage (on a hapi server typically by around 30%, see [fhir_rm_div()]).
#' The memory gain is payed with a runtime increase of 10%-20%. The html removal can be disabled by setting `rm_tag = NULL`
#' to increase speed at the cost of increased memory usage.
#'
#' @param request An object of class [fhir_url-class] or a character vector of length one containing the full FHIR search request. It is
#' recommended to explicitly create the request via [fhir_url()] as this will do some validity checks and format the url properly.
#' Defaults to [fhir_current_request()]
#' @param body A character vector of length one or object of class `fhir_body` with type `"application/x-www-form-urlencoded"`. A body should be provided
#' when the FHIR search request is too long and might exceed the maximal allowed length of the URL when send to the server. In this case
#' a search via POST (see https://www.hl7.org/fhir/search.html#Introduction) can be used. The body should contain all the parameters that
#' follow after the `?` in the FHIR search request. When a body is provided, the required `_search` is automatically added
#' to the url in `request`. See examples and `?fhir_body`.
#' @param username A character vector of length one containing the username for basic authentication.
#' @param password A character vector of length one containing the password for basic authentication.
#' @param token A character vector of length one or object of class [httr::Token-class], for bearer token authentication (e.g. OAuth2). See [fhir_authenticate()]
#' for how to create this.
#' @param cookies Optional. A named character vector containing key value pairs for cookies, e.g. `c(mycookie = "d385se12394j")`.
#' @param max_bundles Maximal number of bundles to get. Defaults to Inf meaning all available bundles are downloaded.
#' @param verbose An integer vector of length one. If 0, nothing is printed, if 1, only finishing message is printed, if > 1,
#' downloading progress will be printed. Defaults to 1.
#' @param max_attempts `r lifecycle::badge("deprecated")` The number of maximal attempts is now determined by the length of `delay_between_attempts`
#' @param delay_between_attempts A numeric vector specifying the delay in seconds between attempts of reaching the server
#' that `fhir_search()` will make. The length of this vector determines the number of attempts that will be made before stopping with an error.
#' Defaults to `c(1,3,9,27,81)`.
#' @param log_errors Either `NULL` or a character vector of length one indicating the name of a file in which to save the http errors.
#' `NULL` means no error logging. When a file name is provided, the errors are saved in the specified file. Defaults to `NULL`.
#' Regardless of the value of `log_errors` the most recent http error message within the current R session is saved internally and can
#' be accessed with [fhir_recent_http_error()].
#' @param save_to_disc Either `NULL` or a character vector of length one indicating the name of a directory in which to save the bundles.
#' If a directory name is provided, the bundles are saved as numerated xml-files into the directory specified
#' and not returned as a bundle list in the R session. This is useful when a lot of bundles are to be downloaded and keeping them all
#' in one R session might overburden working memory. When the download is complete, the bundles can be loaded into R using [fhir_load()].
#' Defaults to `NULL`, i.e. bundles are returned as a list within the R session.
#' @param delay_between_bundles A numeric scalar specifying a time in seconds to wait between pages of the search result,
#' i.e. between downloading the current bundle and the next bundle. This can be used to avoid choking a weak server with
#' too many requests to quickly. Defaults to zero.
#' @param rm_tag Character vector of length 1 defining an xml tag of elements that will removed from the bundle automatically.
#' Defaults to `"div"`,leading to the removal of all html parts (see Details). Set to `NULL` to keep the bundles untouched.
#' See [fhir_rm_div()] and [fhir_rm_tag()] for more info.
#' @return A [fhir_bundle_list-class] when `save_to_disc = NULL` (the default),  else `NULL`.
#' @export
#'
#' @seealso
#' - Creating a FHIR search request: [fhir_url()] and [fhir_body()] (for POST based search)
#' - OAuth2 Authentication: [fhir_authenticate()]
#' - Saving/reading bundles from disc: [fhir_save()] and [fhir_load()]
#' - Flattening the bundles: [fhir_crack()]
#' @importFrom lifecycle deprecated
#' @examples
#' \donttest{
#' #Search with GET
#' #create fhir search url
#' request <- fhir_url(url = "https://server.fire.ly",
#'                     resource = "Patient",
#'                     parameters = c(gender="female"))
#' #download bundles
#' bundles <- fhir_search(request, max_bundles = 5)
#'
#'
#' #Search with POST (should actually be used for longer requests)
#' request <- fhir_url(url = "https://server.fire.ly",
#'                     resource = "Patient")
#'
#' body <- fhir_body(content = list(gender = "female"))
#'
#' bundles <- fhir_search(request = request,
#'                        body = body,
#'                        max_bundles = 5)
#' }

fhir_search <- function(
	request                = fhir_current_request(),
	body                   = NULL,
	username               = NULL,
	password               = NULL,
	token                  = NULL,
	cookies                = NULL,
	max_bundles            = Inf,
	verbose                = 1,
	delay_between_attempts = c(1, 3, 9, 27, 81),
	log_errors             = NULL,
	save_to_disc           = NULL,
	delay_between_bundles  = 0,
	rm_tag                 = "div",
	max_attempts           = deprecated()
) {
	if(lifecycle::is_present(max_attempts)) {
		lifecycle::deprecate_warn(
			when = "2.0.0",
			what = "fhir_search(max_attempts)",
			details = "The number of maximal attempts is now controlled by the length of the argument delay_between_attempts."
		)
	}

	if(is.null(request)) {
		stop(
			"You have not provided a FHIR search request and there is no ",
			"current search request fhir_search() can fall back to. See documentation ",
			"for fhir_current_request()"
		)
	}

	#Extract base URL
	base <- stringr::str_match(request, ".*:\\/\\/.*?\\/")

	#preparation for POST vs. GET
	if(!is.null(body)) {
		#filter out bad urls
		if(grepl("\\?", request)) {
			stop(
				"The url in argument request should only consist of base url and resource type. ",
				"The one you provided has a `?` which indicates the presence of parameters. ",
				"If your request just ends with a `?`, please remove it to remove this error.\n",
				"If you want to perform search via GET, please set body to NULL."
			)
		}
		if(!grepl("_search", request)) {request <- paste(request, "_search", sep = "/")}

		#convert body to appropriate class
		if(is.character(body)) {
			body <- fhir_body(content = body, type = "application/x-www-form-urlencoded")
		} else if(is(body, "fhir_body")) {
			if(body@type != "application/x-www-form-urlencoded") {
				stop("The (content) type of the body for a search via POST must be `application/x-www-form-urlencoded`")
			}
		} else {
			stop("The body must be either of type character or of class fhir_body")
		}

		#startup message
		if(0 < verbose) {
			message(
				"Initializing search via POST from FHIR base URL ",
				base,
				".\n"
			)
		}
	} else if(0 < verbose) {
		message(
			"Starting download of ",
			if(max_bundles < Inf) {max_bundles} else {"all"},
			" bundles of resource type ",
			stringr::str_extract(request, "(?<=/)([^/\\?]*)(?=\\?|$)"),
			" from FHIR base URL ",
			base,
			".\n"
		)
		if(9 < max_bundles) {message("This may take a while...")}
	}

	bundles <- list()
	addr <- fhir_url(url = request)

	#download bundles
	cnt <- 0
	repeat {
		cnt <- cnt + 1
		if(1 < verbose) {message("bundle[", cnt, "]", appendLF = FALSE)}

		bundle <- get_bundle(
			request = addr,
			body = body,
			username = username,
			password = password,
			token = token,
			cookies = cookies,
			verbose = verbose,
			max_attempts = max_attempts,
			delay_between_attempts = delay_between_attempts,
			log_errors = log_errors,
			rm_tag = rm_tag
		)

		if(is.null(bundle)) {
			if(0 < verbose) {
				message("Download interrupted.")
			}
			break
		}

		if(!is.null(save_to_disc)) {
			if (!dir.exists(save_to_disc)) {
				dir.create(path = save_to_disc, recursive = TRUE)
			}
			xml2::write_xml(
				x    = bundle,
				file = pastep(save_to_disc, cnt, ext = ".xml")
			)
		} else {
			bundles[[length(bundles) + 1]] <- bundle
		}
		if(cnt == max_bundles) { #stop because max_bundles is reached
			if(0 < verbose) {
				if(0 < length(bundle@next_link)) {
					message(
						"\nDownload completed. Number of downloaded bundles was limited to ",
						cnt,
						" bundles, this is less than the total number of bundles available."
					)
					assign(x = "last_next_link", value = bundle@next_link, envir = fhircrackr_env)
				} else {
					message("\nDownload completed. All available bundles were downloaded.")
				}
			}
			break
		} else { #finished because there are no more bundles
			assign(x = "last_next_link", value = new("fhir_url"), envir = fhircrackr_env)
		}

		if(length(bundle@next_link) == 0) {
			if(0 < verbose) {
				message("\nDownload completed. All available bundles were downloaded.")
			}
			break
		}

		if(grepl("^/", bundle@next_link)){#when next links are relative
			addr <- paste0(base,
						   stringr::str_sub(bundle@next_link, start = 2))
		}else{
			addr <- bundle@next_link
		}



		if(0 < delay_between_bundles) {
			Sys.sleep(delay_between_bundles)
		}
	}

	fhircrackr_env$current_request <- request

	if(is.null(save_to_disc)) {fhir_bundle_list(bundles)} else {NULL} #brauchts eigentlich auch nicht
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
#' obs <- fhir_table_description(resource = "Patient")
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
fhir_next_bundle_url <- function(bundle = NULL) {

	if(!is.null(bundle)) {

		if(!is(bundle, "fhir_bundle")) {
			stop("bundle must be an object of type fhir_bundle")
		}

		if(is(bundle, "fhir_bundle_xml")) {
			bundle@next_link
		} else {
			b <- fhir_unserialize(b)
			b@next_link
		}

	} else {
		fhircrackr_env$last_next_link
	}
}

#' Return FHIR search request used in last call to [fhir_search()] or [fhir_url()]
#'
#' @return An object of class [fhir_url()]
#'
#' @examples
#' \donttest{
#' request <- fhir_url(url = "https://server.fire.ly", resource = "Patient")
#' fhir_current_request()
#'
#' fhir_search("https://server.fire.ly/Medication", max_bundles = 1)
#' fhir_current_request()
#' }
#'
#'
#' @export


fhir_current_request <- function() {
	fhircrackr_env$current_request
}


#' Return most recent http error from [fhir_search()]
#'
#' Whenever a call to [fhir_search()] produces any http error, the error information is saved internally
#' until the next http error occurs (or the R session ends). The error information can be accessed with `fhir_recent_http_error`.
#' If you want to log that information outside of your R session, set the argument `log_errors` of [fhir_search()] .
#' @return A string containing the error message
#'
#' @examples
#' \dontrun{
#' fhir_search("https://server.fire.ly/Medicatio", max_bundles = 1)
#' cat(fhir_recent_http_error())
#' }
#'
#' @seealso [fhir_search()]
#' @export

fhir_recent_http_error <- function() {
	fhircrackr_env$recent_http_error
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
#' @param username A character vector of length one containing the username for basic authentication. Defaults to NULL, meaning no authentication.
#' @param password A character vector of length one containing the password for basic authentication. Defaults to NULL, meaning no authentication.
#' @param token A character vector of length one or object of class [httr::Token-class], for bearer token authentication (e.g. OAuth2). See [fhir_authenticate()]
#' for how to create this.
#' @param cookies Optional. A named character vector containing key value pairs for cookies, e.g. `c(mycookie = "d385se12394j")`.
#' @param sep A character vector of length one to separate pasted multiple entries
#' @param brackets A character vector of length two defining the brackets surrounding indices for multiple entries, e.g. `c( "[", "]")`. Defaults to `NULL`.
#' If `NULL`, no indices will be added to multiple entries.
#' @param log_errors Either `NULL` or a character vector of length one indicating the name of a file in which to save the http errors.
#' `NULL` means no error logging. When a file name is provided, the errors are saved in the specified file. Defaults to `NULL`
#' @param verbose `r lifecycle::badge("deprecated")`
#' @return A list of data frames containing the information from the statement
#' @export
#'
#' @examples
#' \dontrun{
#' #without indices
#' cap <- fhir_capability_statement(url = "https://server.fire.ly")
#'
#' #with indices
#' cap <- fhir_capability_statement(url = "https://server.fire.ly",
#'                                  brackets = c("[","]"),
#'                                  sep = " || ")
#'
#' #melt searchInclude variable
#' resources <- fhir_melt(cap$Resources,
#'                        columns = "searchInclude",
#'                        brackets = c("[", "]"),
#'                        sep = " || ",
#'                        all_columns = FALSE)
#'
#' #remove indices
#' resources <- fhir_rm_indices(resources, brackets = c("[", "]"))
#'
#' head(resources)
#'}

fhir_capability_statement <- function(
	url = "https://hapi.fhir.org/baseR4",
	username = NULL,
	password = NULL,
	token = NULL,
	cookies = NULL,
	brackets = NULL,
	sep = " ::: ",
	log_errors = NULL,
	verbose = deprecated()) {

	if(lifecycle::is_present(verbose)){
		lifecycle::deprecate_warn(when = "2.0.0", what = "fhir_capability_statement(verbose)")
	}

	resource <- NULL #To stop "no visible binding" NOTE in check()

	use_indices <- FALSE
	if(!is.null(brackets)) {
		bra <- brackets[1]
		ket <- brackets[2]
		use_indices <- TRUE
	}

	auth <- auth_helper(username = username, password = password, token = token, cookies = cookies)

	response <- try(httr::GET(
		url = pastep(url, "metadata?"),
		config = httr::add_headers(
			Accept = "application/fhir+xml",
			Authorization = auth$token
		),
		auth$basicAuth,
		auth$cookies
	), silent = TRUE)

	#check for http errors
	check_response(response = response, log_errors = log_errors)

	#extract payload
	payload <- httr::content(x = response, as = "text", encoding = "UTF-8")
	xml <- xml2::read_xml(x = payload)
	xml <- fhir_ns_strip(xml)

	xml_meta <- xml2::xml_new_root(.value = xml, .copy = TRUE)
	xml2::xml_remove(.x = xml2::xml_find_all(x = xml_meta, xpath = "/CapabilityStatement/rest"))

	xml_rest <- xml2::xml_new_root(.value = xml, .copy = TRUE)
	xml2::xml_remove(.x = xml2::xml_find_all(x = xml_rest, xpath = "/CapabilityStatement/rest/resource"))


	#### META ####
	META <- data.table(
		node   = xml2::xml_find_all(
			xml_meta,
			'//@*'
		)
	)
	(META[, path     := xml2::xml_path(node) |> busg('/CapabilityStatement/', '')|> busg('([^]])/', '\\1[1]/')] # add missing indices
		[, value    := xml2::xml_text(node)] # get value
		[, path     := path |> busg('@.*', '')] # remove attribute from path
		#[, spath    := path |> busg('^[^/]+/[^/]+/[^/]+/','')] # remove 'Bundle/entry/resource' from paths
		[, xpath    := path |> busg('\\[[0-9]+]*/', '/') |> busg('\\/$', '')]
		[, column   := xpath |> busg('/', '.')]
	)
	if(use_indices) {
		META <- (META[, id := path |> busg('[^0-9]+', '.') |> busg('(^\\.)|(\\.$)', '')][, paste0(bra, id, ket, value, collapse = sep), by='column'] |>
				 	 data.table::transpose(make.names = 'column'))
	} else {
		META <- (META[, paste0(value, collapse = sep), by='column'] |> data.table::transpose(make.names = 'column'))
	}
	###########

	#### REST ####
	REST <- data.table(
		node   = xml2::xml_find_all(
			xml_rest,
			'/CapabilityStatement/rest//@*'
		)
	)
	(REST[, path     := xml2::xml_path(node) |> busg('/CapabilityStatement/', '')|> busg('([^]])/', '\\1[1]/')] # add missing indices
		[, value    := xml2::xml_text(node)] # get value
		[, path     := path |> busg('@.*', '')] # remove attribute from path
		[, spath    := path |> busg('^[^/]+/','')] # remove 'rest' from paths
		[, xpath    := spath |> busg('\\[[0-9]+]*/', '/') |> busg('\\/$', '')]
		[, column   := xpath |> busg('/', '.')]
	)
	if(use_indices) {
		REST<- (REST[, id := spath |> busg('[^0-9]+', '.') |> busg('(^\\.)|(\\.$)', '')][, paste0(bra, id, ket, value, collapse = sep), by='column'] |>
					 data.table::transpose(make.names = 'column'))
	} else {
		REST <- (REST[, paste0(value, collapse = sep), by='column'] |>  data.table::transpose(make.names = 'column'))
	}
	#########

	#### RECOURCE ####
	RESOURCE <- data.table(
		node   = xml2::xml_find_all(
			xml,
			'/CapabilityStatement/rest/resource//@*'
		)
	)
	(RESOURCE[, path     := xml2::xml_path(node) |> busg('/CapabilityStatement/', '')|> busg('([^]])/', '\\1[1]/')] # add missing indices
		[, value    := xml2::xml_text(node)] # get value
		[, path     := path |> busg('@.*', '')] # remove attribute from path
		[, path     := path |> busg('rest\\[([0-9]+)]/', '')] #remove rest[1] from path
		[, resource    := path |> busg('resource\\[([0-9]+)].*', '\\1') |> as.integer()] # enumerate resource
		[, spath    := path |> busg('^[^/]+/','')] # remove 'rest/resource' from paths
		[, xpath    := spath |> busg('\\[[0-9]+]*/', '/') |> busg('\\/$', '')]
		[, column   := xpath |> busg('/', '.')]
	)
	if(use_indices) {
		RESOURCE<- (RESOURCE[, id := spath |> busg('[^0-9]+', '.') |> busg('(^\\.)|(\\.$)', '')][, paste0(bra, id, ket, value, collapse = sep), by=c('resource', 'column')] |>
					dcast(resource ~ column, value.var = 'V1'))[,-c('resource')]
	} else {
		RESOURCE <- (RESOURCE[, paste0(value, collapse = sep), by=c('resource', 'column')] |> dcast(resource ~ column, value.var = 'V1'))[,-c('resource')]
	}

	list(Meta = META, Rest = REST, Resources = RESOURCE)
}

####Saving Bundles####

#' Save FHIR bundles as xml-files
#' @description Writes a list of FHIR bundles as numbered xml files into a directory.
#'
#' @param bundles A list of xml objects representing the FHIR bundles.
#' @param directory A character vector of length one containing the path to the folder to store the data in.
#' @export
#'
#' @examples
#' #unserialize example bundle
#' bundles <- fhir_unserialize(medication_bundles)
#'
#' #save all bundles to temporary directory
#' fhir_save(bundles, directory = tempdir())
#'
#' #save only two bundles (the second and the third) to temporary directory
#' fhir_save(bundles[c(2,3)], directory = tempdir())

fhir_save <- function(bundles, directory) {

	w <- 1 + floor(log10(length(bundles)))

	if(!dir.exists(directory)) {

		dir.create(path = directory, recursive = TRUE)
	}

	for(n in seq_len(length(bundles))) {

		xml2::write_xml(
			x    = bundles[[n]],
			file = pastep(
				directory,
				stringr::str_pad(n, width = w, pad = "0"),
				ext =  ".xml"
			)
		)
	}
}



#' Load bundles from xml-files
#' @description Reads all bundles stored as xml files from a directory.
#'
#' @param directory A character vector of length one containing the path to the folder were the files are stored.
#' @param indices A numeric vector of integers indicating which bundles from the specified directory should be loaded. Defaults to NULL meaning all bundles from the directory are loaded.
#' @param pattern A character vector of length one with a regex expression defining the naming pattern of the xml files
#' to be read. Defaults to the regex expression matching file names as produced by [fhir_save()].
#' @return A [fhir_bundle_list-class].
#' @export
#'
#' @examples
#' #unserialize example bundle
#' bundles <- fhir_unserialize(medication_bundles)
#' length(bundles)
#'
#' #save to temporary directory
#' dir <- tempdir()
#' fhir_save(bundles, directory = dir)
#'
#' #load from temporary directory
#' loaded_bundles <- fhir_load(dir)
#' length(loaded_bundles)
#'
#' #load only two, the second and the third bundle
#' loaded_bundles <- fhir_load(dir, indices = c(2,3))
#' length(loaded_bundles)

fhir_load <- function(directory, indices = NULL, pattern = '^[0-9]+\\.xml$') {

	if(!dir.exists(directory)) {

		warning("Cannot find the specified directory.")

		return(fhir_bundle_list(list()))
	}

	files <- grep(pattern, dir(directory), value = T)

	if(is.null(indices)) {

		indices <- seq_along(files)
	}

	if(any(indices > length(files))){stop("Indices are greater than number of files available in the directory")}

	chosen.files <- files[indices]

	if(length(chosen.files) < 1) {

		warning("Cannot find any xml-files in the specified directory.")

		return(fhir_bundle_list(list()))
	}

	fhir_bundle_list(
		bundles = lapply(
			lst(chosen.files),
			function(x) {
				xml2::read_xml(pastep(directory, x))
			}
		)
	)
}



#' Serialize a [fhir_bundle-class], [fhir_bundle_list-class] or [fhir_resource-class]
#'
#' @description Serializes FHIR bundles or resources to allow for saving in .rda or .RData format without losing integrity of pointers
#' i.e. it turns a [fhir_bundle_xml-class]/[fhir_resource_xml-class] object into an [fhir_bundle_serialized-class]/[fhir_resource_serialized-class] object.
#' @param bundles A [fhir_bundle-class], [fhir_bundle_list-class] or [fhir_resource-class] object.
#' @return A  [fhir_bundle_xml-class], [fhir_bundle_list-class] or [fhir_resource_xml-class]  object.
#' @export
#' @docType methods
#' @rdname fhir_serialize-methods
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
#'

setGeneric(
	name = "fhir_serialize",
	def = function(bundles) {
		standardGeneric("fhir_serialize")
	}
)

#' @rdname fhir_serialize-methods
#' @aliases fhir_serialize,fhir_bundle_xml-method
setMethod(
	f = "fhir_serialize",
	signature = c(bundles = "fhir_bundle_xml"),
	definition = function(bundles) {
		fhir_bundle_serialized(bundle = xml2::xml_serialize(object = bundles, connection = NULL))
	}
)

#' @rdname fhir_serialize-methods
#' @aliases fhir_serialize,fhir_bundle_serialized-method
setMethod(
	f = "fhir_serialize",
	signature = c(bundles = "fhir_bundle_serialized"),
	definition = function(bundles) {
		bundles
	}
)

#' @rdname fhir_serialize-methods
#' @aliases fhir_serialize,fhir_bundle_list-method
setMethod(
	f = "fhir_serialize",
	signature = c(bundles = "fhir_bundle_list"),
	definition = function(bundles) {
		if(is(bundles[[1]], "fhir_bundle_xml")) {
			fhir_bundle_list(
				lapply(
					bundles,
					xml2::xml_serialize,
					connection = NULL
				)
			)
		} else {
			bundles
		}
	}
)

#' @rdname fhir_serialize-methods
#' @aliases fhir_serialize,fhir_resource_xml-method
setMethod(
	f = "fhir_serialize",
	signature = c(bundles = "fhir_resource_xml"),
	definition = function(bundles) {
		fhir_resource_serialized(resource = xml2::xml_serialize(object = bundles, connection = NULL))
	}
)

#' @rdname fhir_serialize-methods
#' @aliases fhir_serialize,fhir_resource_serialized-method
setMethod(
	f = "fhir_serialize",
	signature = c(bundles = "fhir_resource_serialized"),
	definition = function(bundles) {
		bundles
	}
)

#' Unserialize a [fhir_bundle-class], [fhir_bundle_list-class] or [fhir_resource-class]
#'
#' @description Unserializes FHIR resources or bundles that have been serialized to allow for saving in .rda or .RData format,
#' i.e. it turns a [fhir_bundle_serialized-class]/[fhir_resource_serialized-class] object into an [fhir_bundle_xml-class]/[fhir_resource_xml-class] object.
#' @param bundles A [fhir_bundle-class], [fhir_bundle_list-class] or [fhir_resource-class] object.
#' @return A  [fhir_bundle_serialized-class], [fhir_bundle_list-class] or [fhir_resource_serialized-class]object.
#' @export
#' @docType methods
#' @rdname fhir_unserialize-methods
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
	name = "fhir_unserialize",
	def = function(bundles) {
#		standardGeneric(f = "fhir_unserialize")
		standardGeneric("fhir_unserialize")
	}
)

#' @rdname fhir_unserialize-methods
#' @aliases fhir_unserialize,fhir_bundle_xml-method
setMethod(
	f = "fhir_unserialize",
	signature = c(bundles = "fhir_bundle_xml"),
	definition = function(bundles) {
		bundles
	}
)

#' @rdname fhir_unserialize-methods
#' @aliases fhir_unserialize,fhir_bundle_serialized-method
setMethod(
	f = "fhir_unserialize",
	signature = c(bundles = "fhir_bundle_serialized"),
	definition = function(bundles) {
		b <- xml2::xml_unserialize(connection = bundles)
		fhir_bundle_xml(bundle = b)
	}
)

#' @rdname fhir_unserialize-methods
#' @aliases fhir_unserialize,fhir_resource_xml-method
setMethod(
	f = "fhir_unserialize",
	signature = c(bundles = "fhir_resource_xml"),
	definition = function(bundles) {
		bundles
	}
)

#' @rdname fhir_unserialize-methods
#' @aliases fhir_unserialize,fhir_resource_serialized-method
setMethod(
	f = "fhir_unserialize",
	signature = c(bundles = "fhir_resource_serialized"),
	definition = function(bundles) {
		b <- xml2::xml_unserialize(connection = bundles)
		fhir_resource_xml(b)
	}
)

#' @rdname fhir_unserialize-methods
#' @aliases fhir_unserialize,fhir_bundle_list-method
setMethod(
	f = "fhir_unserialize",
	signature = c(bundles = "fhir_bundle_list"),
	definition = function(bundles) {
		if(is(bundles[[1]], "fhir_bundle_xml")){
			bundles
		} else {
			fhir_bundle_list(lapply(bundles, xml2::xml_unserialize))
		}
	}
)

#' Create token for Authentication
#'
#' @description
#' This function is a wrapper to create an [httr::Token] object for authentication with OAuth2/OpenID Connect.
#' Internally, it calls [httr::oauth_app()], [httr::oauth_endpoint()] and [httr::oauth2.0_token()] to create a token that can
#' then be used in [fhir_search].
#'
#' @param key Consumer key, also called client ID.
#' For Keycloak this would for instance be the Keycloak client, e.g. "postman".
#' @param secret The consumer/client secret, belonging to `key`.
#' @param base_url The URL the user will be redirected to after authorization is complete.
#' This will usually be the base url of you FHIR server.
#' @param authorize The url to send the client for authorization.
#' @param access The url used to exchange unauthenticated for authenticated token.
#' This can be identical to `authorize`.
#' @param query_authorize_extra A named list holding query parameters to append to initial auth page query.
#' Could hold info about user identity and scope for keycloak like this:
#' ```
#' list(scope = "openid",
#'      grant_type = "password",
#'      username = "fhir-user",
#'      password = "fhirtest")
#' ```
#' @export

fhir_authenticate <- function(
	secret,
	key,
	base_url,
	access,
	authorize,
	query_authorize_extra = list()) {

	#Initialize app
	app <- httr::oauth_app(
		appname = key,#could be any name
		key = key,
		secret = secret,
		redirect_uri = base_url
	)
	#set endpoint
	endpoint <- httr::oauth_endpoint(access = access, authorize = authorize)
	#Create Token
	t_ <- httr::oauth2.0_token(
		endpoint = endpoint,
		app = app,
		client_credentials = TRUE,
		cache = FALSE,
		query_authorize_extra = query_authorize_extra
	)
	if(names(t_$credentials)[1] == "error") {
		stop(
			"The token could not be created.\n\n",
			"Error code: ", t_$credentials$error, "\n",
			"Error description: ", t_$credentials$error_description, "\n")
	}

	t_
}


#################################################################################################
#################################################################################################

#' Download single FHIR bundle
#' @description Download a single FHIR bundle via FHIR search request and return it as a xml object.
#'
#' @param request An object of class [fhir_search_url-class] or character vector of length one containing the full FHIR search request.
#' @param username A character vector of length one containing the username for basic authentication. Defaults to NULL, meaning no authentication.
#' @param password A character vector of length one containing the password for basic authentication. Defaults to NULL, meaning no authentication.
#' @param token The token for token based auth, either a string or a httr token object
#' @param cookies Optional. A named character vector containing key value pairs for cookies, e.g. `c(mycookie = "d385se12394j")`.
#' @param max_attempts Deprecated. The number of maximal attempts is determined by the length of `delay_between_attempts`
#' @param verbose An integer scalar. If > 1,  Downloading progress is printed. Defaults to 2.
#' @param delay_between_attempts A numeric vector specifying the delay in seconds between attempts of reaching the server
#' that `fhir_search()` will make. The length of this vector determines the number of attempts that will be made before stopping with an error.
#' Defaults to `c(1,3,9,27,81)`.
#' @param log_errors Either `NULL` or a character vector of length one indicating the name of a file in which to save the http errors.
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
	body = NULL,
	username = NULL,
	password = NULL,
	token = NULL,
	cookies = NULL,
	verbose = 2,
	max_attempts = NULL,
	delay_between_attempts = c(1,3,9,27,81),
	log_errors = NULL,
	rm_tag = "div") {

	#download response
	for(n in seq_along(delay_between_attempts)) {
		if(1 < verbose) {message("(", n, "): ", request)}

		auth <- auth_helper(username = username, password = password, token = token, cookies = cookies)

		#paging is implemented differently for Hapi/Vonk When initial request is POST
		#VonK: Next-Links have to be POSTed, Hapi: Next-Links have to be GETed
		#search via POST
		if(grepl("_search", request)) {
			response <- try(httr::POST(
				url = request,
				config = httr::add_headers(
					Accept = "application/fhir+xml",
					Authorization = auth$token
				),
				httr::content_type(type = body@type),
				auth$basicAuth,
				auth$cookies,
				body = body@content
			), silent = TRUE)

		} else {#search via GET
			response <- try(httr::GET(
				url = request,
				config = httr::add_headers(
					Accept = "application/fhir+xml",
					Authorization = auth$token
				),
				auth$basicAuth,
				auth$cookies
			), silent = TRUE)
		}
		#check for errors
		check_response(response = response, log_errors = log_errors)

		#extract payload
		payload <- try(httr::content(x = response, as = "text", encoding = "UTF-8"), silent = TRUE)
		if(class(payload)[1] != "try-error") {
			xml <- try(xml2::read_xml(x = payload), silent = TRUE)
			if(class(xml)[1] != "try-error") {
				bundle <- fhir_bundle_xml(bundle = xml)
				if(!is.null(rm_tag)){bundle <- fhir_rm_tag(x = bundle, tag = rm_tag)}
				return(bundle)
			}
		}
		Sys.sleep(delay_between_attempts[n])
	}
	NULL
}

#'log the error message of a http response
#'
#' @param response A http response
#' @param log_errors A character vector of length one indicating the name of a file in which to save the http errors.
#' @param append Append to existing file?
#' @noRd
#'
error_to_file <- function(response, log_errors, append) {
	payload <- httr::content(x = response, as = "text", encoding = "UTF-8")
	write(x = payload, file = log_errors, append = append)
}
#' Check http response
#'
#' Checks the http response and issues an error or warning if necessary
#'
#' @param response A http response
#' @param log_errors Either `NULL` or a character vector of length one indicating the name of a file in which to save the http errors.
#' `NULL` means no error logging. When a file name is provided, the errors are saved in the specified file.
#' @param append Append message to existing file? Defaults to FALSE
#' @noRd
#'
#'
check_response <- function(response, log_errors, append = FALSE) {

	#Error in curl
	if(is(response, "try-error")){
		if(!is.null(log_errors)){
			write(x = response, file = log_errors)
			stop("The server could not be reached:\n", response, ".\n",
				 "This has been logged in the generated error file.")
		}else{
			stop("The server could not be reached:\n", response, ".\n")
		}
	}

	#http error
	code <- response$status_code
	if(code >= 400){
		fhircrackr_env$recent_http_error <- httr::content(x = response, as = "text", encoding = "UTF-8")
	}
	if(code >= 400 && !is.null(log_errors)) {
		error_to_file(response = response, log_errors = log_errors, append = append)
	}
	if(code == 400) {
		if (!is.null(log_errors)) {
			stop(
				"HTTP code 400 - This can be caused by an invalid request or a server issue. ",
				"For more information see the generated error file."
			)
		} else {
			stop(
				"HTTP code 400 - This can be caused by an invalid request or a server issue. ",
				"To print more detailed error information, run fhir_recent_http_error() or set argument log_errors to a filename and rerun you request."
			)
		}
	}
	if(code == 401) {
		if(!is.null(log_errors)) {
			stop("HTTP code 401 - Authentication needed. For more information see the generated error file.")
		} else {
			stop(
				"HTTP code 401 - Authentication needed. To print more detailed error information,",
				"run fhir_recent_http_error() or set argument log_errors to a filename and rerun you request."
			)
		}
	}
	if(code == 404) {
		if(!is.null(log_errors)) {
			stop("HTTP code 404 - Not found. Did you misspell the resource? For more information see the generated error file.")
		} else {
			stop(
				"HTTP code 404 - Not found. Did you misspell the resource? To print more detailed error information, ",
				"run fhir_recent_http_error() or set argument log_errors to a filename and rerun you request."
			)
		}
	}
	if(300 <= code && code < 400) {
		if(!is.null(log_errors)) {
			warning(
				"Your request generated a HTTP code ",
				code,
				". For more information see the generated error file ."
			)
		} else {
			warning(
				"Your request generated a HTTP code ",
				code,
				". To print more detailed error information, run fhir_recent_http_error() or set argument log_errors to a filename and rerun you request.")
		}
	}
	if(400 <= code && code < 500) {
		if(!is.null(log_errors)) {
			stop(
				"Your request generated a client error, HTTP code ",
				code,
				". For more information see the generated error file."
			)
		} else {
			stop(
				"Your request generated a client error, HTTP code ",
				code,
				". To print more detailed error information, run fhir_recent_http_error() or set argument log_errors to a filename and rerun you request."
			)
		}
	}
	if(500 <= code && code < 600) {
		if(!is.null(log_errors)) {
			stop(
				"Your request generated a server error, HTTP code ",
				code,
				". For more information see the generated error file."
			)
		} else {
			stop(
				"Your request generated a server error, HTTP code ",
				code,
				". To print more detailed error information, run fhir_recent_http_error() or set argument log_errors to a filename and rerun you request."
			)
		}
	}
}
