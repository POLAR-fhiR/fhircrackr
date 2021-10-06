#' Order Strings with Numbers correctly
#'
#' @param s A character containing the strings to be ordered.
#'
#' @return A character containing the order of the given strings.
#' @noRd
#' @examples
#' s <- c("12", "2", "1", "21")
#' order(s)
#' order_strings_with_numbers_correctly(s = s)
order_strings_with_numbers_correctly <- function(s) {
	sc <- as.character(s)
	ss <- stringr::str_extract_all(sc, "[0-9]+")
	i  <- 1
	max_char <- max(sapply(ss, function(x) {if(0 < length(x) && !is.na(x)) max(nchar(x), na.rm = T) else 0}))
	s_numbers <- strsplit(sc, "[^0-9]+")
	s_letters <- strsplit(sc, "[0-9]+")
	order(
		sapply(
			seq_along(ss),
			function(i) {
				sn <- s_numbers[[i]]
				sl <- s_letters[[i]]
				if(length(sn) < length(sl)) {
					sn <- c(sn, "")
				} else if(length(sl) < length(sn)) {
					sl <- c(sl, "")
				}
				sn <- ifelse(!is.na(sn) && sn != "", stringr::str_pad(sn, max_char, "left", "0"), "")
				if(sn[1] != "") paste0(sl, sn, collapse = "") else paste0(sn, sl, collapse = "")
			}
		)
	)
}



#' Sort Strings with Numbers correctly
#'
#' @param s A character containing the strings to be sorted.
#'
#' @return A character containing the the given strings sorted.
#' @noRd
#' @examples
#' s <- c("12", "2", "1", "21")
#' sort(s)
#' sort_strings_with_numbers_correctly(s = s)
sort_strings_with_numbers_correctly <- function(s) {
	if(is.null(s)){
		return(s)
	}
	sc <- as.character(s)
	s[order_strings_with_numbers_correctly(s)]
}



#' Paste Parameters for a FHIR Search Request
#'
#' @param parameters Either a length 1 character containing properly formatted FHIR search parameters, e.g.
#' `"gender=male&_summary=count"` or a named list or named character vector e.g. `list(gender="male", "_summary"="count")`
#' or `c(gender="male", "_summary"="count")`. Note that parameter names beginning with `_` have to be put in quotation marks!
#'
#' @param parameters2add Either a length 1 character containing properly formatted FHIR search parameters, e.g.
#' `"gender=male&_summary=count"` or a named list or named character vector e.g. `list(gender="male", "_summary"="count")`
#' or `c(gender="male", "_summary"="count")`. Note that parameter names beginning with `_` have to be put in quotation marks!
#'
#' @param add_question_sign Logical. Should an ?-Sign to be prefixed. Defaults to FALSE.
#'
#' @return A character of length 1 representing the pasted parameter string.
#' @noRd
#'
#' @examples
#' paste_parameters(
#'   parameters = "gender=male",
#'   parameters2add = list("_summary" = "count", "_count" = "1"),
#'   add_question_sign = TRUE)

paste_parameters <- function(parameters = NULL, parameters2add = NULL, add_question_sign = F) {
	convert <- function(arg) {
		n <- names(arg)
		if(length(n) < length(arg)) {
			if(0 == length(n) && length(arg) == 1) {
				arg
			}
		} else {
			paste0(
				sapply(
					seq_along(arg),
					function(i) {
						paste0(n[i], "=", arg[i])
					}
				),
				collapse = "&"
			)
		}
	}
	pre  = convert(parameters)
	post = convert(parameters2add)
	s <- if(0 < nchar(pre) && 0 < nchar(post)) {
		paste0(pre, "&", post)
	} else if(0 < nchar(pre)) {
		pre
	} else {
		post
	}
	if(0 < nchar(s) && substr(s, 1, 1) != "?" && add_question_sign) {
		s <- paste0("?", s)
	}
	s
}



#' Get Resources' Counts
#'
#' Downloads a count of resources matching the resource type and search parameters specified in `resource` and `parameters`.
#' This function makes use of the `_summary=count` parameter of FHIR search and is therefore able to count resources on the server
#' without actually downloading them.
#'
#' @param base_url A character vector of length one specifying the base URL of the FHIR server, e.g. `"http://hapi.fhir.org/baseR4"`.
#' @param resource A character vector of length one or [fhir_resource_type-class] object with the resource type to be searched, e.g. `"Patient"`.
#' @param parameters Optional. Either a length 1 character vector containing properly formatted FHIR search parameters, e.g.
#' `"gender=male&_summary=count"` or a named list or named character vector e.g. `list(gender="male", "_summary"="count")`
#' or `c(gender="male", "_summary"="count")`. Note that parameter names beginning with `_` have to be put in quotation marks!
#'
#' @return An integer of length 1 containing the number of resources matching the type and search parameters specified in `resource` and `parameters`.
#' @export
#'
#' @examples
#' \donttest{
#' #number of female Patient resources on the server
#' fhir_count_resource(
#'   base_url = 'https://vonk.fire.ly/R4',
#'   resource = "Patient",
#'   parameters = c(gender = "female"))
#'}
fhir_count_resource <- function(base_url, resource, parameters = NULL) {
	as.numeric(
		xml2::xml_attr(
			xml2::xml_find_first(
				fhir_search(
					fhir_url(
						url = base_url,
						resource = resource,
						parameters = paste_parameters(parameters = parameters, parameters2add = c("_summary" = "count"), add_question_sign = F),
					),
					verbose = 0
				)[[1]],
				"//total"
			),
			"value"
		)
	)
}



#' Get Resources' IDs
#'
#' Download the resource (aka logical) IDs of all resources matching the FHIR search request build from the resource type and search
#' parameters specified in `resource` and `parameters`. This function does not download the entire resources, but only extracts their IDs
#' using the `_elements` parameter of FHIR Search.
#'
#' @param base_url A character vector of length one specifying the base URL of the FHIR server, e.g. `"http://hapi.fhir.org/baseR4"`.
#' @param resource A character vector of length one or [fhir_resource_type-class] object with the resource type to be searched, e.g. `"Patient"`.
#' @param parameters Optional. Either a length 1 character vector containing properly formatted FHIR search parameters, e.g.
#' `"gender=male&_summary=count"` or a named list or named character vector e.g. `list(gender="male", "_summary"="count")`
#' or `c(gender="male", "_summary"="count")`. Note that parameter names beginning with `_` have to be put in quotation marks!
#' @param verbose An integer of length 1 containing the level of verbosity. Defaults to 0.
#'
#' @return A character vector containing the resource (aka logical) IDs of all requested resources.
#' @export
#'
#' @examples
#' \donttest{
#' fhir_get_resource_ids(
#'   base_url   = 'https://vonk.fire.ly/R4',
#'   resource   = "Patient",
#'   parameters = "gender=female", verbose=1)
#'   }
fhir_get_resource_ids <- function(base_url, resource, parameters = NULL, verbose = 0) {
	if(0 < verbose) {
		message(
			paste0(
				"Download ",
				fhir_count_resource(
					base_url = base_url,
					resource = resource,
					parameters = paste_parameters(parameters)
				),
				" ",
				resource,
				"s' IDs...\n"
			)
		)
	}

	request <-  fhir_url(
					url = base_url,
					resource = resource,
					parameters = paste_parameters(parameters, c("_elements" = "id", "_count" = "500"))
				)

	bundles <- 	fhir_search(
					request = request,
					verbose = 0
				)

	unlist(
		lapply(
			bundles,
			function(bundle) {
				xml2::xml_attr(
					xml2::xml_find_all(
						bundle,
						paste0(
							"entry/resource/",
							resource,
							"/id"
						)
					),
					"value"
				)
			}
		)
	)

}


#' Get Resources by their IDs
#'
#' Downloads FHIR resources represented by a vector of resource IDs.
#'
#' @details
#' This function takes a character vector `ids` containing logical Ids of resources of a given type (specified in `resource`) on a
#' FHIR server (specified in `base_url`) and downloads the corresponding resources from the server. The function will attempt to download the resources using a
#' FHIR search request via POST where the IDs are part of the body. See [fhir_search()] for details. If this fails
#' (e.g. because the server doesn't allow POST operations), the function falls back on a GET request. If the set of ids is too long to fit
#' into one GET request (i.e. if the request gets longer than 2083 characters), it will be spread across several requests.
#' @param base_url A character vector of length one specifying the base URL of the FHIR server, e.g. `"http://hapi.fhir.org/baseR4"`.
#' @param resource A character vector of length one or [fhir_resource_type-class] object with the resource type to be searched, e.g. `"Patient"`.
#' @param ids A character vector containing the resource (aka logical) IDs of the resources that should be downloaded.
#' @param verbose An integer vector of length 1 containing the level of verbosity. Defaults to 0.
#'
#' @return A [fhir_bundle_list-class] containing the downloaded resources.
#' @export
#'
#' @examples
#' \donttest{
#' #find IDs of Patient resources representing Homer Simpson
#' ids <- fhir_get_resource_ids(
#'   base_url   = 'https://hapi.fhir.org/baseR4',
#'   resource   = "Patient",
#'   parameters = "name=Homer&name=Simpson")
#'
#' #Download all corresponding resources
#' bundles <- fhir_get_resources_by_ids(
#'   base_url = 'https://hapi.fhir.org/baseR4',
#'   resource = "Patient",
#'   ids      = ids)
#'
#' #have a look at the resources
#' fhir_crack(
#'   bundles,
#'   fhir_table_description(
#'     resource = "Patient",
#'     cols     = list(
#'       ID      = "id",
#'       given   = "name/given",
#'       family = "name/family")))
#'}
fhir_get_resources_by_ids <- function(base_url, resource, ids, verbose = 0) {

	#download via GET
	get_resources_by_ids_get <- function(base_url, resource, ids, verbose = 1) {
		collect_ids_for_request <- function(ids, max_ids = length(ids), max_len = 2083 - sum(nchar(base_url),nchar(resource),50)) {
			if(length(ids) < 1) {
				warning(
					paste0(
						"The length of ids is zero. So no single id is added to the list."
					)
				)
				list(str = "", n = 0)
			} else if(max_len < nchar(ids[[1]])) {
				warning(
					paste0(
						"The length of the first id string ",
						ids[[1]],
						" is already greater than max_len=",
						max_len,
						", i.e. the maximal allowed length of the request. So no single id is added to the list."
					)
				)
				list(str = "", n = 0)
			} else {
				n <- 1
				s <- ids[[n]]
				while(n < max_ids && n < length(ids) && nchar(s) + nchar(ids[[n + 1]]) < max_len) {
					n <- n + 1
					s <- paste0(s, ",", ids[[n]])
				}
				list(str = s, n = n)
			}
		}
		bundles <- list()
		total <- 1
		bundle_count <- 1
		while(0 < length(ids)) {
			ids_ <- collect_ids_for_request(ids = ids, max_ids = length(ids))
			url_ <- paste0(paste_paths(base_url, resource), "?_id=", ids_$str)
			bnd_ <- fhir_search(request = url_, verbose = 0)
			total <- total + ids_$n
			ids <- ids[-seq_len(ids_$n)]
			bundles <- c(bundles, bnd_)
			cat(paste0("Bundle ", bundle_count, " a ", ids_$n, " ", resource, "s  \u03A3 ", total - 1, "\n"))
			bundle_count <- bundle_count + 1
		}
		fhir_bundle_list(bundles)
	}

	#download via POST
	get_resources_by_ids_post <- function(base_url, resource, ids, verbose = 1) {
		fhir_search(
			fhir_url(
				url = base_url,
				resource = resource,
				url_enc = T
			),
			fhir_body(
				content = list(
					"_id" = paste0(ids, collapse = ","),
					"_count" = "500"
				),
				type = "application/x-www-form-urlencoded"
			),
			verbose = verbose
		)
	}

	bundles <- try(get_resources_by_ids_post(base_url = base_url, resource = resource, ids = ids, verbose = verbose), silent = T)
	if(inherits(bundles, "try-error")) {
		if(verbose>0){message("Search via POST failed, falling back to iterative download via GET")}
		bundles <- get_resources_by_ids_get(base_url = base_url, resource = resource, ids = ids, verbose = verbose)
	}
	bundles
}



#' Download random sample if resource ID list from a FHIR server
#'
#' Downloads a random sample of resources from a vector of resource IDs.
#'
#' @details
#' This function takes a character vector `ids` containing logical Ids of resources of a given type (specified in `resource`) on a
#' FHIR server (specified in `base_url`) and downloads a random sample of size `sample_size` of the corresponding resources from the server.
#'
#' Internally, the download of the resources is done by [fhir_get_resources_by_ids()]. This function will attempt to download the resources using a
#' FHIR search request via POST where the IDs are part of the body. See [fhir_search()] for details. If this fails
#' (e.g. because the server doesn't allow POST operations), the function falls back on a GET request. If the set of IDs is too long to fit
#' into one GET request (i.e. if the request gets longer than 2083 characters), it will be spread across several requests.
#'
#' @param base_url A character vector of length one specifying the base URL of the FHIR server, e.g. `"http://hapi.fhir.org/baseR4"`.
#' @param resource A character vector of length one or [fhir_resource_type-class] object with the resource type to be downloaded e.g. `"Patient"`.
#' @param ids A character vector containing the IDs from which to sample.
#' @param sample_size A integer of length 1 containing the number of resources to sample.
#' @param seed A integer of length 1 containing the seed for the random generator.
#' @param verbose An integer of length 1 containing the level of verbosity. Defaults to 1.
#'
#' @return A list of bundles containing sampled resources.
#' @export
#'
#' @examples
#' \donttest{
#' #find IDs of all resources representing Homer Simpson
#' ids <- fhir_get_resource_ids(
#'   base_url   = 'https://hapi.fhir.org/baseR4',
#'   resource   = "Patient",
#'   parameters = "name=Homer&name=Simpson")
#'
#' #Sample 10 of them
#' bundles <- fhir_sample_resources_by_ids(
#'   base_url    = 'https://hapi.fhir.org/baseR4',
#'   resource    = "Patient",
#'   ids         = ids,
#'   sample_size = 10,
#'   seed        = 1)
#'
#' #Have a look at the samples
#' fhir_crack(
#'   bundles,
#'   fhir_table_description(
#'     resource = "Patient",
#'     cols     = list(
#'       ID      = "id",
#'       given  = "name/given",
#'       family = "name/family")))
#'}
fhir_sample_resources_by_ids <- function(base_url, resource, ids, sample_size = 20, seed = as.double(Sys.time()), verbose = 1) {
	if(length(ids) < sample_size) {stop("The id list has only length", length(ids), " . sample_size must be smaller than this number.")}
	set.seed(seed = seed)
	ids <- sample(ids, sample_size, replace = F)
	if(0 < verbose){message("Downloading ", sample_size, " full resources.")}
	fhir_get_resources_by_ids(base_url = base_url, resource = resource, ids = ids, verbose = verbose)
}



#' Randomly sample resources from a FHIR server
#'
#' Downloads a random sample of resources of a given resource type from a FHIR server. The resources can be further constrained
#' using FHIR search parameters.
#'
#' @details
#' This function performs three steps to draw a random sample of resources from a FHIR server:
#'
#' 1) Count how many resources matching the type `resource` and the search parameters in `parameters` are found on the server. This is done to assert that the desired `sample_size`
#' is bigger than the number of resources it is drawn from. This step can also be performed individually using [fhir_count_resource()].
#'
#' 2) Extract the resource (aka logical) IDs of all requested resources (without downloading the resources completely).
#' This step can be also be performed individually using [fhir_get_resource_ids()]
#'
#' 3) Draw a random sample of size `sample_size` from the vector of resource IDs and download the corresponding set of resources from the server.
#' This can also be done individually using [fhir_sample_resources_by_ids()]
#'
#' The actual download of the resources is done by [fhir_get_resources_by_ids()]. This function will attempt to download the resources using a
#' FHIR search request via POST where the IDs are part of the body. See [fhir_search()] for details. If this fails
#' (e.g. because the server doesn't allow POST operations), the function falls back on a GET request. If the set of IDs is too long to fit
#' into one GET request (i.e. if the request gets longer than 2083 characters), it will be spread across several requests.
#'
#' @param base_url A character vector of length one specifying the base URL of the FHIR server, e.g. `"http://hapi.fhir.org/baseR4"`.
#' @param resource A character vector of length one or [fhir_resource_type-class] object with the resource type to be downloaded, e.g. `"Patient"`.
#' @param parameters Optional. Either a length 1 character vectorcontaining properly formatted FHIR search parameters, e.g.
#' `"gender=male&_summary=count"` or a named list or named character vector e.g. `list(gender="male", "_summary"="count")`
#' or `c(gender="male", "_summary"="count")`. Note that parameter names beginning with `_` have to be put in quotation marks!
#' @param sample_size A integer of length 1 containing the number of resources to sample.
#' @param seed A integer of length 1 containing the seed for the random generator.
#' @param verbose An integer of length 1 containing the level of verbosity. Defaults to 1.
#'
#' @return A [fhir_bundle_list-class] containing randomly sampled resources.
#' @export
#'
#' @examples
#' \donttest{
#' #how many resources are on the server?
#' count <- fhir_count_resource(
#'   base_url    = 'https://hapi.fhir.org/baseR4',
#'   resource    = "Patient",
#'   parameters  = "gender=female")
#'
#' #randomly sample 30 of them
#' bundles <- fhir_sample_resources(
#'   base_url    = 'https://hapi.fhir.org/baseR4',
#'   resource    = "Patient",
#'   parameters  = "gender=female",
#'   sample_size = 30,
#'   seed        = 1)
#'
#' bundles
#'}

fhir_sample_resources <- function(base_url, resource, parameters = parameters, sample_size = 20, seed = as.double(Sys.time()), verbose = 1) {
	cnt <- fhir_count_resource(base_url = base_url, resource = resource, parameters = parameters)
	if(cnt < sample_size) stop("There are only ", cnt, " resources on the server. sample_size must be smaller than this number.")
	ids <- fhir_get_resource_ids(base_url = base_url, resource = resource, parameters = parameters, verbose = verbose)
	fhir_sample_resources_by_ids(
		base_url = base_url,
		resource = resource,
		ids = ids,
		sample_size = sample_size,
		seed = seed,
		verbose = verbose
	)
}
