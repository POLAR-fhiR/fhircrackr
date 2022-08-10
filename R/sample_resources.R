
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

paste_parameters <- function(parameters = NULL, parameters2add = NULL, add_question_sign = FALSE) {
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
#' For more information on authentication options, please see the help page of [fhir_search()]
#'
#' @param base_url A character vector of length one specifying the base URL of the FHIR server, e.g. `"http://hapi.fhir.org/baseR4"`.
#' @param resource A character vector of length one or [fhir_resource_type-class] object with the resource type to be searched, e.g. `"Patient"`.
#' @param parameters Optional. Either a length 1 character vector containing properly formatted FHIR search parameters, e.g.
#' `"gender=male&_summary=count"` or a named list or named character vector e.g. `list(gender="male", "_summary"="count")`
#' or `c(gender="male", "_summary"="count")`. Note that parameter names beginning with `_` have to be put in quotation marks!
#' @param username A character vector of length one containing the username for basic authentication.
#' @param password A character vector of length one containing the password for basic authentication.
#' @param token A character vector of length one or object of class [httr::Token-class], for bearer token authentication (e.g. OAuth2). See [fhir_authenticate()]
#' for how to create this.
#' @param cookies Optional. A named character vector containing key value pairs for cookies, e.g. `c(mycookie = "d385se12394j")`.
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
fhir_count_resource <- function(
	base_url,
	resource,
	parameters = NULL,
	username   = NULL,
	password   = NULL,
	token      = NULL,
	cookies    = NULL
) {
	as.numeric(
		xml2::xml_attr(
			xml2::xml_find_first(
				fhir_search(
					request = fhir_url(
						url        = base_url,
						resource   = resource,
						parameters = paste_parameters(
							parameters        = parameters,
							parameters2add    = c("_summary" = "count"),
							add_question_sign = FALSE
						),
					),
					username = username,
					password = password,
					token    = token,
					cookies  = cookies,
					verbose  = 0
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
#' For more information on authentication options, please see the help page of [fhir_search()]
#'
#' @param base_url A character vector of length one specifying the base URL of the FHIR server, e.g. `"http://hapi.fhir.org/baseR4"`.
#' @param resource A character vector of length one or [fhir_resource_type-class] object with the resource type to be searched, e.g. `"Patient"`.
#' @param parameters Optional. Either a length 1 character vector containing properly formatted FHIR search parameters, e.g.
#' `"gender=male&_summary=count"` or a named list or named character vector e.g. `list(gender="male", "_summary"="count")`
#' or `c(gender="male", "_summary"="count")`. Note that parameter names beginning with `_` have to be put in quotation marks!
#' @param username A character vector of length one containing the username for basic authentication.
#' @param password A character vector of length one containing the password for basic authentication.
#' @param token A character vector of length one or object of class [httr::Token-class], for bearer token authentication (e.g. OAuth2). See [fhir_authenticate()]
#' for how to create this.
#' @param cookies Optional. A named character vector containing key value pairs for cookies, e.g. `c(mycookie = "d385se12394j")`.
#' @param verbose An integer of length 1 containing the level of verbosity. Defaults to 0.
#'
#' @return A character vector containing the resource (aka logical) IDs of all requested resources.
#' @export
#' @seealso [fhir_search()], [fhir_get_resources_by_ids()]
#' @examples
#' \donttest{
#' fhir_get_resource_ids(
#'   base_url   = 'https://vonk.fire.ly/R4',
#'   resource   = "Patient",
#'   parameters = "gender=female", verbose=1)
#'   }
fhir_get_resource_ids <- function(
	base_url,
	resource,
	parameters = NULL,
	username   = NULL,
	password   = NULL,
	token      = NULL,
	cookies    = NULL,
	verbose    = 0
) {
	if(0 < verbose) {
		message(
			paste0(
				"Download ",
				fhir_count_resource(
					base_url   = base_url,
					resource   = resource,
					parameters = paste_parameters(parameters),
					username   = username,
					password   = password,
					token      = token,
					cookies    = cookies
				),
				" ",
				resource,
				"s' IDs...\n"
			)
		)
	}

	request <-  fhir_url(
		url        = base_url,
		resource   = resource,
		parameters = paste_parameters(parameters, c("_elements" = "id", "_count" = "500"))
	)

	bundles <- 	try(
		fhir_search(
			request  = request,
			username = username,
			password = password,
			token    = token,
			cookies  = cookies,
			verbose  = 0
		)
	)

	if(inherits(bundles, "try-error")){
		warning(paste0(
			"The url ",
			request,
			" could not be succesfully resolved. Use fhir_recent_http_error() to get more information!"
		))
		NA_integer_
	} else {
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
#'
#' For more information on authentication options, please see the help page of [fhir_search()]
#'
#' @param base_url A character vector of length one specifying the base URL of the FHIR server, e.g. `"http://hapi.fhir.org/baseR4"`.
#' @param resource A character vector of length one or [fhir_resource_type-class] object with the resource type to be searched, e.g. `"Patient"`.
#' @param ids A character vector containing the IDs of the resources that should be downloaded. In the default setting these should be resource (aka logical) IDs.
#' @param id_param A character vector of length one containing the FHIR Search parameter belonging to the ids in `ids`. Defaults to `"_id"` meaning `ids` is interpreted as
#' containing resource (aka logical) ids. Could be changed to `"identifier"` if `ids` contains a vector of identifier values instead.
#' @param parameters FHIR Search parameters to further restrict the set of resources that is returned, e.g. `gender=male` to only download the resources from
#' the `ids` list that correspond to males. Can be either a length 1 character containing properly formatted FHIR search parameters, e.g.
#' `"gender=male"` or a named list or named character vector e.g. `list(gender="male")`or `c(gender="male")`. Defaults to `NULL` meaning no restriction on
#' the IDs provided in `ids`.
#' @param username A character vector of length one containing the username for basic authentication.
#' @param password A character vector of length one containing the password for basic authentication.
#' @param token A character vector of length one or object of class [httr::Token-class], for bearer token authentication (e.g. OAuth2). See [fhir_authenticate()]
#' for how to create this.
#' @param cookies Optional. A named character vector containing key value pairs for cookies, e.g. `c(mycookie = "d385se12394j")`.
#' @param verbose An integer vector of length 1 containing the level of verbosity. Defaults to 0.
#'
#' @return A [fhir_bundle_list-class] containing the downloaded resources.
#' @export
#' @seealso [fhir_search()], [fhir_get_resource_ids()]
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
#'       family  = "name/family")))
#'}
fhir_get_resources_by_ids <- function(
	base_url,
	resource,
	ids,
	id_param   = '_id',
	parameters = NULL,
	username   = NULL,
	password   = NULL,
	token      = NULL,
	cookies    = NULL,
	verbose    = 0
) {
	#download via GET
	get_resources_by_ids_get <- function(base_url, resource, ids, id_param, username = NULL, password = NULL, token = NULL, cookies = NULL, verbose = 1) {
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
			url_ <- fhir_url(base_url, resource, paste_parameters(paste0(id_param, "=", ids_$str), parameters))
			bnd_ <- fhir_search(request = url_, username = username, password = password, token = token, cookies = cookies, verbose = 0)
			total <- total + ids_$n
			ids <- ids[-seq_len(ids_$n)]
			bundles <- c(bundles, bnd_)
			if(1 < verbose){cat(paste0("Bundle ", bundle_count, " a ", ids_$n, " ", resource, "s  \u03A3 ", total - 1, "\n"))}
			bundle_count <- bundle_count + 1
		}
		fhir_bundle_list(bundles)
	}

	#download via POST
	get_resources_by_ids_post <- function(base_url, resource, ids, id_param, username, password, token, cookies, verbose = 1) {

		parameters_list <- stats::setNames(
			list(paste0(ids, collapse = ","),"100"),
			c(id_param, '_count')
		)

		fhir_search(
			request = fhir_url(
				url = base_url,
				resource = resource,
				url_enc  = TRUE
			),
			body = fhir_body(
				content = paste_parameters(
					parameters     = parameters_list,
					parameters2add = parameters
				),
				type = "application/x-www-form-urlencoded"
			),
			username = username,
			password = password,
			token    = token,
			cookies  = cookies,
			verbose  = verbose
		)
	}

	bundles <- try(
		get_resources_by_ids_post(
			base_url = base_url,
			resource = resource,
			ids      = ids,
			id_param = id_param,
			username = username,
			password = password,
			token    = token,
			cookies  = cookies,
			verbose  = verbose
		),
		silent = TRUE
	)
	if(inherits(bundles, "try-error")) {
		if(0 < verbose) {
			message("Search via POST failed, falling back to iterative download via GET")
		}

		bundles <- get_resources_by_ids_get(
			base_url = base_url,
			resource = resource,
			ids      = ids,
			id_param = id_param,
			username = username,
			password = password,
			token    = token,
			cookies  = cookies,
			verbose  = verbose
		)
	}
	bundles
}




#' Download a random sample of resources from a vector of resource IDs.
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
#' For more information on authentication options, please see the help page of `[fhir_search()]
#'
#' @param base_url A character vector of length one specifying the base URL of the FHIR server, e.g. `"http://hapi.fhir.org/baseR4"`.
#' @param resource A character vector of length one or [fhir_resource_type-class] object with the resource type to be downloaded e.g. `"Patient"`.
#' @param ids A character vector containing the IDs from which to sample.
#' @param id_param A character vector of length one containing the FHIR Search parameter belonging to the ids in `ids`. Defaults to `"_id"` meaning `ids` is interpreted as
#' containing resource (aka logical) ids. Could be changed to `"identifier"` if `ids` contains a vector of identifier values instead.
#' @param username A character vector of length one containing the username for basic authentication.
#' @param password A character vector of length one containing the password for basic authentication.
#' @param token A character vector of length one or object of class [httr::Token-class], for bearer token authentication (e.g. OAuth2). See [fhir_authenticate()]
#' for how to create this.
#' @param cookies Optional. A named character vector containing key value pairs for cookies, e.g. `c(mycookie = "d385se12394j")`.
#' @param sample_size A integer of length 1 containing the number of resources to sample.
#' @param seed A integer of length 1 containing the seed for the random generator.
#' @param verbose An integer of length 1 containing the level of verbosity. Defaults to 1.
#'
#' @return A list of bundles containing sampled resources.
#' @export
#' @seealso [fhir_search()], [fhir_sample_resources()], [fhir_get_resources_by_ids()], [fhir_count_resource()]
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
#'       ID     = "id",
#'       given  = "name/given",
#'       family = "name/family")))
#'}
fhir_sample_resources_by_ids <- function(
	base_url,
	resource,
	ids,
	id_param    = "_id",
	username    = NULL,
	password    = NULL,
	token       = NULL,
	cookies     = NULL,
	sample_size = 20,
	seed        = 1,
	verbose     = 1
) {
	if(length(ids) < sample_size) {
		stop("The id list has only length ", length(ids), ". sample_size must be smaller than this number.")
	}

	set.seed(seed = seed)
	ids <- sample(ids, sample_size, replace = FALSE)

	if(0 < verbose){
		message("Downloading ", sample_size, " full resources.")
	}

	fhir_get_resources_by_ids(
		base_url = base_url,
		resource = resource,
		ids      = ids,
		id_param = id_param,
		username = username,
		password = password,
		token    = token,
		cookies  = cookies,
		verbose  = verbose
	)
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
#' For more information on authentication options, please see the help page of [fhir_search()]
#'
#' @param base_url A character vector of length one specifying the base URL of the FHIR server, e.g. `"http://hapi.fhir.org/baseR4"`.
#' @param resource A character vector of length one or [fhir_resource_type-class] object with the resource type to be downloaded, e.g. `"Patient"`.
#' @param parameters Optional. Either a length 1 character vector containing properly formatted FHIR search parameters, e.g.
#' `"gender=male&_summary=count"` or a named list or named character vector e.g. `list(gender="male", "_summary"="count")`
#' or `c(gender="male", "_summary"="count")`. Note that parameter names beginning with `_` have to be put in quotation marks!
#' @param username A character vector of length one containing the username for basic authentication.
#' @param password A character vector of length one containing the password for basic authentication.
#' @param token A character vector of length one or object of class [httr::Token-class], for bearer token authentication (e.g. OAuth2). See [fhir_authenticate()]
#' for how to create this.
#' @param cookies Optional. A named character vector containing key value pairs for cookies, e.g. `c(mycookie = "d385se12394j")`.
#' @param sample_size A integer of length 1 containing the number of resources to sample.
#' @param seed A integer of length 1 containing the seed for the random generator.
#' @param verbose An integer of length 1 containing the level of verbosity. Defaults to 1.
#'
#' @return A [fhir_bundle_list-class] containing randomly sampled resources.
#' @seealso [fhir_search()], [fhir_sample_resources_by_ids()], [fhir_get_resources_by_ids()], [fhir_count_resource()]
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

fhir_sample_resources <- function(
	base_url,
	resource,
	parameters  = NULL,
	username    = NULL,
	password    = NULL,
	token       = NULL,
	cookies     = NULL,
	sample_size = 20,
	seed        = 1,
	verbose     = 1
) {
	cnt <- fhir_count_resource(
		base_url   = base_url,
		resource   = resource,
		parameters = parameters,
		username   = username,
		password   = password,
		token      = token,
		cookies    = cookies
	)

	if(cnt < sample_size) {
		stop(
			"There are only ",
			cnt,
			" resources available on the server. sample_size must be smaller than this number."
		)
	}

	fhir_sample_resources_by_ids(
		base_url    = base_url,
		resource    = resource,
		ids         = fhir_get_resource_ids(
			base_url   = base_url,
			resource   = resource,
			parameters = parameters,
			username   = username,
			password   = password,
			token      = token,
			cookies    = cookies,
			verbose    = verbose
		),
		username    = username,
		password    = password,
		token       = token,
		cookies     = cookies,
		sample_size = sample_size,
		seed        = seed,
		verbose     = verbose
	)
}
