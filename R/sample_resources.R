MAX_LEN = 2083



# dyadic <- function(x, y, fun = function(x, y) x * y, ...) {
# 	d <- data.table()
# 	n <- as.character(y)
# 	d[,(n):=lapply(y, function(y_) lapply(x, function(x_) fun(x_, y_, ...)))]
# 	d$row <- as.character(x)
# 	setcolorder(d, c("row", n))
# 	d[]
# }



#' Order Strings with Numbers correctly
#'
#' @param s A character containing the strings to be ordered.
#'
#' @return A character containing the order of the given strings.
#' @export
#'
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



#' Order Strings with Numbers correctly
#'
#' @param s A character containing the strings to be sorted.
#'
#' @return A character containing the the given strings sorted.
#' @export
#'
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



#' Paste Parematers for a FHIR Search Request
#'
#' @param parameters Either a character of length 1 containing an existing parameter string or
#' a named vector or list of the form
#' c("_summary" = "count", "gender" = "male") or list("_summary" = "count", "gender" = "female").
#' @param parameters2add Either a character of length 1 containing a parameter string to add or
#' a named vector or list of the form
#' c("_summary" = "count", "gender" = "male") or list("_summary" = "count", "gender" = "female").
#' @param add_question_sign Logical. Should an ?-Sign to be prefixed. Defaults to FALSE.
#'
#' @return A character of length 1 representing the pasted parameter string.
#' @export
#'
#' @examples paste_parameters(
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
#' @param endpoint A character of length 1 containing the FHIR R4 endpoint.
#' @param resource A character of length 1 containing the Resource's name.
#' @param parameters Either a character of length 1 containing the parameter string or
#' a named vector of the form c("gender" = "male", "_summary" = "count").
#' @param verbose An integer of length 1 containing the level of verbosity. Defaults to 0.
#'
#' @return An integer of length 1 containing the number of resources named `resource`
#' filtered by parameters available on the FHIR R4 endpoint.
#' @export
#'
#' @examples
#' get_resources_count(
#'   endpoint = 'https://hapi.fhir.org/baseR4',
#'   resource = "Patient",
#'   parameters = c(gender = "female", name = "Simpson"))
get_resources_count <- function(endpoint, resource, parameters = NULL, verbose = 0) {
	as.numeric(
		xml2::xml_attr(
			xml2::xml_find_first(
				fhir_search(
					fhir_url(
						url = endpoint,
						resource = resource,
						parameters = paste_parameters(parameters = parameters, parameters2add = c("_summary" = "count"), add_question_sign = F),
					),
					verbose = verbose
				)[[1]],
				"//total"
			),
			"value"
		)
	)
}



#' Get Resources' IDs
#'
#' @param endpoint A character of length 1 containing the FHIR R4 endpoint.
#' @param resource A character of length 1 containing the Resource's name.
#' @param parameters Either a character of length 1 containing the parameter string or
#' a named vector of the form c("gender" = "male", "_summary" = "count").
#' @param verbose An integer of length 1 containing the level of verbosity. Defaults to 0.
#'
#' @return A character containing the IDs all requested Resources.
#' @export
#'
#' @examples
#' get_resources_ids(
#'   endpoint   = 'https://hapi.fhir.org/baseR4',
#'   resource   = "Patient",
#'   parameters = "name=Homer")
get_resources_ids <- function(endpoint, resource, parameters = NULL, verbose = 0) {
	if(0 < verbose) {
		cat(
			paste0(
				"Download ",
				get_resources_count(
					endpoint = endpoint,
					resource = resource,
					parameters = paste_parameters(parameters)
				),
				" ",
				resource,
				"s' IDs...\n"
			)
		)
	}
	bundles <- try(
		fhir_search(
			url <- fhir_url(
				url = endpoint,
				resource = resource,
				parameters = paste_parameters(parameters, c("_elements" = "id", "_count" = "500"))
			),
			verbose = verbose
		)
	)

	if(inherits(bundles, "try-error")) {
		warning(paste0("The url ", url, " could not be succesfully resolved. Use fhir_recent_http_error() to get more information!"))
		NA_integer_;
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
#' @param endpoint A character of length 1 containing the FHIR R4 endpoint.
#' @param resource A character of length 1 containing the Resource's name.
#' @param ids A character containing the IDs.
#' @param verbose An integer of length 1 containing the level of verbosity. Defaults to 0.
#'
#' @return A list of bundles containing the downloaded resources.
#' @export
#'
#' @examples
#' ids <- get_resources_ids(
#'   endpoint   = 'https://hapi.fhir.org/baseR4',
#'   resource   = "Patient",
#'   parameters = "name=Homer&name=Simpson")
#' ids <- sample(ids, 10)
#' bndls <- get_resources_by_ids(
#'   endpoint = 'https://hapi.fhir.org/baseR4',
#'   resource = "Patient",
#'   ids      = ids)
#' fhir_crack(
#'   bndls,
#'   fhir_table_description(
#'     resource = "Patient",
#'     cols     = list(
#'       ID      = "id",
#'       Homer   = "name/given",
#'       Simpson = "name/family")))
get_resources_by_ids <- function(endpoint, resource, ids, verbose = 0) {
	get_resources_by_ids_get <- function(endpoint, resource, ids, verbose = 1) {
		collect_ids_for_request <- function(ids, max_ids = length(ids), max_len = MAX_LEN - 300) {
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
						". So no single id is added to the list."
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
			url_ <- paste0(paste_paths(endpoint, resource), "?_id=", ids_$str)
			bnd_ <- fhir_search(request = url_, verbose = 0)
			total <- total + ids_$n
			ids <- ids[-seq_len(ids_$n)]
			bundles <- c(bundles, bnd_)
			cat(paste0("Bundle ", bundle_count, " a ", ids_$n, " ", resource, "s  \u03A3 ", total - 1, "\n"))
			bundle_count <- bundle_count + 1
		}
		fhir_bundle_list(bundles)
	}

	get_resources_by_ids_post <- function(endpoint, resource, ids, verbose = 1) {
		fhir_search(
			fhir_url(
				url = endpoint,
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

	bundles <- try(get_resources_by_ids_post(endpoint = endpoint, resource = resource, ids = ids, verbose = verbose))
	if(inherits(bundles, "try-error")) {
		bundles <- get_resources_by_ids_get(endpoint = endpoint, resource = resource, ids = ids, verbose = verbose)
	}
	bundles
}



#' Sample Resources of certain IDs
#'
#' @param endpoint A character of length 1 containing the FHIR R4 endpoint.
#' @param resource A character of length 1 containing the Resource's name.
#' @param ids A character containing the IDs that should be sampled.
#' @param sample_size A integer of length 1 containing the size of the sample.
#' @param seed A integer of length 1 containing the seed for the random generator.
#' @param verbose An integer of length 1 containing the level of verbosity. Defaults to 1.
#'
#' @return A list of bundles containing sampled resources.
#' @export
#'
#' @examples
#' ids <- get_resources_ids(
#'   endpoint   = 'https://hapi.fhir.org/baseR4',
#'   resource   = "Patient",
#'   parameters = "name=Homer&name=Simpson")
#' bndls <- sample_identified_resources(
#'   endpoint    = 'https://hapi.fhir.org/baseR4',
#'   resource    = "Patient",
#'   ids         = ids,
#'   sample_size = 10,
#'   seed        = 1)
#' fhir_crack(
#'   bndls,
#'   fhir_table_description(
#'     resource = "Patient",
#'     cols     = list(
#'       ID      = "id",
#'       Homer   = "name/given",
#'       Simpson = "name/family")))
sample_identified_resources <- function(endpoint, resource, ids, sample_size = 20, seed = as.double(Sys.time()), verbose = 1) {
	if(length(ids) < sample_size) stop("The sample must be smaller or equal than the population size.")
	set.seed(seed = seed)
	ids <- try(sample(ids, sample_size, replace = F))
	if(inherits(ids, "try-error"))  stop("The sample must be smaller or equal than the population size.")
	get_resources_by_ids(endpoint = endpoint, resource = resource, ids = ids, verbose = verbose)
}



#' Sample Resources of certain IDs
#'
#' @param endpoint A character of length 1 containing the FHIR R4 endpoint.
#' @param resource A character of length 1 containing the Resource's name.
#' @param parameters Either a character of length 1 containing the parameter string or
#' a named vector of the form c("gender" = "male", "_summary" = "count").
#' @param sample_size A integer of length 1 containing the size of the sample.
#' @param seed A integer of length 1 containing the seed for the random generator.
#' @param verbose An integer of length 1 containing the level of verbosity. Defaults to 1.
#'
#' @return A list of bundles containing sampled resources.
#' @export
#'
#' @examples
#' bndls <- sample_resources(
#'   endpoint    = 'https://hapi.fhir.org/baseR4',
#'   resource    = "Patient",
#'   parameters  = "name=Marge&name=Simpson",
#'   sample_size = 10,
#'   seed        = 1)
#' fhir_crack(
#'   bndls,
#'   fhir_table_description(
#'     resource = "Patient",
#'     cols     = list(
#'       ID      = "id",
#'       Marge   = "name/given",
#'       Simpson = "name/family")))
sample_resources <- function(endpoint, resource, parameters = parameters, sample_size = 20, seed = as.double(Sys.time()), verbose = 1) {
	cnt <- get_resources_count(endpoint = endpoint, resource = resource, parameters = parameters, verbose = verbose)
	if(cnt < sample_size) stop("The sample must be smaller or equal than the population size.")
	ids <- get_resources_ids(endpoint = endpoint, resource = resource, parameters = parameters, verbose = verbose)
	sample_identified_resources(
		endpoint = endpoint,
		resource = resource,
		ids = ids,
		sample_size = sample_size,
		seed = seed,
		verbose = verbose
	)
}
