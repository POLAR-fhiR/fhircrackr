MAX_LEN = 2083

dyadic <- function(x, y, fun = function(x, y) x * y, ...) {
	d <- data.table()
	n <- as.character(y)
	d[,(n):=lapply(y, function(y_) lapply(x, function(x_) fun(x_, y_, ...)))]
	d$row <- as.character(x)
	setcolorder(d, c("row", n))
	d[]
}

order_correctly_strings_with_numbers <- function(s) {
	sc <- as.character(s)
	ss <- stringr::str_extract_all(sc, "[0-9]+")
	i<-1
	max_char <- max(sapply(ss, function(x) {if(0 < length(x) && !is.na(x)) max(nchar(x), na.rm = T) else 0}))
	(s_numbers <- strsplit(sc, "[^0-9]+"))
	(s_letters <- strsplit(sc, "[0-9]+"))

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

sort_correctly_strings_with_numbers <- function(s) {
	if(is.null(s)){

		return(s)
	}
	sc <- as.character(s)
	s[order_correctly_strings_with_numbers(s)]
}

add_parameter <- function(parameters, parm2add = c("summary" = "count")) {
	if(is.null(parameters) || nchar(parameters) < 1) {
		parm2add
	} else if(length(names(parameters)) < length(parameters)) {
		paste0(
			parameters,
			if(0 < nchar(parameters)) "&" else "?",
			names(parm2add), "=", parm2add
		)
	} else {
		c(parameters, parm2add)
	}
}

get_resources_count <- function(endpoint, resource_name, parameters = NULL, verbose = 0) {
	as.numeric(
		xml2::xml_attr(
			xml2::xml_find_first(
				fhir_search(
					fhir_url(
						url = endpoint,
						resource = resource_name,
						parameters = add_parameter(parameters, c("_summary" = "count")),
					),
					verbose = verbose
				)[[1]],
				"//total"
			),
			"value"
		)
	)
}

get_resources_ids <- function(endpoint = endpoints$vonk, resource_name, parameters = NULL, verbose = 0) {
	if(0 < verbose) {
		cat(
			paste0(
				"Download ",
				get_resources_count(
					endpoint = endpoint,
					resource_name = resource_name,
					parameters = parameters
				),
				" ",
				resource_name,
				"s' IDs...\n"
			)
		)
	}
	bundles <- try(
		fhir_search(
			url <- fhir_url(
				url = endpoint,
				resource = resource_name,
				parameters = parameters %>%
					add_parameter(c("_elements" = "id")) %>%
					add_parameter(c("_count" = "500"))
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
								resource_name,
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

collect_indices_for_request <- function(ids, max_ids = length(ids), max_len = MAX_LEN - 300) {
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

# get_resources_by_ids_fix_bundle_size <- function(endpoint, resource_name, ids, bundle_size = 7, seed = as.double(Sys.time())) {
# 	# cnt <- get_resources_count(endpoint = endpoint, resource_name = resource_name)
# 	# if(cnt < sample_size) stop("The sample must be smaller or equal than the population size.")
# 	# ids <- get_resources_ids(endpoint = endpoint, resource_name = resource_name)
# 	# set.seed(seed = seed)
# 	# ids <- try(sample(ids, sample_size, replace = F))
# 	# #	ids <- sort(ids)
# 	# if(inherits(ids, "try-error"))  stop("The sample must be smaller or equal than the population size.")
#
# 	bundles <- list()
# 	total <- 1
# 	bundle_count <- 1
# 	while(0 < length(ids)) {
# 		blist <- xml2::xml_new_root("Bundle")
# 		end <- min(c(bundle_size, length(ids)))
# 		current_bundle_size <- 0
# 		while(current_bundle_size < end) {
# 			ids_ <- collect_indices_for_request(ids = ids, max_ids = end - current_bundle_size, 1200)
# 			url_ <- paste0(paste_paths(endpoint, resource_name), "?_id=", ids_$str)
# 			bnd_ <- fhir_search(request = url_, verbose = 0, log_errors = "tmp/log_get_resources_fix_bundlesize.txt")
# 			for(bn in bnd_) {
# 				entries <- xml2::xml_find_all(bn, paste0("entry[./resource/", resource_name,"]"))
# 				for(entry in entries) xml2::xml_add_child(blist, entry)
# 			}
# 			total <- total + ids_$n
# 			current_bundle_size <- current_bundle_size + ids_$n
# 			ids <- ids[-seq_len(ids_$n)]
# 		}
# 		bundles[[bundle_count]] <- blist
# 		cat(paste0("Bundle ", bundle_count, " a ", current_bundle_size, " ", resource_name, "s  \u03A3 ", total - 1, "\n"))
# 		bundle_count <- bundle_count + 1
# 	}
# 	fhir_bundle_list(bundles)
# }

get_resources_by_ids <- function(endpoint, resource_name, ids, seed = as.double(Sys.time()), verbose = 1) {
	bundles <- list()
	total <- 1
	bundle_count <- 1
	while(0 < length(ids)) {
		ids_ <- collect_indices_for_request(ids = ids, max_ids = length(ids))
		url_ <- paste0(paste_paths(endpoint, resource_name), "?_id=", ids_$str)
		bnd_ <- fhir_search(request = url_, verbose = 0)
		total <- total + ids_$n
		ids <- ids[-seq_len(ids_$n)]
		bundles <- c(bundles, bnd_)
		cat(paste0("Bundle ", bundle_count, " a ", ids_$n, " ", resource_name, "s  \u03A3 ", total - 1, "\n"))
		bundle_count <- bundle_count + 1
	}
	fhir_bundle_list(bundles)
}

get_resources_by_ids_post <- function(endpoint, resource_name, ids, seed = as.double(Sys.time()), verbose = 1) {
	fhir_search(
		fhir_url(
			url = endpoint,
			resource = resource_name,
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
	# bundles <- list()
	# total <- 1
	# bundle_count <- 1
	# while(0 < length(ids)) {
	# 	ids_ <- collect_indices_for_request(ids = ids, max_ids = length(ids))
	# 	url_ <- paste0(paste_paths(endpoint, resource_name), "?_id=", ids_$str)
	# 	bnd_ <- fhir_search(request = url_, verbose = 0)
	# 	total <- total + ids_$n
	# 	ids <- ids[-seq_len(ids_$n)]
	# 	bundles <- c(bundles, bnd_)
	# 	cat(paste0("Bundle ", bundle_count, " a ", ids_$n, " ", resource_name, "s  \u03A3 ", total - 1, "\n"))
	# 	bundle_count <- bundle_count + 1
	# }
	# fhir_bundle_list(bundles)
}

# sample_resources_fix_bundle_size <- function(endpoint, resource_name, sample_size = 20, bundle_size = 7, seed = as.double(Sys.time())) {
# 	cnt <- get_resources_count(endpoint = endpoint, resource_name = resource_name)
# 	if(cnt < sample_size) stop("The sample must be smaller or equal than the population size.")
# 	ids <- get_resources_ids(endpoint = endpoint, resource_name = resource_name, verbose = 1)
# 	set.seed(seed = seed)
# 	ids <- try(sample(ids, sample_size, replace = F))
# 	#	ids <- sort(ids)
# 	if(inherits(ids, "try-error"))  stop("The sample must be smaller or equal than the population size.")
#
# 	get_resources_by_ids_fix_bundle_size(endpoint = endpoint, resource_name = resource_name, ids = ids, bundle_size = bundle_size, seed = seed)
# }

sample_resources <- function(endpoint, resource_name, parameters = parameters, sample_size = 20, seed = as.double(Sys.time()), verbose = 1) {
	cnt <- get_resources_count(endpoint = endpoint, resource_name = resource_name, parameters = parameters, verbose = 1)
	if(cnt < sample_size) stop("The sample must be smaller or equal than the population size.")
	ids <- get_resources_ids(endpoint = endpoint, resource_name = resource_name, parameters = parameters, verbose = verbose)
	set.seed(seed = seed)
	ids <- try(sample(ids, sample_size, replace = F))
	#	ids <- sort(ids)
	if(inherits(ids, "try-error"))  stop("The sample must be smaller or equal than the population size.")
	bundles <- try(get_resources_by_ids_post(endpoint = endpoint, resource_name = resource_name, ids = ids, seed = seed, verbose = verbose))
	if(inherits(bundles, "try-error")) {
		bundles <- get_resources_by_ids(endpoint = endpoint, resource_name = resource_name, ids = ids, seed = seed, verbose = verbose)
	}
	bundles
}
