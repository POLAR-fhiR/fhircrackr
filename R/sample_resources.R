MAX_LEN = 2083

get_resources_count <- function(endpoint, resource_name) {
	as.numeric(xml2::xml_attr(xml2::xml_find_first(fhir_search(paste0(paste_paths(endpoint, resource_name), "?_summary=count"), verbose = 0, log_errors = "tmp/log_resources_counts.txt")[[1]], "//total"), "value"))
}

get_resources_ids <- function(endpoint = endpoints$vonk, resource_name) {
	cat(paste0("Download ", get_resources_count(endpoint = endpoint, resource_name = resource_name), " ", resource_name, "s' IDs...\n"))
	unlist(
		sapply(
			fhir_search(paste0(paste_paths(endpoint, resource_name), "?_count=500&_elements=id"), log_errors = "tmp/log_get_resources_ids.txt", verbose = 2),
			function(bundle) {
				xml2::xml_attr(xml2::xml_find_all(bundle, paste0("entry/resource/", resource_name, "/id")), "value")
			}
		)
	)
}

collect_indices_for_request <- function(ids, max_ids = length(ids), max_len = MAX_LEN - 300) {
	n <- 1
	s <- ids[[n]]
	n <- n + 1
	while(n <= max_ids && n <= length(ids) && nchar(s) + nchar(ids[[n]]) < max_len) {
		s <- paste0(s, ",", ids[[n]])
		n <- n + 1
	}
	list(str = s, n = n - 1)
}

# ids <- get_resources_ids(endpoint = endpoints$agiop, resource_name = resource_name)
# nchar(collect_indices_for_request(ids = ids, max_len = 20)$str)


# get_samples <- function(endpoint, resource_name, sample_size = 50, bundle_size = 7, seed = as.double(Sys.time())) {
# 	set.seed(seed = seed)
# 	cnt <- get_resources_count(endpoint = endpoint, resource_name = resource_name)
# 	if(cnt < sample_size) stop("The sample must be smaller or equal than the population size.")
# 	ids <- get_resources_ids(endpoint = endpoint, resource_name = resource_name)
# 	ids <- try(sort(sample(ids, sample_size, replace = F)))
# 	if(inherits(ids, "try-error"))  stop("The sample must be smaller or equal than the population size.")
#
# 	bundles <- list()
# 	i <- 1
# 	b <- 1
# 	while(i <= sample_size) {
# 		end <- min(c(i + bundle_size - 1, sample_size, length(ids)))
# 		blist <- xml2::xml_new_root("Bundle")
# 		j <- 1
# 		while(i <= end) {
# 			ids_ <- collect_indices_for_request(ids = ids, 1200)
# 			url_ <- paste0(paste_paths(endpoint, resource_name), "?_id=", ids_$str)
# #			url_ <- paste0(paste_paths(endpoint, resource_name), "?_id=", paste0(ids[i : (i + 10)], collapse = ","))
# 			bnd_ <- fhir_search(request = url_, max_bundles = 1, verbose = 0)
# 			entries <- xml2::xml_find_all(bnd_[[1]], paste0("entry[./resource/", resource_name,"]"))
# 			for(entry in entries) xml2::xml_add_child(blist, entry)
# 			# entry <- xml2::xml_find_first(bnd_[[1]], paste0("entry[./resource/", resource_name,"]"))
# 			# xml2::xml_add_child(blist, entry)
# 			i <- i + ids_$n
# 			j <- j + ids_$n
# 			ids <- ids[-seq_len(ids_$n)]
# 		}
# 		bundles[[b]] <- blist
# 		cat(paste0("Bundle ", b, " a ", j - 1, " ", resource_name, "s  \u03A3 ", resource_name, "s = ", i - 1, "\n"))
# 		b <- b + 1
# 	}
# 	fhir_bundle_list(bundles)
#
# }
#
get_resources_by_ids_fix_bundle_size <- function(endpoint, resource_name, ids, bundle_size = 7, seed = as.double(Sys.time())) {
	# cnt <- get_resources_count(endpoint = endpoint, resource_name = resource_name)
	# if(cnt < sample_size) stop("The sample must be smaller or equal than the population size.")
	# ids <- get_resources_ids(endpoint = endpoint, resource_name = resource_name)
	# set.seed(seed = seed)
	# ids <- try(sample(ids, sample_size, replace = F))
	# #	ids <- sort(ids)
	# if(inherits(ids, "try-error"))  stop("The sample must be smaller or equal than the population size.")

	bundles <- list()
	total <- 1
	bundle_count <- 1
	while(0 < length(ids)) {
		blist <- xml2::xml_new_root("Bundle")
		end <- min(c(bundle_size, length(ids)))
		current_bundle_size <- 0
		while(current_bundle_size < end) {
			ids_ <- collect_indices_for_request(ids = ids, max_ids = end - current_bundle_size, 1200)
			url_ <- paste0(paste_paths(endpoint, resource_name), "?_id=", ids_$str)
			bnd_ <- fhir_search(request = url_, verbose = 0, log_errors = "tmp/log_get_resources_fix_bundlesize.txt")
			for(bn in bnd_) {
				entries <- xml2::xml_find_all(bn, paste0("entry[./resource/", resource_name,"]"))
				for(entry in entries) xml2::xml_add_child(blist, entry)
			}
			total <- total + ids_$n
			current_bundle_size <- current_bundle_size + ids_$n
			ids <- ids[-seq_len(ids_$n)]
		}
		bundles[[bundle_count]] <- blist
		cat(paste0("Bundle ", bundle_count, " a ", current_bundle_size, " ", resource_name, "s  \u03A3 ", total - 1, "\n"))
		bundle_count <- bundle_count + 1
	}
	fhir_bundle_list(bundles)
}

sample_resources_fix_bundle_size <- function(endpoint, resource_name, sample_size = 20, bundle_size = 7, seed = as.double(Sys.time())) {
	cnt <- get_resources_count(endpoint = endpoint, resource_name = resource_name)
	if(cnt < sample_size) stop("The sample must be smaller or equal than the population size.")
	ids <- get_resources_ids(endpoint = endpoint, resource_name = resource_name)
	set.seed(seed = seed)
	ids <- try(sample(ids, sample_size, replace = F))
	#	ids <- sort(ids)
	if(inherits(ids, "try-error"))  stop("The sample must be smaller or equal than the population size.")

	get_resources_by_ids_fix_bundle_size(endpoint = endpoint, resource_name = resource_name, ids = ids, bundle_size = bundle_size, seed = seed)
}

get_resources_by_ids <- function(endpoint, resource_name, ids, seed = as.double(Sys.time())) {
	bundles <- list()
	total <- 1
	bundle_count <- 1
	while(0 < length(ids)) {
		ids_ <- collect_indices_for_request(ids = ids, max_ids = length(ids))
		url_ <- paste0(paste_paths(endpoint, resource_name), "?_id=", ids_$str)
		bnd_ <- fhir_search(request = url_, verbose = 0, log_errors = "tmp/log_get_resources.txt")
		total <- total + ids_$n
		ids <- ids[-seq_len(ids_$n)]
		bundles <- c(bundles, bnd_)
		cat(paste0("Bundle ", bundle_count, " a ", ids_$n, " ", resource_name, "s  \u03A3 ", total - 1, "\n"))
		bundle_count <- bundle_count + 1
	}
	fhir_bundle_list(bundles)
}

sample_resources <- function(endpoint, resource_name, sample_size = 20, seed = as.double(Sys.time())) {
	cnt <- get_resources_count(endpoint = endpoint, resource_name = resource_name)
	if(cnt < sample_size) stop("The sample must be smaller or equal than the population size.")
	ids <- get_resources_ids(endpoint = endpoint, resource_name = resource_name)
	set.seed(seed = seed)
	ids <- try(sample(ids, sample_size, replace = F))
	#	ids <- sort(ids)
	if(inherits(ids, "try-error"))  stop("The sample must be smaller or equal than the population size.")
	get_resources_by_ids(endpoint = endpoint, resource_name = resource_name, ids = ids, seed = seed)
}

