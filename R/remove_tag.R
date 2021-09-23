#library("Rcpp")

#Rcpp::sourceCpp("CPP/remove_tag.cpp")



remove_tag <- function(string, tag = "div") {
	if(1 < length(string)) return(lapply(string, remove_tag, tag = tag))
	tOn <- paste0("<", tag, "[^/]*?>")
	tOff <- paste0("</", tag, ">")
	tagOn  <- as.data.table(stringr::str_locate_all(string = string, tOn)[[1]])
	tagOff <- as.data.table(stringr::str_locate_all(string = string, tOff)[[1]])
	tagOn[,(c("type", "id")) := .(1, seq_len(nrow(tagOn)))]
	tagOff[,(c("type", "id")) := .(-1, seq_len(nrow(tagOff)))]
	tags <- rbindlist(list(tagOn, tagOff))
	setorder(tags, start)
	if(0 < nrow(tags)) {
		if(tags$type[1] == -1) {
			stop("Tag closes before it opens.")
		}
		if(1 < tags$start[1]) {
			tags <- rbindlist(list(data.table(start = 1, end = tags$start[1] - 1, type = 0, id = 1), tags))
		}
		if(tags$end[nrow(tags)] < nchar(string)) {
			tags <- rbindlist(list(tags, data.table(start = tags$end[nrow(tags)] + 1, end = nchar(string), type = 0, id = 2)))
		}
		tags[,s:=sapply(seq_len(nrow(tags)),function(x) sum(type[seq_len(x)]))]
		tags[,text:=substr(string, start, end), by = start]
		text <- list()
		for(i in seq_len(nrow(tags))) {
			part <- if(tags$type[i] == 0) {
				tags$text[i]
			} else if(tags$type[i] == -1 && tags$s[i] == 0 && i < nrow(tags)) {
				substr(string, tags$end[i] + 1, tags$start[i + 1] - 1)
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
		string
	}
}

remove_tag_from_bundle <- function(bundle, tag = "div") {
	fhircrackr::fhir_bundle_xml(bundle = xml2::read_xml(remove_tag(string = toString(bundle), tag = tag)))
}

remove_tag_from_bundles <- function(bundles, tag = "div") {
	nr.of.cores <- min(length(bundles), parallel::detectCores())
	get_os <- function() {
		sysinf <- Sys.info()
		if(!is.null(sysinf)) {
			os <- sysinf['sysname']
			if(os == 'Darwin') {os <- "osx"}
		} else { ## mystery machine
			os <- .Platform$OS.type
			if(grepl("^darwin", R.version$os)) {os <- "osx"}
			if(grepl("linux-gnu", R.version$os)) {os <- "linux"}
		}
		tolower(os)
	}

	os <- get_os()

	fhir_bundle_list(
		if(os %in% c("linux", "osx")) {
			message(paste0("Working on ", os, " using ", nr.of.cores, " core", if(1 < nr.of.cores) "s" else "", ".\n"))
			parallel::mclapply(
				bundles,
				function(bundle) {
					remove_tag_from_bundle(bundle = bundle, tag = tag)
				}
			)
		} else {
			message(paste0("Working on ms windows using 1 core.\n"))
			lapply(
				bundles,
				function(bundle) {
					remove_tag_from_bundle(bundle = bundle, tag = tag)
				}
			)
		}
	)
}
