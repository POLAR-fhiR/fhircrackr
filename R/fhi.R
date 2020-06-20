lst <- function(..., prefix = NULL, suffix = NULL) {

	v <- as.list(c(...))

	names(v) <- paste0(prefix, v, suffix)

	v
}

get_ns <- function(xml, ns = "http://hl7.org/fhir") {

	n <- xml2::xml_ns( xml )

	names( n )[ n == ns ][ 1 ]
}

get_fhir_ns <- function(xml) {

	get_ns( xml )
}

use_ns_id <- function(xpath, ns.id) {

	pattern <- "\\'.+\\'"

	strings <- stringr::str_extract_all(xpath, pattern)

	xpath.without.strings <- gsub(pattern,"ENTFERNTER_STRING",xpath)

	repl <- paste0(ns.id, ":\\2")

	d <- gsub(
		"(\\[)([^@/])",
		paste0("\\[", repl),
		gsub(
			"(/)([^@/])",
			paste0("/", repl),
			gsub(
				"(^)([^\\.\\*@/])",
				repl,
				xpath.without.strings
			)
		)
	)

	for( s in strings ) {

		#dbg
		#s<-strings[[1]]

		if (0<length(s)) d <- sub("ENTFERNTER_STRING",s,d)
	}

	d
}

rec <- function( x, fun = attributes, max.level = 0x100 ) {

	if( max.level < 1 ) return( NULL )

	if( is.list( x ) ) {

		if( length( x ) < 1 ) {

			fun( x )
		}
		else {

			lapply( x, rec, fun, max.level - 1 )
		}
	}
	else fun( x )
}

rbind_list_of_data_frames <- function( list ) {

	unique.names <- unique(
		Reduce(
			union,
			sapply(
				list,
				function( l ) {
					names( l )
				}
			)
		)
	)

	d  <- data.frame( as.list( character( length( unique.names ) ) ), stringsAsFactors = F )

	names( d ) <- unique.names

	for( l in list ) {

		#dbg
		#l <- list[[ 1 ]]

		n <- nrow( d )

		m <- nrow( l )

#		d[ ( n + 1 ) : ( n + m ), ] <- d[ 1, ]

		d[ ( n + 1 ) : ( n + m ), names( l ) ] <- l[ , names( l ), drop = F]
	}

	if( 1 < nrow( d ) ) d <- d[ 2 : nrow( d ), , drop = F]

	d
}

#' Concatenate paths
#' @description Concatenates to path string correctly.
#'
#' @param path1 A string specifying the left hand part of final path.
#' @param path2 A string specifying the right hand part of final path.
#' @param os A string specifying theoperating system you're operating on: windows or linux.
#'
#' @return A string containing the concatenated path.
#' @export
#'
#' @examples
#' paste_paths("data", "patients")
#' paste_paths("/data", "patients")
#' paste_paths("/data/", "patients")
#' paste_paths("/data", "/patients")
#' paste_paths("/data/", "/patients/")
#' paste_paths("data", "patients", "windows")

paste_paths <- function(path1="w", path2="d", os = "LiNuX") {

	os <- tolower(substr(os, 1, 1))

	if (os == "w") {

		return(paste0(sub( "\\\\$" , "", path1), "\\", sub( "^\\\\", "", path2)))
	}

	paste0(sub("/$" , "", path1), "/", sub("^/", "", path2))
}

get_attributes <- function(xml, xpath, ns.id = NULL) {

	if(is.null(xml)) {

		warning("Argument xml is NULL, returning NULL.")

		return(NULL)
	}

	if(is.null(xpath)) {

		warning("Argument xpath is NULL, returning NULL.")

		return(NULL)
	}

	if(is.null(ns.id)) {ns.id <- get_fhir_ns(xml)}

	xpath <- use_ns_id(xpath, ns.id)

	xml.  <- xml2::xml_find_all(xml, xpath)

	xml2::xml_text(xml.)
}

get_bundle <- function(request, username = NULL, password = NULL, verbose = T, max.attempts = 5, delay.between.attempts = 10) {

	#dbg
	#request="https://hapi.fhir.org/baseR4/Medication?_format=xml"

	for(n in 1 : max.attempts) {

		#dbg
		#n <- 1

		if(verbose) cat(paste0("(", n, "): ", request, "\n"))

		auth <- if (!is.null(username) & !is.null(password)){

			httr::authenticate(username, password)
		}

		response <- try(
			httr::GET(
				request,
				httr::add_headers(Accept = "application/fhir+xml"),
				httr::content_type("application/fhir+xml;charset=utf-8"),
				auth
			),
			silent = T
		)

		if (class(response)[1] != "try-error") {

			payload <- try(httr::content(response, as = "text", encoding = "UTF-8"), silent = T)

			if (class(payload)[1] != "try-error") {

				xml <- try(xml2::read_xml(payload), silent = T)

				if(class(xml)[1] != "try-error") {

					return(xml)
				}
			}
		}

		Sys.sleep(10)
	}

	NULL
}


#' Download fhir search result
#' @description Downloads all fhir bundles of a fhir search request from a fhir server.
#'
#' @inheritParams get_bundle
#' @param max.bundles Maximal number of bundles to get. Defaults to Inf meaning all available bundles are downloaded.
#'
#' @return A list of bundles in xml format.
#' @export
#'
#' @examples
#' \dontrun{
#' bundles <- fhir_search("https://hapi.fhir.org/baseR4/Medication?", max.bundles=10)
#' }

fhir_search <- function(request, username = NULL, password = NULL, max.bundles = Inf, verbose = T, max.attempts = 5, delay.between.attempts = 10) {

	bundles <- list()

	addr <- request

	cnt <- 0

	repeat {

		if (verbose) {cat(paste0("bundle[", cnt <- cnt + 1, "]"))}

		bundle <- get_bundle(request = addr, username = username, password = password, verbose = verbose, max.attempts = max.attempts, delay.between.attempts = delay.between.attempts)

		if (is.null(bundle)) {

			message("download interrupted.\n")

			break
		}

		bundles[[addr]] <- bundle

		ns.id <- get_fhir_ns( bundle )

		links <- xml2::xml_find_all(bundle, use_ns_id("link", ns.id))

		rels.nxt  <- xml2::xml_attr(xml2::xml_find_first(links, use_ns_id("./relation", ns.id)), "value") == "next"

		if (cnt == max.bundles) {

			if(any(!is.na(rels.nxt) & rels.nxt)) {

				message(
					"\nDownload completed. Number of downloaded bundles was limited to ",
					cnt,
					" bundles, this is less than the total number of bundles available.\n"
				)
			}
			else {

				message("\nDownload completed. All available bundles were downloaded.\n")
			}

			break
		}

		if (!any(!is.na(rels.nxt) & rels.nxt)) {

			message("\nDownload completed. All available bundles were downloaded.\n")

			break
		}

		urls  <- xml2::xml_attr(xml2::xml_find_first(links, use_ns_id("./url",ns.id)), "value")

		addr <- urls[rels.nxt][1]

		if(is.null(addr) || is.na(addr) || length(addr) < 1 || addr == "") {

			message("\nDownload completed. All available bundles were downloaded.\n")

			break
		}
	}

	bundles
}


#' Save bundles as xml-files
#' @description Writes all fhir bundles as numbered xml files into a directory.
#'
#' @param bundles A list of xml objects representing the pages of a fhir bundle.
#' @param directory A string containing the path to the folder to store the data in.
#'
#' @return Nothing to return.
#' @export
#'
#' @examples
#' \dontrun{
#' save_bundles(bundles, "result")
#' }

save_bundles <- function(bundles, directory = "result") {

	w <- 1 + floor(log10(length(bundles)))

	if (!dir.exists(directory))

		dir.create(directory, recursive = T)

	for (n in 1:length(bundles)) {

		xml2::write_xml(bundles[[n]], paste_paths(directory, paste0(stringr::str_pad(n, width = w, pad = "0"), ".xml")))
	}
}


#' Load bundles from xml-files
#' @description Reads all bundles stored as xml files from a directory.
#'
#' @param directory A string containing the path to the folder were the data are stored.
#'
#' @return A list of bundles in xml format.
#' @export
#'
#' @examples
#' \dontrun{
#' bundles.bak <- load_bundles("result")
#' }

load_bundles <- function(directory) {

	xml.files <- dir(directory, "*.xml")

	lapply(lst(xml.files), function(x) xml2::read_xml( paste_paths(directory, x)))
}


xtrc_all_columns <- function( child, sep = " -+- " ) {

	s <- lapply(xml2::as_list(child), rec, attributes)

	t <- as.list(unlist(s))

	d <- as.data.frame(t, stringsAsFactors = F)

	n <- names(d)

	un <- n[grep( "\\.[^0-9]+$", n)]

	l <- lapply(
		lst(un),
		function(m) {

			#dbg
			#m <- un[[ 11 ]]

			paste0(d[ , n[gsub("\\.[0-9]+$", "", n) == m] ], collapse = sep)
		}
	)

	as.data.frame(l, stringsAsFactors = F)
}


xtrc_columns <- function( child, df.columns, sep = " -+- ", ns.id = NULL) {

	l <- lapply(
		lst(names(df.columns)),
		function(column.name)  {

			#dbg
			#column.name <- names( df.columns )[ 1 ]

			i.srch <- df.columns[[column.name]]

			val  <- get_attributes(xml = child, xpath = i.srch, ns.id = ns.id)

			if(is.na(val) || length(val) < 1 ) {

				NA

			} else if (1 < length(val)) {

				paste0(val, collapse = sep)

			} else {

				val
			}
		}
	)

	as.data.frame(l, stringsAsFactors = F)
}


bundle2df <- function(bundle, design.df, sep = " -+- ", ns.id = NULL) {

	if (is.null(bundle)) {

		warning("Argument bundle is NULL, returning NULL.")

		return(NULL)
	}

	if (is.null(design.df)) {

		warning("Argument design.df is NULL, returning NULL.")

		return(NULL)
	}

	if (length(design.df)<1) {

		warning("Argument design.df has length 0, returning NULL.")

		return(NULL)
	}

	if (is.null(ns.id)) ns.id <- get_fhir_ns(bundle)

	xpath <- design.df[[1]]

	children <- xml2::xml_find_all(bundle, use_ns_id(xpath, ns.id))

	df.list <- lapply(
		children,
		function(child) {

			#dbg
			#child <- children[[ 2 ]]

			cat( "." )

			if (length(design.df)<2) {
				xtrc_all_columns(child,sep)
			}
			else{
				df.columns <- design.df[[2]]
				xtrc_columns( child, df.columns, sep = sep, ns.id = ns.id)
			}
		}
	)

	rbind_list_of_data_frames(list = df.list)
}


bundles2df <- function(bundles, design.df, sep = " -+- ", ns.id = NULL) {

	if (is.null(bundles)) {

		warning("Argument bundles is NULL, returning NULL.")

		return(NULL)
	}

	if (is.null(design.df)) {

		warning("Argument design.df is NULL, returning NULL.")

		return(NULL)
	}

	if (length(design.df)<1) {

		warning("Argument design has length 0, returning NULL.")

		return(NULL)
	}

	rbind_list_of_data_frames(
		lapply(
			seq_len(length(bundles)),
			function( i ) {

				#dbg
				#i<-1

				cat( i )

				bundle <- bundles[[ i ]]

				bundle2df( bundle, design.df, sep, ns.id)
			}
		)
	)
}


bundles2dfs <- function(bundles, design, sep = " -+- ", ns.id = NULL) {

	if (is.null(bundles)) {

		warning("Argument bundles is NULL, returning NULL.")

		return(NULL)
	}

	if (is.null(design)) {

		warning("Argument design.df is NULL, returning NULL.")

		return(NULL)
	}

	if (length(design)<1) {

		warning("Argument design has length 0, returning NULL.")

		return(NULL)
	}

	lapply(
		lst(names(design)),
		function(n) {

			#dbg
			#n <- names(design)[1]

			design.df <- design[[n]]

			cat(paste0("\n", n, "\n"))

			bundles2df(bundles = bundles, design.df = design.df, sep = sep, ns.id = ns.id)
		}
	)
}


#' Crack it!
#'
#' @param bundles A list of fhir bundles as xml docs.
#' @param design A named list specifiying which data.frame should contain which entries of the bundle.
#' The names correspond to the names of the resulting data.frames. Each element of design is a list of length 2 where
#' the first element is a XPath expression to locate the entry in a fhir bundle page and second element is a named list
#' with XPath expressions of locations to the values of the items in the bundle page. The names of this second element named
#' list are the column names of the resulting data.frames.
#' @param sep A string to separate pasted multiple entries.
#' @param ns.id A string containing the namespace id.
#'
#' @return A list of data frames as specified by \code{design}
#'
#' @export
crack <- function(bundles, design, sep = " -+- ", ns.id = NULL) {

	bundles2dfs(bundles, design, sep = " -+- ", ns.id = NULL)
}


#' Get capability statement
#' @description Get the capability statement about a fhir server.
#'
#' @param url The url of the fhir server endpoint.
#' @param sep A string to separate pasted multiple entries
#' @param remove.empty.columns Logical Scalar. Remove empty columns?
#'
#' @return A data frame with the capability statement.
#' @export
#'
#' @examples
#' \dontrun{
#' capability_statement("https://hapi.fhir.org/baseR4")
#' }
capability_statement <- function(url = "https://hapi.fhir.org/baseR4", sep = " -+- ", remove.empty.columns = T) {

	caps <- fhir_search(request = paste_paths(url, "/metadata?_format=xml&_pretty=true"))

	design <- list(
		META = list(
			"/CapabilityStatement",
			list(
				id               = "id/@value",
				meta.versionId   = "meta.versionId/@value",
				meta.lastUpdated = "meta/@value",
				language         = "language/@value",
				url              = "url/@value",
				version          = "version/@value",
				name             = "name/@value",
				status           = "status/@value",
				experimental     = "experimental/@value",
				date             = "date/@value",
				publisher        = "publisher/@value",
				contact.name     = "contact/name/@value",
				contact.telecom.system = "contact/telecom/system/@value",
				contact.telecom.value  = "contact/telecom/value/@value",
				contact.telecom.use    =  "contact/telecom/use/@value",
				kind                   = "kind/@value",
				status    = "status/@value",
				date      = "date/@value",
				publisher = "publisher/@value",
				kind      = "kind/@value",
				software.name = "software/name/@value",
				software.version = "software/version/@value",
				implementation.description = "implementation/description/@value",
				implementation.url         = "implementation/url/@value",
				fhirVersion                = "fhirVersion/@value",
				fhirVersion.format         = "format/@value"
			)
		),
		REST.META = list(
			"/CapabilityStatement/rest",
			list(
				extension.url      = "extension/@url",
				extension.valueUri = "extension/valueUri/@value",
				mode               = "mode/@value"
			)
		),
		REST = list( "/CapabilityStatement/rest/resource")
	)

	dfs <- crack(bundles = caps, design = design, sep = sep)

	if(remove.empty.columns) {

		dfs <- lapply(
			dfs,
			function( df ) {

				df[ , sapply( df, function( col ) 0 < sum( ! is.na( col ) ) ), drop = F ]
			}
		)
	}

	dfs
}