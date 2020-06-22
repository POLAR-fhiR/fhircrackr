lst <- function(..., prefix = NULL, suffix = NULL) {

	v <- as.list(c(...))

	names(v) <- paste0(prefix, v, suffix)

	v
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

	#d  <- data.frame( as.list( character( length( unique.names ) ) ), stringsAsFactors = F )
	d <- as.data.frame(lapply(seq_along(unique.names),function(dummy)character(0)), stringsAsFactors = F)

	names( d ) <- unique.names

	for( l in list ) {

		#dbg
		#l <- list[[ 1 ]]

		n <- nrow( d )

		m <- nrow( l )

		d[ ( n + 1 ) : ( n + m ), names( l ) ] <- l[ , names( l ), drop = F]
	}

	#if( 1 < nrow( d ) ) d <- d[ 2 : nrow( d ), , drop = F]

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
#' @param request A string containing the full fhir search request.
#' @param username A string containing the username for basic authentification. Defaults to NULL, meaning no authentification.
#' @param password A string containing the passwort for basic authentification. Defaults to NULL, meaning no authentification.
#' @param max.attempts A numeric scalar. The maximal number of attempts to send a request, defaults to 5.
#' @param verbose A logical scalar. Should downloading information be printed to the console? Defaults to TRUE.
#' @param delay.between.attempts A scalar numeric specifying the delay in seconds between two attempts. Defaults to 10.
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

		xml2::xml_ns_strip( bundle )

		bundles[[addr]] <- bundle

		links <- xml2::xml_find_all(bundle, "link")

		rels.nxt  <- xml2::xml_attr(xml2::xml_find_first(links, "./relation"), "value") == "next"

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

		urls  <- xml2::xml_attr(xml2::xml_find_first(links, "./url"), "value")

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
#' fhir_save(bundles, "result")
#' }

fhir_save <- function(bundles, directory = "result") {

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
#' bundles.bak <- fhir_load("result")
#' }

fhir_load <- function(directory) {

	xml.files <- dir(directory, "*.xml")

	lapply(lst(xml.files), function(x) xml2::read_xml( paste_paths(directory, x)))
}


# xtrct_all_columns <- function( child, sep = " -+- " ) {
#
# 	s <- lapply(xml2::as_list(child), rec, attributes)
#
# 	t <- as.list(unlist(s))
#
# 	d <- as.data.frame(t, stringsAsFactors = F)
#
# 	n <- names(d)
#
# 	un <- n[grep( "\\.[^0-9]+$", n)]
#
# 	l <- lapply(
# 		lst(un),
# 		function(m) {
#
# 			#dbg
# 			#m <- un[[ 11 ]]
#
# 			d. <- d[ , n[gsub("\\.[0-9]+$", "", n) == m] ]
# 			paste0("{",seq_len(length(d.)), "}", d., collapse = sep)
# 			#paste0(seq_along(val), "[", val, "]")
#
# 		}
# 	)
#
# 	as.data.frame(l, stringsAsFactors = F)
# }


xtrct_all_columns <- function( child, sep = " -+- ", add_ids = F, xpath = ".//@*") {

	tree <- xml2::xml_find_all(child, xpath)

	xp.child  <- xml2::xml_path( child )
	xp.remain <- xml2::xml_path( tree )
	xp.rel    <- substr( xp.remain, nchar( xp.child ) + 2, nchar( xp.remain ) )
	xp.cols   <- gsub("/", ".", gsub("@", "", unique( gsub( "\\[[0-9]+\\]", "", xp.rel))))

	d <- lapply(1:length(xp.cols),function(dummy)character(0))

	names( d ) <- xp.cols

	val  <- xml2::xml_text(tree)

	s <- stringr::str_split( xp.rel, "/" )

	o <- sapply(
		seq_along( s ),
		function( i ) {

			#dbg
			#i<-1

			s. <- s[[ i ]]

			i.f <- ! grepl( "\\[[0-9]+\\]", s. )

			if( any( i.f ) ) {

				s.[ i.f ] <- paste0( s.[ i.f ], "[1]" )
			}

			c(
				gsub(".1$", "", paste0(gsub( "[^0-9]", "", s. ), collapse = "." )),
				gsub( "@", "", gsub( "\\[[0-9]+\\]", "", paste0(s., collapse = "." )))
			)
		}
	)

	if( add_ids ) {

		val  <- paste0( "{",o[ 1, ], "}", val)
	}

	for( col in xp.cols ) {

		#dbg
		#col <- xp.cols[1]

		d[[ col ]] <- paste0( val[ col == o[ 2, ] ], collapse = sep )
	}

	as.data.frame(d, stringsAsFactors = F)
}

xtrct_columns <- function( child, df.columns, sep = " -+- ", add_ids = F) {

	xp <- xml2::xml_path( child )

	l <- lapply(
		lst(names(df.columns)),
		function(column.name)  {

			#dbg
			#column.name <- names( df.columns )[ 2 ]

			i.srch <- df.columns[[column.name]]
			#TODO: mask via flags 1:n
			loc <- xml2::xml_find_all(x = child, xpath = i.srch)

			val  <- xml2::xml_text(loc)

			if( add_ids ) {

				loc.xp <- xml2::xml_path( loc )

				loc.xp.rel <- substr( loc.xp, nchar( xp ) + 2, nchar( loc.xp ) )

				s <- stringr::str_split( loc.xp.rel, "/" )

				o <- sapply(
					seq_along( s ),
					function( i ) {

						#dbg
						#i<-1

						s. <- s[[ i ]]

						i.f <- ! grepl( "\\[[0-9]+\\]", s. )

						if( any( i.f ) ) {

							s.[ i.f ] <- paste0( s.[ i.f ], "[1]" )
						}

						gsub(".1$", "", paste0(gsub( "[^0-9]", "", s. ), collapse = "." ))
					}
				)

				if(is.na(val) || length(val) < 1 ) {

					NA

				} else if (1 < length(val)) {

					paste0("{",o,"}", val, collapse = sep)
					#paste0(val, collapse = sep)

				} else {

					paste0("{",o,"}", val, collapse = sep)
				}
			}
			else {

				if(is.na(val) || length(val) < 1 ) {

					NA

				} else if (1 < length(val)) {

					paste0(val, collapse = sep)
					#paste0(val, collapse = sep)

				} else {

					paste0(val, collapse = sep)
				}
			}
		}
	)

	as.data.frame(l, stringsAsFactors = F)
}


bundle2df <- function(bundle, design.df, sep = " -+- ", add_ids = F) {

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

	xml2::xml_ns_strip(bundle)

	xpath <- design.df[[1]]

	children <- xml2::xml_find_all(bundle, xpath)

	df.list <- lapply(
		children,
		function(child) {

			#dbg
			#child <- children[[ 1 ]]

			cat( "." )

			if (1<length(design.df) && is.list(design.df[[2]])) {

				df.columns <- design.df[[2]]

				xtrct_columns( child, df.columns, sep = sep, add_ids = add_ids)
			}
			else{

				xp <- if(1<length(design.df)) design.df[[2]] else ".//@*"
				xtrct_all_columns(child = child, sep = sep, add_ids = add_ids, xpath = xp)
			}
		}
	)

	rbind_list_of_data_frames(list = df.list)
}


bundles2df <- function(bundles, design.df, sep = " -+- ", add_ids = F) {

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

	ret <- rbind_list_of_data_frames(
		lapply(
			seq_len(length(bundles)),
			function( i ) {

				#dbg
				#i<-1

				cat( "\n", i )

				bundle <- bundles[[ i ]]

				bundle2df( bundle, design.df, sep, add_ids)
			}
		)
	)

	cat( "\n" )

	ret
}


bundles2dfs <- function(bundles, design, sep = " -+- ", add_ids = F) {

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

	l <- lapply(
		lst(names(design)),
		function(n) {

			#dbg
			#n <- names(design)[3]

			design.df <- design[[n]]

			cat("\n", n)

			bundles2df(bundles = bundles, design.df = design.df, sep = sep, add_ids)
		}
	)

	cat("\n")

	l
}


#' fhir_crack it!
#'
#' @param bundles A list of fhir bundles as xml docs.
#' @param design A named list specifiying which data.frame should contain which entries of the bundle.
#' The names correspond to the names of the resulting data.frames. Each element of design is a list of length 2 where
#' the first element is a XPath expression to locate the entry in a fhir bundle page and second element is a named list
#' with XPath expressions of locations to the values of the items in the bundle page. The names of this second element named
#' list are the column names of the resulting data.frames.
#' @param sep A string to separate pasted multiple entries.
#' @param add_ids = Logigal Scalar. Should indices be added to multiple entries?
#'
#' @return A list of data frames as specified by \code{design}
#'
#' @export
fhir_crack <- function(bundles, design, sep = " -+- ", add_ids = F) {

	bundles2dfs(bundles, design, sep, add_ids)
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
#' fhir_cs("https://hapi.fhir.org/baseR4")
#' }
fhir_cs <- function(url = "https://hapi.fhir.org/baseR4", sep = " -+- ", remove.empty.columns = T) {

	caps <- fhir_search(request = paste_paths(url, "/metadata?_format=xml&_pretty=true"))

	# design <- list(
	# 	META      = list( "/CapabilityStatement", "./*/@value", 1 ),
	# 	REST.META = list( "/CapabilityStatement/rest", "./*/@value", 3 ),
	# 	REST      = list( "/CapabilityStatement/rest/resource")
	# )
	# design <- list(
	# 	META      = list( "/CapabilityStatement", "//@value", 1 ),
	# 	REST.META = list( "/CapabilityStatement/rest", 2, 3 ),
	# 	REST      = list( "/CapabilityStatement/rest/resource")
	# )
	design <- list(
		META      = list("/CapabilityStatement", "./*/@*"),
		REST.META = list("/CapabilityStatement/rest", "./*/@*"),
		REST      = list("/CapabilityStatement/rest/resource")
	)

	dfs <- fhir_crack(bundles = caps, design = design, sep = sep)

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
