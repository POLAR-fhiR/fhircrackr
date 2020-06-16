usethis::use_package("xml2")
usethis::use_package("stringr")
usethis::use_package("httr")
usethis::use_package("utils")

#' Create count strings
#' @description Add the right suffix to a number or a vector of numbers. e.g. 1st 2nd 3rd ...
#'
#' @param n A numeric vector containing one or more numbers.
#'
#' @return A character vector containing the converted numbers as strings.
th <- function(n) {

	n.th <- n < 1 | 3 < n
	n.1st <- n == 1
	n.2nd <- n == 2
	n.3rd <- n == 3

	n[n.th] <- paste0(n[n.th], "th")
	n[n.1st] <- "1st"
	n[n.2nd] <- "2nd"
	n[n.3rd] <- "3rd"

	n
}


#' Transform vector to named list
#'@description Transforms a vector of items to a named list. The names are created with a prefix and a suffix surrounding the items.
#'
#' @param ... A vector that can be coerced to a character.
#' @param prefix A string taken as the prefix for the names of the list elements.
#' @param suffix A string taken as the suffix for the names of the list elements.
#'
#' @return A named list, where the names are the content surrounded by a prefix and a suffix.
lst <- function(..., prefix = NULL, suffix = NULL) {

	v <- as.list(c(...))

	names(v) <- paste0(prefix, v, suffix)

	v
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



#' Extract paths
#'@description Extracts an attribute from tags in a xml object.
#'
#' @param xml A xml document, node, or node set.
#' @param xpath A string containing a xpath (1.0) expression
#'
#' @return A character vector containing the attribute.
#' @export
#'
#' @examples
#' \dontrun{
#' tag_attr(bundles[[1]], xpath = ".//total/@value")
#' }

tag_attr <- function(xml, xpath) {

	addr   <- sub("/@[a-zA-Z0-9]+$", "", xpath)
	attrib <- sub("^.*/@", "", xpath)

	xml2::xml_attr(xml2::xml_find_all(xml, addr), attrib)
}



#' Download single fhir bundle
#' @description Downloads a single fhir bundle via fhir search request and return it as a xml object.
#'
#' @param request A string containing the full fhir search request.
#' @param username A string containing the username for basic authentification. Defaults to NULL, meaning no authentification.
#' @param password A string containing the passwort for basic authentification. Defaults to NULL, meaning no authentification.
#' @param max.attempts A numeric scalar. The maximal number of attempts to send a request, defaults to 5.
#' @param verbose A logical scalar. Should downloading information be printed to the console? Defaults to TRUE.
#' @param delay.between.attempts A scalar numeric specifying the delay in seconds between two attempts. Defaults to 10.

#'
#' @return The downloaded bundle in xml format.
#' @export
#'
#' @examples
#' \dontrun{
#' get_bundle(request = "https://hapi.fhir.org/baseR4/Medication?_count=500&_format=xml")
#' }
get_bundle <- function(request, username = NULL, password = NULL, verbose = T, max.attempts = 5, delay.between.attempts = 10) {

	#dbg
	#request="https://hapi.fhir.org/baseR4/Medication?_format=xml"

	for(n in 1 : max.attempts) {

		#dbg
		#n <- 1

		if(verbose) cat(paste0("(", n, "): ", request, "\n"))

		auth <- if (!is.null(username) & !is.null(password)){

			httr::authenticate(username, password)

		} else {

			NULL

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

		xml2::xml_ns_strip(bundle)

		bundles[[addr]] <- bundle

		links <- xml2::xml_find_all(bundle, "link")

		rels.nxt  <- xml2::xml_attr(xml2::xml_find_first(links, "./relation"), "value") == "next"

		if (cnt == max.bundles) {

			if(any(!is.na(rels.nxt) & rels.nxt)) {

				message("\nDownload completed. Number of downloaded bundles was limited to ",
						 cnt,
						 " bundles, this is less than the total number of bundles available.\n"
				)
			}
			else {

				message("\nDownload completed. All available bundles were downloaded.\n")
			}

			break
		}

		if(!any(!is.na(rels.nxt) & rels.nxt)) {

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



#' Flatten xml objects.
#' @description Converts an xml doc or xml node object to one data frame.
#'
#' @param xml An xml doc or xml node object.
#' @param dsgn.df A design for a single data frame.
#' @param sep A string used to separate pasted multiple entries
#'
#' @return A data frame containing the data specified in \code{dsgn.df}.
#' @export
#'
#' @examples
#' \dontrun{
#' xml2df(xml, design$Patient)
#' }
xml2df <- function(xml, dsgn.df, sep = " -+- ") {

	#xml2::xml_ns_strip( xml )
	#dbg
	#dsgn.df  <- design[[ 1 ]]
	#df.xpaths  <- dsgn.df[[ 1 ]]
	df.columns <- dsgn.df[[2]]

	#( entries <- xml2::xml_find_all( bundle, "/Specimen" ) ) #entry )
	#( xml <- xml2::xml_find_all( entries[ 1 ], df.xpath ) )

	s <- sapply(
		lst(names(df.columns)),
		function(column.name)  {

			#dbg
			#column.name <- names( df.columns )[ 1 ]

			i.srch <- df.columns[[column.name]]

			#addr   <- sub( "/@[a-zA-Z_0-9]+$", "", i.srch )
			#attrib <- sub( "^.*/@", "", i.srch )

			#val  <- xml2::xml_attr( xml2::xml_find_all( xml, addr ), attrib )
			val  <- tag_attr(xml, i.srch)

			if(is.na(val) || length(val) < 1 ) {

				NA

			} else if (1 < length(val)) {

				paste0(val, collapse = sep)

			} else {

				val

			}
		}
	)

	as.data.frame(as.list(s), stringsAsFactors = F)
}



#' Flatten single fhir bundle
#' @description Converts a fhir bundle to a list of data frames.

#'
#' @param bundle a xml text file the represents a fhir bundle.
#' @param design A named list specifiying which data.frame should contain which entries of the bundle.
#' The names correspond to the names of the resulting data.frames. Each element of design is a list of length 2 where
#' the first element is a XPath expression to locate the entry in a fhir bundle page and second element is a named list
#' with XPath expressions of locations to the values of the items in the bundle page. The names of this second element named
#' list are the column names of the resulting data.frames.
#' @param sep A string to separate pasted multiple entries.
#'
#' @return A list of data frames as specified by \code{design}
#' @export
#'
#' @examples
#' \dontrun{
#' bundle2dfs(
#' bundles[[1]],
#' design = list(
#' MEDICATION = list(
#'  tag     = ".//Medication",
#'  items   = list(
#' 	  SYSTEM  = "code/coding/system/@value",
#' 	  CODE    = "code/coding/code/@value",
#' 	  DISPLAY = "code/coding/display/@value"
#' 	))))
#' 	}
bundle2dfs <- function(bundle, design, sep = " -+- ") {

	xml2::xml_ns_strip(bundle)

	#dbg
	#bundle <- bundles[[ length( bundle ) ]]

	if(is.null(bundle)) {return(NULL)}

	lapply(
		lst(names(design)),
		function(df.name) {

			#dbg
			#df.name <- names( design )[ 1 ]

			cat(df.name)

			dsgn.df    <- design[[df.name]]
			df.xpaths  <- dsgn.df[[1]]
			df.columns <- dsgn.df[[2]]

			xml.nodeset <- xml2::xml_find_all(bundle, df.xpaths)

			df <- if (1 < length(xml.nodeset)) {

				dfs <- lapply(
					xml.nodeset,
					function(node) {

						cat(".")

						xml2df(node, dsgn.df, sep)
					}
				)

				if (1 < length(dfs)){

					Reduce( rbind.data.frame, dfs )

				} else if (1 == length(dfs)){

					dfs[[1]]

				} else {

					NULL

				}
			} else if (1 == length(xml.nodeset)){

				as.list(xml2df(xml.nodeset[1], dsgn.df, sep))

			} else {

				NULL
			}

			cat("\n")

			as.data.frame(df, stringsAsFactors = F)
		}
	)
}

#' Flatten list of fhir bundles
#' @description Converts all fhir bundles (the result of \code{\link{fhir_search}}) to a list of data frames.
#'
#' @param bundles A fhir search result as returned by \code{\link{fhir_search}}.
#' @inheritParams bundle2dfs
#'
#' @return A list of data framesas specified by \code{design}.
#' @export
#'
#' @examples
#' \dontrun{
#' fhir2dfs(bundles, design)
#' }
fhir2dfs <- function(bundles, design, sep = " -+- ") {

	bundles.dfs <- lapply(
		bundles,
		function(bundle) {

			#dbl
			#bundle <- bundles[[ 11 ]]
			bundle2dfs(bundle, design, sep)
		}
	)

	d <- if (1 < length(bundles.dfs)) {

		lapply(
			lst(names(design)),
			function(n) {

				#dbg
				#n<-names( design )[ 1 ]
				dfs.n <- lapply(
					bundles.dfs,
					function(dfs) {
						#dbg
						#dfs <- bundles.dfs[[ 1 ]]
						dfs[[n]]
					}
				)

				r <- if (1 < length(dfs.n)) {

					Reduce(rbind.data.frame, dfs.n)

				} else if (1 == length(dfs.n)) {

					dfs.n[[1]]

				} else {

					NULL

				}

				if (is.null(r)) {

					NULL

				} else {

					as.data.frame(
						r,
						fix.empty.names = T,
						stringsAsFactors = F,
						row.names = seq_len(nrow(r))
					)

				}
			}
		)
	} else if (1 == length(bundles.dfs)){

		bundles.dfs[[ 1 ]]

	} else {

		NULL

	}

	cat( "\n" )

	d
}


#' Coerce columns
#' @description Coerce a data frame's columns.
#'
#' @param df A data frame with strings as column entries.
#' @param stringsAsFactors Strings as factors.
#'
#' @return A data frame with coerced types.
#'
#' @examples
#' \dontrun{
#' coerce_types(dfs$Besuch)
#' }
coerce_types <- function(df, stringsAsFactors = F) {

	utils::type.convert(df, as.is = !stringsAsFactors)
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

	caps <- fhiR::get_bundle(fhiR::paste_paths(url, "/metadata?_format=xml&_pretty=true"))

	if(is.null(caps)) {

		message("Capability Statement could not be downloaded.")

		return(NULL)
	}

	xml2::xml_ns_strip(caps)

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
		REST = list(
			"/CapabilityStatement/rest/resource",
			list(
				ext.url           = "extension/@url",
				ext.decVal        = "extension/valueDecimal/@value",
				type              = "type/@value",
				profile           = "profile/@value",
				interaction       = "interaction/code/@value",
				searchParam.name  = "searchParam/name/@value",
				searchParam.type  = "searchParam/type/@value",
				searchParam.documentation = "searchParam/documentation/@value",
				versioning        = "versioning/@value",
				conditionalCreate = "conditionalCreate/@value",
				conditionalUpdate = "conditionalUpdate/@value",
				conditionalDelete = "conditionalDelete/@value",
				searchInclude     = "searchInclude/@value"
			)
		)
	)

	dfs <- fhiR::bundle2dfs(bundle = caps, design = design, sep = sep)

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
