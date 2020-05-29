usethis::use_package( "xml2" )
usethis::use_package( "stringr" )
usethis::use_package( "httr" )

#' th
#' @description add the right suffix to a number or a vector of numbers. e.g. 1st 2nd 3rd ...
#'
#' @param n a number or a vector of numbers.
#'
#' @return a vector of strings
#' @export
#'
#' @examples
#' th( 0 : 4 )
th <- function( n ) {

	n.th <- n < 1 | 3 < n
	n.1st <- n == 1
	n.2nd <- n == 2
	n.3rd <- n == 3

	n[ n.th ] <- paste0( n[ n.th ], "th" )
	n[ n.1st ] <- "1st"
	n[ n.2nd ] <- "2nd"
	n[ n.3rd ] <- "3rd"

	n
}


#' lst
#'@description transforms a vector of items to a named list. The names are created with a prefix and a suffix surrounding the items.
#'
#' @param ... a scalar or a vector of scalars.
#' @param prefix a prefix in front of the result text.
#' @param suffix a suffix after the result text.
#'
#' @return a named list, where the names are the content surrounded by a prefix and a suffix
#' @export
#'
#' @examples
#' lst( LETTERS[ 1 : 5 ], pre = "id[", post = "]" )
lst <- function( ..., prefix = NULL, suffix = NULL ) {

	v <- as.list( c( ... ) )

	names( v ) <- paste0( prefix, v, suffix )

	v
}


#' concatenate.paths
#' @description concatenates to paths strings correctly
#'
#' @param path1 the left hand part of final path.
#' @param path2 the right hand part of final path.
#' @param os the operating system you'r operating on: windows or linux.
#'
#' @return a
#' @export
#'
#' @examples
#' concatenate.paths( "data", "patients" )
#' concatenate.paths( "/data", "patients" )
#' concatenate.paths( "/data/", "patients" )
#' concatenate.paths( "/data", "/patients" )
#' concatenate.paths( "/data/", "/patients/" )
#' concatenate.paths( "data", "patients", "windows" )
concatenate.paths <- function( path1="w", path2="d", os = "LiNuX" ) {

	os <- tolower( substr( os, 1, 1 ) )

	if( os == "w" ) {

		return( paste0( sub( "\\\\$" , "", path1 ), "\\", sub( "^\\\\", "", path2 ) ) )
	}

	paste0( sub( "/$" , "", path1 ), "/", sub( "^/", "", path2 ) )
}


#' download.bundle
#' @description downloads a fhir bundle via fhir search request and return it as xml file.
#'
#' @param fhir.search.request a fhir search request. it must contain _format=xml.
#' @param max.attempts the maximal number of attempts to send a request. Default is 5.
#' @param username a string containing the username for basic authentification. Defaults to NULL, meaning no authentification.
#' @param password a string containing the passwort for basic authentification. Defaults to NULL, meaning no authentification.
#'
#' @return the downloaded bundle in xml format.
#' @export
#'
#' @examples
#' \dontrun{
#' download.bundle( "https://hapi.fhir.org/baseR4/Medication?_format=xml" )
#' }
download.bundle <- function( fhir.search.request, max.attempts = 5, username=NULL, password=NULL ) {

	#dbg
	#fhir.search.request="https://hapi.fhir.org/baseR4/Medication?_format=xml"

	for( n in 1 : max.attempts ) {

		#dbg
		#n <- 1

		cat( paste0( th( n ), " attempt to download: ", fhir.search.request, "\n" ) )

		auth <- NULL

		r <- try(
			{
				if( ! is.null( username ) & ! is.null( password ) ) {

					auth <- httr::authenticate( username, password )
				}

				response <- httr::GET(
					fhir.search.request,
					httr::add_headers( Accept = "application/fhir+xml" ),
					httr::content_type( "application/fhir+xml;charset=utf-8" ),
					auth )

				payload <- httr::content( response, as = "text", encoding = "UTF-8" )

				xml2::read_xml( payload )
			}

			#xml2::read_xml( fhir.search.request, silent = T )
		)

		if( class( r )[ 1 ] != "try-error" ) {

			return( r )
		}

		Sys.sleep( 10 )
	}

	NULL
}




#' download.bundles
#' @description downloads all fhir bunde of a fhir search request from a fhir server.
#'
#' @param fhir.search.request a fhir search request
#' @param max.attempts maximal attempts to connect to a page address
#' @param username a string containing the username for basic authentification. Defaults to NULL, meaning no authentification.
#' @param password a string containing the passwort for basic authentification. Defaults to NULL, meaning no authentification.
#'
#' @return the downloaded bundles as a list of pages in xml format
#' @export
#'
#' @examples
#' \dontrun{
#' bundles <- download.bundles( "https://vonk.fire.ly/R4/Medication?_format=xml" )
#' }
download.bundles <- function( fhir.search.request, max.attempts = 5, username = NULL, password = NULL ) {

	xmls <- list( )

	addr <- fhir.search.request

	repeat {

		xml <- download.bundle( addr, max.attempts, username, password )

		if( is.null( xml ) ) {

			cat( "download interrupted.\n" )

			break
		}

		xml2::xml_ns_strip( xml )

		xmls[[ addr ]] <- xml

		links <- xml2::xml_find_all( xml, "link" )

		rels.nxt  <- xml2::xml_attr( xml2::xml_find_first( links, "./relation" ), "value" ) == "next"

		if( ! any( ! is.na( rels.nxt ) & rels.nxt ) ) {

			cat( "\ndownload completed\n" )

			break
		}

		urls  <- xml2::xml_attr( xml2::xml_find_first( links, "./url" ), "value" )

		addr <- urls[ rels.nxt ][ 1 ]

		if( is.null( addr ) || is.na( addr ) || length( addr ) < 1 || addr == "" ) {

			cat( "\ndownload completed\n" )

			break
		}
	}

	xmls
}


#' write.bundles
#' @description writes all fhir bundle as numbered xml files into a directory.
#'
#' @param bundles a list of xml text files representing the pages of a fhir bundle.
#' @param directory the location to store the data.
#'
#' @return nothing to return.
#' @export
#'
#' @examples
#' \dontrun{
#' write.bundles( bundles, "result" )
#' }
write.bundles <- function( bundles, directory = "result" ) {

	w <- 1 + floor( log10( length( bundles ) ) )

	if( ! dir.exists( directory ) )

		dir.create( directory, recursive = T )

	for( n in 1 : length( bundles ) ) {

		xml2::write_xml( bundles[[ n ]], concatenate.paths( directory, paste0( stringr::str_pad( n, width = w, pad = "0" ), ".xml" ) ) )
	}
}


#' read.bundles
#' @description reads all bundles stored as xml files from a directory
#'
#' @param directory the location the data are stored.
#'
#' @return the bundles as a list of xml text files.
#' @export
#'
#' @examples
#' \dontrun{
#' bundles.bak <- read.bundles( "result" )
#' }
read.bundles <- function( directory ) {

	xml.files <- dir( directory, "*.xml" )

	lapply( lst( xml.files ), function( x ) xml2::read_xml( concatenate.paths( directory, x ) ) )
}


#' bundle.to.dataframes
#' @description converts a fhir bundle to a list of data frames.
#' design is a named list. Its names are the one of the resulting tables.
#' The elements of design are lists of 2 elements.
#' The first one is a XPath expression to locate the entry in a fhir bundle page.
#' The second one is a named list with XPath expressions of locations to the values of the items in the bundle page.
#' The names are the column names of the resultung data frames
#'
#' @param bundle a xml text file the represents a fhir bundle.
#' @param design a structure that specifies which table should contain which entries of the bundle.
#'
#' @return a list of data frames
#' @export
#'
#' @examples
#' \dontrun{
#' bundle.to.dataframes(
#' bundles[[ 1 ]],
#' design = list(
#' MEDICATION = list(
#'  tag     = ".//Medication",
#'  items   = list(
#' 	  SYSTEM  = "code/coding/system/@value",
#' 	  CODE    = "code/coding/code/@value",
#' 	  DISPLAY = "code/coding/display/@value"
#' 	) ) ) )
#' 	}
bundle.to.dataframes <- function( bundle, design, sep = "›" ) {

	#dbg
	#bundle <- bundles[[ 1 ]]

	if( is.null( bundle ) ) return( NULL )

	xml2::xml_ns_strip( bundle )

	lapply(
		lst( names( design ) ),
		function( n.e ) {

			#dbg
			#n.e <- names( design )[ 1 ]

			cat( n.e )

			e <- design[[ n.e ]]

			entry <- e[[ 1 ]]
			items <- e[[ 2 ]]

			bundle.entry <- xml2::xml_find_all( bundle, entry )

			r <- Reduce(
				rbind,
				lapply(
					bundle.entry,
					function( tg ) {

						#dbg
						#tg <- bundle.entry[[ 1 ]]

						s <- sapply(
							names( items ),
							function( i.n )  {

								#dbg
								#i.n <- names( items )[ 1 ]

								i.srch <- items[[ i.n ]]

								addr <- sub( "/@[a-zA-Z0-9]+$", "", i.srch )
								item <- sub( "^.*/@", "", i.srch )
								val  <- xml2::xml_attr( xml2::xml_find_all( tg, addr ), item )

								if( is.character( val ) ) {

									if( length( val ) < 1 ) val <- NA

									else if( 1 < length( val ) ) val <- paste0( val, collapse = sep )
								}

								val
							}
						)

						cat( "." )

						s
					}
				)
			)

			cat( "\n" )

			as.data.frame( r, stringsAsFactors = F )
		}
	)
}


#' bundles.to.dataframes
#' @description converts all fhir bundles to a list of data frames
#'
#' @param bundles a list of xml text files representing the fhir bundles.
#' @param design a structure that specifies which table should contain which entries of the bundle.
#'
#' @return a list of data frames.
#' @export
#'
#' @examples
#' \dontrun{
#' bundles.to.dataframes( bundles, design )
#' }
bundles.to.dataframes <- function( bundles, design, sep = "›" ) {

	bundle.dfs <- lapply(
		bundles,
		function( x ) {

			bundle.to.dataframes( x, design, sep )
		}
	)

	if( 1 < length( bundle.dfs ) ) {

		d <- lapply(
			lst( names( design ) ),
			function( n ) {

				#dbg
				#n<-names( design )[ 1 ]
				as.data.frame(
					Reduce(
						rbind,
						lapply(
							bundle.dfs,
							function( dfs ) {
								#dbg
								#dfs <- bundle.dfs[[ 1 ]]
								dfs[[ n ]]
							}
						)
					),
					fix.empty.names = T,
					stringsAsFactors = FALSE
				)
			}
		)
	}
	else {

		d <- bundle.dfs[[ 1 ]]
	}

	cat( "\n" )

	d
}
#
#
# b <- download.bundles( "https://vonk.fire.ly/R4/Observation?_include=Observation:encounter&_include=Observation:patient&_format=xml&_pretty=true&_count=1000000")
#
# d <- list(
# 	Besuch = list(
# 		".//Observation",
# 		list(
# 			OID     = "id/@value",
# 			PID     = "subject/reference/@value",
# 			WERT    = "valueQuantity/value/@value",
# 			EINHEIT = "valueQuantity/unit/@value",
# 			TEXT    = "code/text/@value",
# 			CODE    = "code/coding/code/@value",
# 			DATUM   = "effectiveDateTime/@value"
# 		)
# 	),
# 	Aufnahme = list(
# 		".//Encounter",
# 		list(
# 			EID           = "id/@value",
# 			PATIENTEN.ID  = "subject/reference/@value",
# 			TEILNEHMER.ID = "participant/individual/reference/@value",
# 			BEGINN        = "period/start/@value",
# 			ENDE          = "period/end/@value",
# 			SYSTEM        = "class/system/@value",
# 			CODE          = "class/code/@value",
# 			DISPLAY       = "class/display/@value"
# 		)
# 	),
# 	Patient = list(
# 		".//Patient",
# 		list(
# 			PID             = "id/@value",
# 			NAME.VERWENDUNG = "name/use/@value",
# 			VORNAME         = "name/given/@value",
# 			NACHNAME        = "name/family/@value",
# 			SEX             = "gender/@value",
# 			BIRTHDAY        = "birthDate/@value"
# 		)
# 	)
# )
#
# dfs <- bundle.to.dataframes( b, d )
# dfs$Besuch


#' tag.attr
#'@description extracts an attribute from tags.
#'
#' @param bundle fhir bundle
#' @param xpath the path to the tag's attribute in the xml document.
#'
#' @return a single value or vector
#' @export
#'
#' @examples
#' \dontrun{
#' tag.attr( bundles[[ 1 ]], xpath = ".//total/@value" )
#' }
tag.attr <- function( bundle, xpath ) {

	addr <- sub( "/@[a-zA-Z0-9]+$", "", xpath )
	item <- sub( "^.*/@", "", xpath )

	xp <- concatenate.paths( "/Bundle", addr )

	xml2::xml_attr( xml2::xml_find_all( bundle, xp ), item )
}
