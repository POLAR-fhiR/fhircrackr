usethis::use_package( "xml2" )
usethis::use_package( "stringr" )
usethis::use_package( "httr" )
usethis::use_package( "utils" )

#' th
#' @description add the right suffix to a number or a vector of numbers. e.g. 1st 2nd 3rd ...
#'
#' @param n a number or a vector of numbers.
#'
#' @return a vector of strings
#' @export
#'
#' @examples
#' \dontrun{
#' th( 0 : 4 )
#' }
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
#' \dontrun{
#' lst( LETTERS[ 1 : 5 ], pre = "id[", post = "]" )
#' }
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
#' @return a string
#' @export
#'
#' @examples
#' \dontrun{
#' concatenate.paths( "data", "patients" )
#' concatenate.paths( "/data", "patients" )
#' concatenate.paths( "/data/", "patients" )
#' concatenate.paths( "/data", "/patients" )
#' concatenate.paths( "/data/", "/patients/" )
#' concatenate.paths( "data", "patients", "windows" )
#' }
concatenate.paths <- function( path1="w", path2="d", os = "LiNuX" ) {

	os <- tolower( substr( os, 1, 1 ) )

	if( os == "w" ) {

		return( paste0( sub( "\\\\$" , "", path1 ), "\\", sub( "^\\\\", "", path2 ) ) )
	}

	paste0( sub( "/$" , "", path1 ), "/", sub( "^/", "", path2 ) )
}



#' tag.attr
#'@description extracts an attribute from tags.
#'
#' @param xml a document, node, or node set
#' @param xpath a string containing a xpath (1.0) expression
#'
#' @return a single value or vector
#' @export
#'
#' @examples
#' \dontrun{
#' tag.attr( bundles[[ 1 ]], xpath = ".//total/@value" )
#' }
tag.attr <- function( xml, xpath ) {

	addr   <- sub( "/@[a-zA-Z0-9]+$", "", xpath )
	attrib <- sub( "^.*/@", "", xpath )

	xml2::xml_attr( xml2::xml_find_all( xml, addr ), attrib )
}



#' get.bundle
#' @description downloads a fhir bundle via fhir search request and return it as xml file.
#'
#' @param url an url of a fhir bundle. it must contain _format=xml.
#' @param max.attempts the maximal number of attempts to send a request. Default is 5.
#' @param username a string containing the username for basic authentification. Defaults to NULL, meaning no authentification.
#' @param password a string containing the passwort for basic authentification. Defaults to NULL, meaning no authentification.
#'
#' @return the downloaded bundle in xml format.
#' @export
#'
#' @examples
#' \dontrun{
#' get.bundle( "https://hapi.fhir.org/baseR4/Medication?_format=xml" )
#' }
get.bundle <- function( url, max.attempts = 5, username = NULL, password = NULL ) {

	#dbg
	#url="https://hapi.fhir.org/baseR4/Medication?_format=xml"

	for( n in 1 : max.attempts ) {

		#dbg
		#n <- 1

		cat( paste0( "(", n, "):", url, "\n" ) )

		auth <- NULL

		r <- try(
			{
				if( ! is.null( username ) & ! is.null( password ) ) {

					auth <- httr::authenticate( username, password )
				}

				response <- httr::GET(
					url,
					httr::add_headers( Accept = "application/fhir+xml" ),
					httr::content_type( "application/fhir+xml;charset=utf-8" ),
					auth )

				payload <- httr::content( response, as = "text", encoding = "UTF-8" )

				xml2::read_xml( payload, silten = T )
			}

			#xml2::read_xml( url, silent = T )
		)

		if( class( r )[ 1 ] != "try-error" ) {

			return( r )
		}

		Sys.sleep( 10 )
	}

	NULL
}



#' fhir.search
#' @description downloads all fhir bunde of a fhir search request from a fhir server.
#'
#' @param request a fhir search request
#' @param max.attempts maximal attempts to connect to a page address
#' @param username a string containing the username for basic authentification. Defaults to NULL, meaning no authentification.
#' @param password a string containing the passwort for basic authentification. Defaults to NULL, meaning no authentification.
#'
#' @return the downloaded bundles as a list of pages in xml format
#' @export
#'
#' @examples
#' \dontrun{
#' bundles <- fhir.search( "https://vonk.fire.ly/R4/Medication?_format=xml" )
#' }
fhir.search <- function( request, max.attempts = 5, username = NULL, password = NULL ) {

	bundles <- list( )

	addr <- request

	cnt <- 0

	repeat {

		cat( paste0( "bundle[", cnt <- cnt + 1, "]" ) )

		bundle <- get.bundle( addr, max.attempts, username, password )

		if( is.null( bundle ) ) {

			cat( "download interrupted.\n" )

			break
		}

		xml2::xml_ns_strip( bundle )

		bundles[[ addr ]] <- bundle

		links <- xml2::xml_find_all( bundle, "link" )

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

	bundles
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



#' xml2df
#' @description converts a xml doc or xml node to one data frame
#'
#' @param xml xml doc or xml node
#' @param dsgn.df a design for a single data frame
#' @param sep a string to separate pasted multiple entries
#'
#' @return a data frame
#' @export
#'
#' @examples
#' \dontrun{
#' xml2df( xml, design$Patient )
#' }
xml2df <- function( xml, dsgn.df, sep = "-+-" ) {

	#xml2::xml_ns_strip( xml )
	#dbg
	#dsgn.df  <- design[[ 1 ]]
	#df.xpaths  <- dsgn.df[[ 1 ]]
	df.columns <- dsgn.df[[ 2 ]]

	#( entries <- xml2::xml_find_all( bundle, "/Specimen" ) ) #entry )
	#( xml <- xml2::xml_find_all( entries[ 1 ], df.xpath ) )

	s <- sapply(
		lst( names( df.columns ) ),
		function( column.name )  {

			#dbg
			#column.name <- names( df.columns )[ 1 ]

			i.srch <- df.columns[[ column.name ]]

			#addr   <- sub( "/@[a-zA-Z_0-9]+$", "", i.srch )
			#attrib <- sub( "^.*/@", "", i.srch )

			#val  <- xml2::xml_attr( xml2::xml_find_all( xml, addr ), attrib )
			val  <- tag.attr( xml, i.srch )

			if( is.na( val ) || length( val ) < 1 ) NA
			else if( 1 < length( val ) ) paste0( val, collapse = sep )
			else val
		}
	)

	as.data.frame( as.list( s ), stringsAsFactors = F )
}



#' bundle2dfs
#' @description converts a fhir bundle to a list of data frames.
#' design is a named list. Its names are the one of the resulting tables.
#' The elements of design are lists of 2 elements.
#' The first one is a XPath expression to locate the entry in a fhir bundle page.
#' The second one is a named list with XPath expressions of locations to the values of the items in the bundle page.
#' The names are the column names of the resultung data frames
#'
#' @param bundle a xml text file the represents a fhir bundle.
#' @param design a structure that specifies which table should contain which entries of the bundle.
#' @param sep a string to separate pasted multiple entries
#'
#' @return a list of data frames
#' @export
#'
#' @examples
#' \dontrun{
#' bundle2dfs(
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
bundle2dfs <- function( bundle, design, sep = "-+-" ) {

	xml2::xml_ns_strip( bundle )

	#dbg
	#bundle <- bundles[[ length( bundle ) ]]

	if( is.null( bundle ) ) return( NULL )

	lapply(
		lst( names( design ) ),
		function( df.name ) {

			#dbg
			#df.name <- names( design )[ 1 ]

			cat( df.name )

			dsgn.df  <- design[[ df.name ]]
			df.xpaths  <- dsgn.df[[ 1 ]]
			df.columns <- dsgn.df[[ 2 ]]

			xml.nodeset <- xml2::xml_find_all( bundle, df.xpaths )

			df <- if( 1 < length( xml.nodeset ) ) {

				dfs <- lapply(
					xml.nodeset,
					function( node ) {

						cat( "." )

						xml2df( node, dsgn.df, sep )
					}
				)

				if( 1 < length( dfs ) ) Reduce( rbind.data.frame, dfs )
				else if( 1 == length( dfs ) ) dfs[[ 1 ]]
				else NULL
			}
			else if( 1 == length( xml.nodeset ) ) as.list( xml2df( xml.nodeset[ 1 ], dsgn.df, sep ) )
			else NULL

			cat( "\n" )

			as.data.frame( df, stringsAsFactors = F )
		}
	)
}


#' result2dfs
#' @description converts all fhir bundles (the result of a fhir.search) to a list of data frames
#'
#' @param result a fhir search result as a list of xml text files.
#' @param design a structure that specifies which table should contain which entries of the bundle.
#' @param sep a string to separate pasted multiple entries
#'
#' @return a list of data frames.
#' @export
#'
#' @examples
#' \dontrun{
#' result2dfs( bundles, design )
#' }
result2dfs <- function( result, design, sep = "-+-" ) {

	bundles.dfs <- lapply(
		result,
		function( bundle ) {

			#dbl
			#bundle <- result[[ 11 ]]
			bundle2dfs( bundle, design, sep )
		}
	)

	d <- if( 1 < length( bundles.dfs ) ) {

		lapply(
			lst( names( design ) ),
			function( n ) {

				#dbg
				#n<-names( design )[ 1 ]
				dfs.n <- lapply(
					bundles.dfs,
					function( dfs ) {
						#dbg
						#dfs <- bundles.dfs[[ 1 ]]
						dfs[[ n ]]
					}
				)

				r <- if( 1 < length( dfs.n ) ) Reduce( rbind.data.frame, dfs.n )
				else if( 1 == length( dfs.n ) ) dfs.n[[ 1 ]]
				else NULL

				as.data.frame(
					r,
					fix.empty.names = T,
					stringsAsFactors = FALSE,
					row.names = 1 : nrow( r )
				)
			}
		)
	}
	else if( 1 == length( bundles.dfs ) ) bundles.dfs[[ 1 ]]
	else NULL

	cat( "\n" )

	d
}


#' coerce.types
#' @description coerce a data frame's columns
#'
#' @param df a data frame with strings as column entries
#' @param stringsAsFactors strings as factors
#'
#' @return a data frame with coerced types
#' @export
#'
#' @examples
#' \dontrun{
#' coerce.types( dfs$Besuch )
#' }
coerce.types <- function( df, stringsAsFactors = F ) {

	utils::type.convert( df, as.is = ! stringsAsFactors )
}
