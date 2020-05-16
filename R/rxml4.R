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
#'@description transforms a vector of items to a named list. The names are created the a prefix and a suffix surrounding the items.
#'
#' @param ... a scalar or a vector of scalars.
#' @param prefix a prefix in front of the result text.
#' @param suffix a suffix after the result text.
#'
#' @return a named list, where the names are the content surrounded bei prefix and suffix
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


#' download.page
#' @description downloads a fhir bundle page via fhir search request and return it as xml file.
#'
#' @param fhir.search.request a fhir search request. it must contain _format=xml.
#' @param max.attempts the maximal number of attempts to send a request. Default is 5.
#'
#' @return the downloaded bundle page in xml format.
#' @export
#'
#' @examples
#' download.page( "https://hapi.fhir.org/baseR4/Medication?_format=xml" )
download.page <- function( fhir.search.request, max.attempts = 5 ) {

	for( n in 1 : max.attempts ) {

		cat( paste0( th( n ), " attempt to download: ", fhir.search.request, "\n" ) )

		r <- try(

			xml2::read_xml( fhir.search.request, silent = T )
		)

		if( class( r )[ 1 ] != "try-error" ) {

			xml2::xml_ns_strip( r )

			return( r )
		}

		Sys.sleep( 10 )
	}

	NULL
}


#' download.bundle
#' @description downloads a fhir bunde via fhir search request from a fhir server.
#'
#' @param fhir.search.request a fhir search request
#' @param max.attempts maximal attempts to connect to a page address
#'
#' @return the downloaded bundle as a list of pages in xml format
#' @export
#'
#' @examples
#' download.bundle( "https://hapi.fhir.org/baseR4/Medication?_format=xml" )
download.bundle <- function( fhir.search.request, max.attempts = 5 ) {

	xmls <- list( )

	addr <- fhir.search.request

	repeat {

		xml <- download.page( addr, max.attempts )

		if( is.null( xml ) ) {

			cat( "download interrupted.\n" )

			break
		}

		xmls[[ addr ]] <- xml

		links <- xml2::xml_find_all( xml, "/Bundle/link" )

		rels.nxt  <- xml2::xml_attr( xml2::xml_find_first( links, "./relation" ), "value" ) == "next"

		if( ! any( ! is.na( rels.nxt ) & rels.nxt ) ) {

			cat( "\ndownload completed\n" )

			break
		}

		urls  <- xml2::xml_attr( xml2::xml_find_first( links, "./url" ), "value" )

		addr <- urls[ rels.nxt ][ 1 ]

		if( addr == "" ) {

			cat( "\ndownload completed\n" )

			break
		}
	}

	xmls
}


#' write.bundle
#' @description writes a fhir bundle as numbered xml files into a directory.
#'
#' @param bundle a list of xml text files representing the pages of a fhir bundle.
#' @param directory the location to store the data.
#'
#' @return nothing to return.
#' @export
#'
#' @examples
#' write.bundle( my.fhir.xmls, "result" )
write.bundle <- function( bundle, directory ) {
#
	w <- 1 + floor( log10( length( bundle ) ) )

	if( ! dir.exists( directory ) )

		dir.create( directory, recursive = T )

	for( n in 1 : length( bundle ) )

		xml2::write_xml( bundle[[ n ]], concatenate.paths( bundle, paste0( stringr::str_pad( n, width = w, pad = "0" ), ".xml" ) ) )
}


#' read.bundle
#' @description reads a bundle stored as xml files from a directory
#'
#' @param directory the location to store the data.
#'
#' @return the bundle as a list of xml text files.
#' @export
#'
#' @examples
#' read.bundle( "result" )
read.bundle <- function( directory ) {

	xml.files <- dir( directory, "*.xml" )

	lapply( lst( xml.files ), function( x ) xml2::read_xml( concatenate.paths( directory, x ) ) )
}


#' page.to.dataframes
#' @description converts a fhir bundle page to a list of data frames.
#' design is a named list. Its names are the one of the resulting tables.
#' The elements of design are lists of 2 elements.
#' The first one is a XPath expression to locate the entry in a fhir bundle page.
#' The second one is a named list with XPath expressions of locations to the values of the items in the bundle page.
#' The names are the column names of the resultung data frames
#'
#' @param page a xml text file the represents a fhir bundle page.
#' @param design a structure that specifies which table should contain which entries of the bundle.
#'
#' @return a list of data frames
#' @export
#'
#' @examples
#' page.to.dataframes(
#' page,
#' design = list(
#' MEDICATION = list(
#'  tag     = ".//Medication",
#'  items   = list(
#' 	  SYSTEM  = "code/coding/system/@value",
#' 	  CODE    = "code/coding/code/@value",
#' 	  DISPLAY = "code/coding/display/@value"
#' 	) ) ) )
page.to.dataframes <- function( page, design ) {

	if( is.null( page ) ) return( NULL )

	lapply(
		lst( names( design ) ),
		function( n.e ) {

			#dbg
			#n.e <- names( design )[ 1 ]

			cat( "\n", n.e )

			e <- design[[ n.e ]]

			entry <- e[[ 1 ]]
			items <- e[[ 2 ]]

			page.entry <- xml2::xml_find_all( page, entry )

			Reduce(
				rbind,
				lapply(
					page.entry,
					function( tg ) {

						#dbg
						#tg <- page.entry[[ 1 ]]

						cat( "." )

						sapply(
							names( items ),
							function( i.n )  {

								#dbg
								#i.n <- names( n$items )[[ 2 ]]

								i.srch <- items[[ i.n ]]

								addr <- sub( "/@[a-zA-Z0-9]+$", "", i.srch )
								item <- sub( "^.*/@", "", i.srch )
								val  <- xml2::xml_attr( xml2::xml_find_all( tg, addr ), item )

								if( is.character( val ) ) {

									if( length( val ) < 1 ) val <- NA

									else if( 1 < length( val ) ) val <- paste0( val, collapse = " # " )
								}

								val
							}
						)
					}
				)
			)
		}
	)
}


#' bundle.to.dataframes
#' @description converts a fhir bundle to a list of data frames
#'
#' @param bundle a list of xml text files representing the pages of a fhir bundle.
#' @param design a structure that specifies which table should contain which entries of the bundle.
#'
#' @return
#' @export
#'
#' @examples
#' bundle.to.dataframes( bundle, design )
bundle.to.dataframes <- function( bundle, design ) {

	d <- lapply(
		lst( names( design ) ),
		function( n ) {

			cat( n, "\n" )

			as.data.frame(
				Reduce(
					rbind,
					lapply(
						bundle,
						function( x ) {

							page.to.dataframes( x, design )[[ n ]]
						}
					)
				)
			)
		}
	)

	cat( "\n" )

	d
}