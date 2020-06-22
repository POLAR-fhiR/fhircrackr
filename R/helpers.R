#' Transform vector to named list
#' @description Transforms a vector of items to a named list. The names are created with a prefix and a suffix surrounding the items.
#'
#' @param ... A vector that can be coerced to a character.
#' @param prefix A string taken as the prefix for the names of the list elements.
#' @param suffix A string taken as the suffix for the names of the list elements.
#'
#' @return A named list, where the names are the content surrounded by a prefix and a suffix.
#'
#' @examples
#' fhircrackr:::lst(letters[1:5], prefix="--", suffix="+")
lst <- function(..., prefix = NULL, suffix = NULL) {

	v <- as.list(c(...))

	names(v) <- paste0(prefix, v, suffix)

	v
}

#' Bind a list of data frames
#' @description Rowbinds a list of data frames to one data frame
#' @param list A list of data frames to bind
#' @return A single data frame
#'
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

	d
}

#' Download single FHIR bundle
#' @description Download a single FHIR bundle via FHIR search request and return it as a xml object.
#'
#' @param request A string containing the full FHIR search request.
#' @param username A string containing the username for basic authentification. Defaults to NULL, meaning no authentification.
#' @param password A string containing the passwort for basic authentification. Defaults to NULL, meaning no authentification.
#' @param max_attempts A numeric scalar. The maximal number of attempts to send a request, defaults to 5.
#' @param verbose A logical scalar. Should downloading information be printed to the console? Defaults to TRUE.
#' @param delay_between_attempts A numeric scalar specifying the delay in seconds between two attempts. Defaults to 10.
#'
#' @return The downloaded bundle in xml format.
#'
#' @examples
#' bundle<-fhircrackr:::get_bundle(request = "https://hapi.fhir.org/baseR4/Patient?")

get_bundle <- function(request, username = NULL, password = NULL, verbose = T, max_attempts = 5, delay_between_attempts = 10) {

	#dbg
	#request="https://hapi.fhir.org/baseR4/Medication?_format=xml"

	for(n in 1 : max_attempts) {

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

		Sys.sleep(delay_between_attempts)
	}

	NULL
}


#' Extract all columns
#'
#' Extracts all available values from a single resource
#'
#' @param child A xml child object, representing one FHIR resource
#' from the resouce
#' @param sep A String to separate pasted multiple entries.
#' @param xpath A String to locate data in tree via xpath.
#' @param add_indices A Logical Scalar.
#' @param indices_brackets A Vector of Strings defining the Brackets surrounding the Indices. e.g. c( "<", ">")
#'
#' @examples
#' #unserialize example bundle
#' bundles <- fhir_unserialize(medication_bundles)
#'
#' #extract child
#' child <- xml2::xml_find_first(bundles[[1]], ".//MedicationStatement")
#'
#' #Extract all columns
#' result <- fhircrackr:::xtrct_all_columns(child)
#'
xtrct_all_columns <- function(child, sep = " -+- ", xpath = ".//@*", add_indices = F, indices_brackets = c( "<", ">")) {

	tree <- xml2::xml_find_all(child, xpath)

	if( length(tree) < 1 ) return(data.frame())

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

	if (add_indices) {

		val  <- paste0(indices_brackets[1], o[ 1, ], indices_brackets[2], val)
	}

	for( col in xp.cols ) {

		#dbg
		#col <- xp.cols[1]

		d[[ col ]] <- paste0(val[ col == o[ 2, ] ], collapse = sep)
	}

	as.data.frame(d, stringsAsFactors = F)
}

#' Extract columns
#'
#' Extracts defined values from a single resource
#'
#' @param  child A xml child object, representing one FHIR resource
#' @param  df.columns The part of design from \code{\link{fhir_crack}} describing which elements to extract
#' from the resouce
#' @param sep A string to separate pasted multiple entries.
#' @param add_indices A Logical Scalar.
#' @param indices_brackets A Vector of Strings defining the Brackets surrounding the Indices. e.g. c( "<", ">")
#'
#' @examples
#' #unserialize example bundle
#' bundles <- fhir_unserialize(medication_bundles)
#'
#' #extract child
#' child <- xml2::xml_find_first(bundles[[1]], ".//MedicationStatement")
#'
#' #define columns
#' cols <-list(
#' 	SYSTEM  = "medicationCodeableConcept/coding/system/@value",
#' 	CODE    = "medicationCodeableConcept/coding/code/@value",
#' 	DISPLAY = "medicationCodeableConcept/coding/display/@value"
#' )
#'
#' #Extract columns
#' result <- fhircrackr:::xtrct_columns(child, cols)

xtrct_columns <- function( child, df.columns, sep = " -+- ", add_indices = F, indices_brackets = c( "<", ">")) {

	xp <- xml2::xml_path( child )

	l <- lapply(
		lst(names(df.columns)),
		function(column.name)  {

			#dbg
			#column.name <- names( df.columns )[ 1 ]

			i.srch <- df.columns[[column.name]]

			loc <- xml2::xml_find_all(x = child, xpath = i.srch)

			val  <- xml2::xml_text(loc)

			if( add_indices ) {

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

#				if(is.na(val) || length(val) < 1 ) {

#					NA

#				} else {

					paste0(indices_brackets[1], o, indices_brackets[2], val, collapse = sep)
#				}
			}
			else {

#				if(is.na(val) || length(val) < 1) {

#					NA

#				} else {

					paste0(val, collapse = sep)
#				}
			}
		}
	)

	as.data.frame(l, stringsAsFactors = F)
}

#' Extracts one data frame out of one bundle
#' @param bundle A xml object containing one FHIR bundle
#' @param design.df On element of the design from \code{\link{fhir_crack}}, i.e. a list of length 1
#' or 2, where the first element is a XPath expression to the ressource and the (optional)
#' second element is either a XPath expression or a named list containing column names and XPath expressions
#' @param sep A string to separate pasted multiple entries.
#' @param add_indices A Logical Scalar.
#' @param indices_brackets A Vector of Strings defining the Brackets surrounding the Indices. e.g. c( "<", ">")
#'
#' @examples
#' #unserialize example bundle
#' bundles <- fhir_unserialize(medication_bundles)
#'
#' #extract first bundle
#' bundle <- bundles[[1]]
#'
#' #define design
#' design <- list(
#'      ".//MedicationStatement",
#'      list(
#' 	      SYSTEM  = "medicationCodeableConcept/coding/system/@value",
#' 	      CODE    = "medicationCodeableConcept/coding/code/@value",
#' 	      DISPLAY = "medicationCodeableConcept/coding/display/@value"
#' 	      )
#' 	 )
#'
#'
#' #convert bundle to data frame
#' result <- fhircrackr:::bundle2df(bundle, design)
bundle2df <- function(bundle, design.df, sep = " -+- ", add_indices = F, indices_brackets = c( "<", ">")) {

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

			if (1<length(design.df) && is.list(design.df[[2]])) {

				df.columns <- design.df[[2]]

				res <- xtrct_columns( child, df.columns, sep = sep, add_indices = add_indices, indices_brackets = indices_brackets)

				if( all(sapply(res, is.na))) cat( "x" ) else cat( "." )
			}
			else{

				xp <- if(1<length(design.df)) design.df[[2]] else ".//@*"

				res <- xtrct_all_columns(child = child, sep = sep, xpath = xp, add_indices = add_indices, indices_brackets = indices_brackets)

				if( nrow(res) < 1 ) cat( "x" ) else cat( "." )
			}

			res
		}
	)

	rbind_list_of_data_frames(list = df.list)
}

#'Convert several bundles to one data frame
#'
#' @param bundles A list of xml objects containing FHIR bundles
#' @param design.df On element of the design from \code{\link{fhir_crack}}, i.e. a list of length 1
#' or 2, where the first element is a XPath expression to the ressource and the (optional)
#' second element is either a XPath expression or a named list containing column names and XPath expressions
#' @param sep A string to separate pasted multiple entries.
#' @param add_indices A Logical Scalar.
#' @param indices_brackets A Vector of Strings defining the Brackets surrounding the Indices. e.g. c( "<", ">")
#'
#' @examples
#' #unserialize example bundle
#' bundles <- fhir_unserialize(medication_bundles)
#'
#' #define design
#' design <- list(
#'      ".//MedicationStatement",
#'      list(
#' 	      SYSTEM  = "medicationCodeableConcept/coding/system/@value",
#' 	      CODE    = "medicationCodeableConcept/coding/code/@value",
#' 	      DISPLAY = "medicationCodeableConcept/coding/display/@value"
#' 	      )
#' 	 )
#'
#'
#' #convert bundles to data frame
#' result <- fhircrackr:::bundles2df(bundles, design)

bundles2df <- function(bundles, design.df, sep = " -+- ", add_indices = F, indices_brackets = c( "<", ">")) {

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

				bundle2df( bundle, design.df, sep, add_indices = add_indices, indices_brackets = indices_brackets)
			}
		)
	)

	ret <- ret[ apply(ret, 1, function(row) ! all(is.na(row))), , drop = F]

	cat( "\n" )

	ret
}

#' Flatten list of FHIR bundles
#' @description Converts all FHIR bundles (the result of \code{\link{fhir_search}}) to a list of data frames.
#'
#' @param bundles A FHIR search result as returned by \code{\link{fhir_search}}.
#' @param design A named list specifiying which data frame should contain which entries of the bundle.
#' The names correspond to the names of the resulting data frames.
#'
#' Each element of design is a list of length 1 or 2, where the first element is a XPath expression to locate the entry in a
#' FHIR bundle page. There are 3 options for the second element of that list:
#'
#' - There is no second element: all attributes of the recource are extracted
#' - The second element is string containing a XPath expression to all the values that should be extracted. "./@value" e.g. would extract all
#'   values on the root level.
#' - The second element is a named list where the elements are XPath expressions indicating the specific position of values to extract, where the names of the
#' list elements are the column names of the resulting data frame.
#'
#' For a more detailed explanation see the package vignette.
#'
#' @param sep A string to separate pasted multiple entries.
#' @param remove_empty_columns Logical scalar. Remove empty columns?
#' @param add_indices A Logical Scalar.
#' @param indices_brackets A Vector of Strings defining the Brackets surrounding the Indices. e.g. c( "<", ">")
#' @return A list of data frames as specified by \code{design}.
#' @export
#'
#' @examples
#' #unserialize example bundle
#' bundles <- fhir_unserialize(medication_bundles)
#'
#' #define attributes to extract
#' df_design <- list(
#'
#'  #define specifically which elements to extract
#' 	MedicationStatement = list(
#'
#' 		".//MedicationStatement",
#'
#' 		list(
#' 			AID                = "id/@value",
#' 			STATUS.TEXT        ="text/status/@value",
#' 			STATUS             = "status/@value",
#' 			MEDICATION.SYSTEM  = "medicationCodeableConcept/coding/system/@value",
#' 			MEDICATION.CODE    = "medicationCodeableConcept/coding/code/@value",
#' 			MEDICATION.DISPLAY = "medicationCodeableConcept/coding/display/@value",
#' 			DOSAGE             = "dosage/text/@value",
#' 			PATIENT            = "subject/reference/@value",
#' 			LAST.UPDATE        = "meta/lastUpdated/@value"
#' 		)
#' 	),
#'
#'  #extract all values
#' 	Patients = list(
#'
#' 		".//Patient"
#' 	)
#' )
#'
#' #convert fhir to data frames
#' list_of_tables <- fhircrackr:::bundles2dfs(bundles, df_design)

bundles2dfs <- function(bundles, design, sep = " -+- ", remove_empty_columns = F, add_indices = F, indices_brackets = c( "<", ">")) {

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

	if (add_indices) {

		if (is.null(indices_brackets)) indices_brackets <- c("<", ">")

		if (length(indices_brackets) < 2) indices_brackets[2] <- indices_brackets[1]
	}

	dfs <- lapply(
		lst(names(design)),
		function(n) {

			#dbg
			#n <- names(design)[1]

			design.df <- design[[n]]

			cat("\n", n)

			bundles2df(bundles = bundles, design.df = design.df, sep = sep, add_indices = add_indices, indices_brackets = indices_brackets)
		}
	)

	cat("\n")

	if(remove_empty_columns) {

		dfs <- lapply(
			dfs,
			function( df ) {

				df[ , sapply( df, function( col ) 0 < sum( ! is.na( col ) ) ), drop = F ]
			}
		)
	}

	dfs
}
