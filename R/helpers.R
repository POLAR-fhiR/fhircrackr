#' Transform vector to named list
#' @description Transforms a vector of items to a named list. The names are created with a prefix and a suffix surrounding the items.
#'
#' @param ... A vector that can be coerced to a character.
#' @param prefix A string taken as the prefix for the names of the list elements.
#' @param suffix A string taken as the suffix for the names of the list elements.
#'
#' @return A named list, where the names are the content surrounded by a prefix and a suffix.
#'
#' @noRd
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
#' @noRd
rbind_list_of_data_frames <- function( list ) {

	#dbg
	#list <- l

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
#' @param username A string containing the username for basic authentication. Defaults to NULL, meaning no authentification.
#' @param password A string containing the password for basic authentication. Defaults to NULL, meaning no authentification.
#' @param max_attempts A numeric scalar. The maximal number of attempts to send a request, defaults to 5.
#' @param verbose An integer scalar. If > 1,  Downloading progress is printed. Defaults to 2.
#' @param delay_between_attempts A numeric scalar specifying the delay in seconds between two attempts. Defaults to 10.
#'
#' @return The downloaded bundle in xml format.
#' @noRd
#'
#' @examples
#' bundle<-fhircrackr:::get_bundle(request = "https://hapi.fhir.org/baseR4/Patient?")

get_bundle <- function(request, username = NULL, password = NULL, verbose = 2, max_attempts = 5, delay_between_attempts = 10) {

	#dbg
	#request="https://hapi.fhir.org/baseR4/Medication?_format=xml"

	for(n in 1 : max_attempts) {

		#dbg
		#n <- 1

		if(1 < verbose) cat(paste0("(", n, "): ", request, "\n"))

		auth <- if (!is.null(username) & !is.null(password)){

			httr::authenticate(username, password)
		}

		response <- httr::GET(
			request,
			httr::add_headers(Accept = "application/fhir+xml"),
			httr::content_type("application/fhir+xml;charset=utf-8"),
			auth
		)

		check_http_code(response$status_code)


		payload <- try(httr::content(response, as = "text", encoding = "UTF-8"), silent = T)

		if (class(payload)[1] != "try-error") {

			xml <- try(xml2::read_xml(payload), silent = T)

			if(class(xml)[1] != "try-error") {

				return(xml)
			}
		}


		Sys.sleep(delay_between_attempts)
	}

	NULL
}


#' Check http status code
#'
#' Checks the status code and issues an error or warning if necessary
#'
#' @param code A http status code
#' @example check_http_code(404)
#' @noRd
#'
#'
check_http_code <- function(code){

	if (code == 400) {

		stop("HTTP code 400 - Please check if your request is a valid FHIR search request.")

	}

	if (code == 401) {

		stop("HTTP code 401 - Authentification needed.")
	}

	if (code == 404) {

		stop("HTTP code 404 - Not found. Did you misspell the resource?")
	}

	if (code >= 300 & code < 400) {

		warning(paste("Your request generated a HTTP code", code))
	}

	if (code >=400 & code < 500) {

		stop(paste("Your request generated a client error, HTTP code", code))

	}

	if (code >=500 & code < 600) {

		stop(paste("Your request generated a server error, HTTP code", code))

	}

}



#' Check design
#' @description Checks whether a design provided to \code{\link{fhir_crack}} is invalid and
#' issues a warning if it is.
#' @param design The design to be checked
#' @return TRUE if design is invalid, FALSE if design is valid
#' @noRd
is_invalid_design <- function(design){

	if (is.null(design)) {

		warning("Argument design is NULL, returning NULL.")
		return(T)
	}

	if (!is.list(design)) {

		warning("Argument design has to be a list, returnign NULL.")
		return(T)
	}

	if (length(design)<1) {

		warning("Argument design has length 0, returning NULL.")
		return(T)
	}

	list.type <- sapply(design, is.list)

	if (any(!list.type)) {

		warning("All elements of design have to be of type list. Returning NULL.")
		return(T)
	}

	if (is.null(names(design)) || any(names(design)=="")) {

		warning("Argument design should be a NAMED list but has at least one unnamed element. Returning NULL")
		return(T)
	}

	lengths <- sapply(design, length)

	if (any(lengths < 1 | 2 < lengths)){

		warning("At least one if the elements of argument design is not a list of length 1 or 2. Returning NUll")
		return(T)
	}

	F
}
#' Check List of Bundles
#' @description Checks whether a List of Bundles provided to \code{\link{fhir_crack}} is ivalid and
#' issues a warning if it is.
#' @param bundles_list The List of Bundles to be checked
#' @return TRUE if bundles_list is invalid, FALSE if not
#' @noRd
is_invalid_bundles_list <- function(bundles_list){

	if (is.null(bundles_list)) {

		warning("Argument bundles_list is NULL, returning NULL.")

		return(T)
	}

	if (!is.list(bundles_list)) {

		warning("Argument bundles_list has to be a list, returnign NULL.")
		return(T)
	}

	if (length(bundles_list)<1) {

		warning("Argument bundles_list has length 0, returning NULL.")
		return(T)
	}

	valid.doc.types <- all(
		sapply(
			bundles_list,
			function(b) {

				if(is.null(b)) {

					F
				}
				else {

					cl <- class(b)
					length(cl) == 2 || cl[1] == "xml_document" || cl[2] == "xml_node"
				}
			}
		)
	)

	if (!valid.doc.types) {

		warning("Argument bundles_list contains at least one invalid Bundle. Bundles have to be of Class 'xml_document' and 'xml_node'. Returning NULL")
		return(T)
	}

	F
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
#' @param brackets A Vector of Strings defining the Brackets surrounding the Indices. e.g. c( "<", ">")
#' @noRd
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
xtrct_all_columns <- function(child, sep = " -+- ", xpath = ".//@*", add_indices = F, brackets = c( "<", ">")) {

	tree <- xml2::xml_find_all(child, xpath)

	if(length(tree) < 1) {return(data.frame())}

	xp.child  <- xml2::xml_path(child)
	xp.remain <- xml2::xml_path(tree)
	xp.rel    <- substr(xp.remain, nchar(xp.child) + 2, nchar(xp.remain))
	xp.cols   <- gsub("/", ".", gsub("@", "", unique(gsub("\\[[0-9]+\\]", "", xp.rel))))

	d <- lapply(1:length(xp.cols),function(dummy)character(0))

	names(d) <- xp.cols

	val  <- xml2::xml_text(tree)

	s <- stringr::str_split(xp.rel, "/")

	o <- sapply(
		seq_along(s),
		function(i) {

			#dbg
			#i<-1

			s. <- s[[i]]

			i.f <- !grepl("\\[[0-9]+\\]", s.)

			if(any(i.f)) {

				s.[i.f] <- paste0(s.[i.f], "[1]")
			}

			c(
				gsub(".1$", "", paste0(gsub( "[^0-9]", "", s. ), collapse = "." )),
				gsub( "@", "", gsub( "\\[[0-9]+\\]", "", paste0(s., collapse = "." )))
			)
		}
	)

	if (add_indices) {

		val  <- paste0(brackets[1], o[ 1, ], brackets[2], val)
	}

	for(col in xp.cols) {

		#dbg
		#col <- xp.cols[1]

		d[[col]] <- paste0(val[col == o[2, ]], collapse = sep)
	}

	as.data.frame(d, stringsAsFactors = F)
}

#' Extract columns
#'
#' Extracts defined values from a single resource
#'
#' @param child A xml child object, representing one FHIR resource
#' @param df.columns The part of design from \code{\link{fhir_crack}} describing which elements to extract
#' from the resouce
#' @param sep A string to separate pasted multiple entries.
#' @param add_indices A Logical Scalar.
#' @param brackets A Vector of Strings defining the Brackets surrounding the Indices. e.g. c( "<", ">")
#' @noRd
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

xtrct_columns <- function(child, df.columns, sep = " -+- ", add_indices = F, brackets = c( "<", ">")) {

	xp <- xml2::xml_path(child)

	l <- lapply(
		lst(names(df.columns)),
		function(column.name)  {

			#dbg
			#column.name <- names( df.columns )[ 1 ]

			i.srch <- df.columns[[column.name]]

			loc <- xml2::xml_find_all(x = child, xpath = i.srch)

			val <- xml2::xml_text(loc)

			if(add_indices) {

				loc.xp <- xml2::xml_path(loc)

				loc.xp.rel <- substr(loc.xp, nchar(xp) + 2, nchar(loc.xp))

				s <- stringr::str_split(loc.xp.rel, "/")

				o <- sapply(
					seq_along(s),
					function(i) {

						#dbg
						#i<-1

						s. <- s[[i]]

						i.f <- !grepl("\\[[0-9]+\\]", s.)

						if(any(i.f)) {

							s.[i.f] <- paste0(s.[i.f], "[1]")
						}

						gsub(".1$", "", paste0(gsub("[^0-9]", "", s.), collapse = "."))
					}
				)

				paste0(brackets[1], o, brackets[2], val, collapse = sep)
			}
			else {

				paste0(val, collapse = sep)
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
#' @param brackets A Vector of Strings defining the Brackets surrounding the Indices. e.g. c( "<", ">")
#' @param verbose An Integer Scalar.  If > 1, extraction progress will be printed. Defaults to 2.
#' @noRd
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
#' #convert bundle to data frame
#' result <- fhircrackr:::bundle2df(bundle, design)
bundle2df <- function(bundle, design.df, sep = " -+- ", add_indices = F, brackets = c( "<", ">"), verbose = 2) {

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

				res <- xtrct_columns( child, df.columns, sep = sep, add_indices = add_indices, brackets = brackets)

				if(1 < verbose){

					if(all(sapply(res, is.na))) {cat("x")} else {cat( ".")}
				}
			}
			else{

				xp <- if(1<length(design.df)) {design.df[[2]]} else {".//@*"}

				res <- xtrct_all_columns(child = child, sep = sep, xpath = xp, add_indices = add_indices, brackets = brackets)

				if(1 < verbose){

					if(nrow(res) < 1) {cat("x")} else {cat(".")}
				}
			}

			res
		}
	)

	rbind_list_of_data_frames(list = df.list)
}

#' Convert several bundles to one data frame
#'
#' @param bundles A list of xml objects containing FHIR bundles
#' @param design.df On element of the design from \code{\link{fhir_crack}}, i.e. a list of length 1
#' or 2, where the first element is a XPath expression to the ressource and the (optional)
#' second element is either a XPath expression or a named list containing column names and XPath expressions
#' @param sep A string to separate pasted multiple entries.
#' @param add_indices A Logical Scalar.
#' @param brackets A Vector of Strings defining the Brackets surrounding the Indices. e.g. c( "<", ">")
#' @param verbose An Integer Scalar.  If > 1, extraction progress will be printed. Defaults to 2.
#' @noRd
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

bundles2df <- function(bundles, design.df, sep = " -+- ", add_indices = F, brackets = c( "<", ">"), verbose = 2) {

	ret <- rbind_list_of_data_frames(
		lapply(
			seq_len(length(bundles)),
			function( i ) {

				#dbg
				#i<-1

				if (1 < verbose) {cat( "\n", i )}

				bundle <- bundles[[ i ]]

				bundle2df( bundle, design.df, sep, add_indices = add_indices, brackets = brackets, verbose = verbose)
			}
		)
	)

	ret <- ret[ apply(ret, 1, function(row) ! all(is.na(row))), , drop = F]

	if (1 < verbose) {cat( "\n" )}

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
#' @param brackets A Vector of Strings defining the Brackets surrounding the Indices. e.g. c( "<", ">")
#' @param verbose An Integer Scalar.  If > 1, extraction progress will be printed. Defaults to 2.
#' @noRd
#' @return A list of data frames as specified by \code{design}.
#'
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

bundles2dfs <- function(bundles, design, sep = " -+- ", remove_empty_columns = F, add_indices = F, brackets = c( "<", ">"), verbose = 2) {

	if (add_indices) {

		if (is.null(brackets)) brackets <- c("<", ">")

		if (length(brackets) < 2) brackets[2] <- brackets[1]
	}

	dfs <- lapply(
		lst(names(design)),
		function(n) {

			#dbg
			#n <- names(design)[1]

			design.df <- design[[n]]

			if (1 < verbose) {cat("\n", n)}

			bundles2df(bundles = bundles, design.df = design.df, sep = sep, add_indices = add_indices, brackets = brackets, verbose = verbose)
		}
	)

	if (1 < verbose) {cat("\n")}

	if (remove_empty_columns) {

		dfs <- lapply(
			dfs,
			function( df ) {

				df[ , sapply( df, function( col ) 0 < sum( ! is.na( col ) ) ), drop = F ]
			}
		)
	}

	dfs
}


# escape if neccessary
esc <- function( s ) gsub( "([\\.|\\^|\\$|\\*|\\+|\\?|\\(|\\)|\\[|\\{|\\\\\\|\\|])", "\\\\\\1", s )

# row to data frame
detree_row <- function( row=a[3,], column.prefix = "id", brackets = c( "<", ">" ), sep = " -+- " ) {

	pattern.col <- paste0( "^", column.prefix, "\\." )

	col.names.mutable  <- names( row )[ grep( pattern.col, names( row ) ) ]

	col.names.constant <- setdiff( names( row ), col.names.mutable )

	row.mutable  <- row[ col.names.mutable ]

	row.constant <- row[ col.names.constant ]

	#dbg
	#row <- d3.3$Entries[ 1, ]

	brackets.escaped <- esc( brackets )

	pattern.ids <- paste0( brackets.escaped[1], "([0-9]+\\.*)+", brackets.escaped[2] )

	ids <- stringr::str_extract_all( row.mutable, pattern.ids)

	names( ids ) <- col.names.mutable

	pattern.items <- paste0( brackets.escaped[1], "([0-9]+\\.*)+", brackets.escaped[2] )

	items <- stringr::str_split( row.mutable, pattern.items)

	items <- lapply( items, function( i ) if( ! is.na( i ) && i[1] == "" ) i[ 2 : length( i ) ] else i )

	names( items ) <- col.names.mutable

	d <- row[ 0, , F ]

	for( i in names( ids ) ) {

		#dbg
		#i<-names( ids )[1]

		id <- ids[[ i ]]

		if( ! all( is.na( id ) ) ) {

			it <- items[[ i ]]

			new.rows        <- gsub( paste0( brackets.escaped[1], "([0-9]+)\\.*.*" ), "\\1", id )
			new.ids         <- gsub( paste0( "(", brackets.escaped[1], ")([0-9]+)\\.*(.*", brackets.escaped[2], ")" ), "\\1\\3", id )
			unique.new.rows <- unique( new.rows )

			set <- paste0( new.ids, it )

			f <- sapply(
				unique.new.rows,
				function( unr ) {

					#dbg
					#unr <- unique.new.rows[1]

					fltr <- unr == new.rows

					paste0( set[ fltr ], collapse = "" )
				}
			)

			for( n in unique.new.rows ) d[ n, i ] <- gsub( paste0( esc( sep ), "$" ), "", f[ n ], perl = T )
		}
	}

	if( 0 < length( col.names.constant ) ) d[ , col.names.constant ] <- row[ col.names.constant ]

	names( d )[ names( d ) %in% col.names.mutable ] <- gsub( paste0( "^", column.prefix, "\\." ), "", col.names.mutable )

	d
}

