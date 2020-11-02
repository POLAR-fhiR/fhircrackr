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
lst <- function(...,
				prefix = NULL,
				suffix = NULL) {
	v <- as.list(c(...))

	names(v) <- paste0(prefix, v, suffix)

	v
}


#' Download single FHIR bundle
#' @description Download a single FHIR bundle via FHIR search request and return it as a xml object.
#'
#' @param request A string containing the full FHIR search request.
#' @param username A string containing the username for basic authentication. Defaults to NULL, meaning no authentication.
#' @param password A string containing the password for basic authentication. Defaults to NULL, meaning no authentication.
#' @param max_attempts A numeric scalar. The maximal number of attempts to send a request, defaults to 5.
#' @param verbose An integer scalar. If > 1,  Downloading progress is printed. Defaults to 2.
#' @param delay_between_attempts A numeric scalar specifying the delay in seconds between two attempts. Defaults to 10.
#' @param log_errors A numeric scalar. 0 means no error logging, 1: crack with fhir_crack and log to csv, 2 log xml directly
#'
#' @return The downloaded bundle in xml format.
#' @noRd
#'
#' @examples
#' bundle<-fhircrackr:::get_bundle(request = "https://hapi.fhir.org/baseR4/Patient?")

get_bundle <-
	function(request,
			 username = NULL,
			 password = NULL,
			 verbose = 2,
			 max_attempts = 5,
			 delay_between_attempts = 10,
			 log_errors = 0) {
		#dbg
		#request="https://hapi.fhir.org/baseR4/Medication?_format=xml"

		for (n in 1:max_attempts) {
			#dbg
			#n <- 1

			if (1 < verbose)
				cat(paste0("(", n, "): ", request, "\n"))

			auth <- if (!is.null(username) && !is.null(password)) {
				httr::authenticate(username, password)
			}

			response <- httr::GET(
				request,
				httr::add_headers(Accept = "application/fhir+xml"),
				auth
			)

			check_response(response, log_errors = log_errors)

			payload <-
				try(httr::content(response, as = "text", encoding = "UTF-8"),
					silent = TRUE)

			if (class(payload)[1] != "try-error") {
				xml <- try(xml2::read_xml(payload), silent = TRUE)

				if (class(xml)[1] != "try-error") {
					return(xml)
				}
			}

			Sys.sleep(delay_between_attempts)
		}

		NULL
	}

#'log the error message of a http response
#'
#' @param response A http response
#' @param log_errors A numeric scalar. 0 means no error logging, 1: crack with fhir_crack and log to csv, 2 log xml directly
#' @noRd
#'
#'
error_to_file <- function(response, log_errors) {
	payload <- httr::content(response, as = "text", encoding = "UTF-8")

	xml <- xml2::read_xml(payload)

	time <- gsub(" |-", "_", Sys.time())
	time <- gsub(":", "", time)

	if (log_errors == 1) {
		message <-
			fhir_crack(list(xml), list(error = list("//*")), verbose = 0)[[1]]
		utils::write.csv(message, paste0("error_message_", time, ".csv"))
	}

	if (log_errors == 2) {
		xml2::write_xml(xml, paste0("error_", time, ".xml"))
	}



}
#' Check http response
#'
#' Checks the http response and issues an error or warning if necessary
#'
#' @param response A http response
#' @param log_errors A numeric scalar. 0 means no error logging, 1: crack with fhir_crack and log to csv, 2 log xml directly
#' @noRd
#'
#'
check_response <- function(response, log_errors) {
	code <- response$status_code

	if (code != 200 && log_errors > 0) {
		error_to_file(response, log_errors)
	}

	if (code == 400) {
		if (log_errors > 0) {
			stop(
				"HTTP code 400 - This can be caused by an invalid FHIR search request or a server issue. For more information see the error file that has been generated in the working directory."
			)

		} else{
			stop(
				"HTTP code 400 - This can be caused by an invalid FHIR search request or a server issue. To print more detailed error information to a file, set argument log_errors to 1 or 2 and rerun fhir_search()."
			)

		}

	}

	if (code == 401) {
		if (log_errors > 0) {
			stop(
				"HTTP code 401 - Authentication needed. For more information see the error file that has been generated in the working directory."
			)

		} else{
			stop(
				"HTTP code 401 - Authentication needed. To print more detailed error information to a file, set argument log_errors to 1 or 2 and rerun fhir_search()."
			)

		}

	}

	if (code == 404) {
		if (log_errors > 0) {
			stop(
				"HTTP code 404 - Not found. Did you misspell the resource? For more information see the error file that has been generated in the working directory."
			)

		} else{
			stop(
				"HTTP code 404 - Not found. Did you misspell the resource? To print more detailed error information to a file, set argument log_errors to 1 or 2 and rerun fhir_search()."
			)

		}

	}

	if (code >= 300 && code < 400) {
		if (log_errors > 0) {
			warning(
				"Your request generated a HTTP code ",
				code,
				". For more information see the error file that has been generated in the working directory."
			)

		} else{
			warning(
				"Your request generated a HTTP code ",
				code,
				". To print more detailed information to a file, set argument log_errors to 1 or 2 and rerun fhir_search()."
			)

		}

	}

	if (code >= 400 & code < 500) {
		if (log_errors > 0) {
			stop(
				"Your request generated a client error, HTTP code ",
				code,
				". For more information see the error file that has been generated in the working directory."
			)

		} else{
			stop(
				"Your request generated a client error, HTTP code ",
				code,
				". To print more detailed information to a file, set argument log_errors to 1 or 2 and rerun fhir_search()."
			)

		}

	}

	if (code >= 500 && code < 600) {
		if (log_errors > 0) {
			stop(
				"Your request generated a server error, HTTP code ",
				code,
				". For more information see the error file that has been generated in the working directory."
			)

		} else{
			stop(
				"Your request generated a server error, HTTP code ",
				code,
				". To print more detailed error information to a file, set argument log_errors to 1 or 2 and rerun fhir_search()."
			)

		}

	}

}


#' Check List of Bundles
#' @description Checks whether a List of Bundles provided to \code{\link{fhir_crack}} is invalid and
#' issues a warning if it is.
#' @param bundles_list The List of Bundles to be checked
#' @return TRUE if bundles_list is invalid, FALSE if not
#' @noRd
is_invalid_bundles_list <- function(bundles_list) {
	if (is.null(bundles_list)) {
		warning("Argument bundles is NULL, returning NULL.")

		return(TRUE)
	}

	if (!is.list(bundles_list)) {
		warning("Argument bundles has to be a list, returnin NULL.")
		return(TRUE)
	}

	if (length(bundles_list) < 1) {
		warning("Argument bundles has length 0, returning NULL.")
		return(TRUE)
	}

	if (any(sapply(bundles_list, is.raw))) {
		warning(
			"Argument bundles seems to contain serialized bundles. Use fhir_unserialize() before proceeding. Returning NULL"
		)
		return(TRUE)
	}

	valid.doc.types <- all(sapply(bundles_list,
								  function(b) {
								  	if (is.null(b)) {
								  		FALSE
								  	}
								  	else {
								  		cl <- class(b)
								  		length(cl) == 2 ||
								  			cl[1] == "xml_document" || cl[2] == "xml_node"
								  	}
								  }))

	if (!valid.doc.types) {
		warning(
			"Argument bundles contains at least one invalid Bundle. Bundles have to be of Class 'xml_document' and 'xml_node'. Returning NULL"
		)
		return(TRUE)
	}

	FALSE
}


#' Extract all columns
#'
#' Extracts all available values from a single resource
#'
#' @param child A xml child object, representing one FHIR resource
#' from the resource
#' @param sep A String to separate pasted multiple entries.
#' @param xpath A String to locate data in tree via xpath.
#' @param brackets A Vector of Strings defining the Brackets surrounding the Indices. e.g. c( "<", ">") NULL means no brackets.
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
xtrct_all_columns <-
	function(child,
			 sep = NULL,
			 xpath = ".//@*",
			 brackets = NULL) {

		tree <- xml2::xml_find_all(child, xpath)

		if (length(tree) < 1) {
			return(data.table::data.table())
		}

		xp.child  <- xml2::xml_path(child)
		xp.remain <- xml2::xml_path(tree)
		xp.rel    <-
			substr(xp.remain, nchar(xp.child) + 2, nchar(xp.remain))
		xp.cols   <-
			gsub("/", ".", gsub("@", "", unique(gsub(
				"\\[[0-9]+\\]", "", xp.rel
			))))

		d <- lapply(1:length(xp.cols), function(dummy)
			character(0))

		names(d) <- xp.cols

		val  <- xml2::xml_text(tree)

		s <- stringr::str_split(xp.rel, "/")

		o <- sapply(seq_along(s),
					function(i) {
						#dbg
						#i<-1

						s. <- s[[i]]

						i.f <- !grepl("\\[[0-9]+\\]", s.)

						if (any(i.f)) {
							s.[i.f] <- paste0(s.[i.f], "[1]")
						}

						c(gsub(".1$", "", paste0(gsub(
							"[^0-9]", "", s.
						), collapse = ".")),
						gsub("@", "", gsub(
							"\\[[0-9]+\\]", "", paste0(s., collapse = ".")
						)))
					})

		if (!is.null(brackets)) {

			is_av_val <- ! is.na(val)

			o. <- o[1, ]

			val[is_av_val] <- paste0(brackets[1], o.[is_av_val], brackets[2], val[is_av_val])
		}

		for (col in xp.cols) {
			#dbg
			#col <- xp.cols[1]

			d[[col]] <- paste0(val[col == o[2, ]], collapse = sep)
		}

		result <- data.table::as.data.table(d)
		names(result) <- gsub("(\\.\\w+)$", "", names(result))
		result
	}

#' Extract columns
#'
#' Extracts defined values from a single resource
#'
#' @param child A xml child object, representing one FHIR resource
#' @param df.columns The part of design from \code{\link{fhir_crack}} describing which elements to extract
#' from the resouce
#' @param sep A string to separate pasted multiple entries.
#' @param brackets A Vector of Strings defining the Brackets surrounding the Indices. e.g. c( "<", ">") NULL means no brackets.
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

xtrct_columns <- function(child,
			 df.columns,
			 sep = NULL,
			 brackets = NULL) {

		xp <- xml2::xml_path(child)

		l <- lapply(lst(names(df.columns)),
					function(column.name)  {
						#dbg
						#column.name <- names( df.columns )[ 1 ]

						i.srch <- df.columns[[column.name]]

						loc <- xml2::xml_find_all(x = child, xpath = i.srch)

						val <- xml2::xml_text(loc)

						if (!is.null(brackets)) {
							loc.xp <- xml2::xml_path(loc)

							loc.xp.rel <- substr(loc.xp, nchar(xp) + 2, nchar(loc.xp))

							s <- stringr::str_split(loc.xp.rel, "/")

							o <- sapply(seq_along(s),
										function(i) {
											#dbg
											#i<-1

											s. <- s[[i]]

											i.f <- !grepl("\\[[0-9]+\\]", s.)

											if (any(i.f)) {
												s.[i.f] <- paste0(s.[i.f], "[1]")
											}

											gsub(".1$", "", paste0(gsub("[^0-9]", "", s.), collapse = "."))
										})

							if (0 < length(val)) {

								is_av <- ! is.na(val)

								paste0(brackets[1], o[is_av], brackets[2], val[is_av], collapse = sep)
							}
							else
								NA
						}
						else {
							if (0 < length(val)) {paste0(val, collapse = sep)} else {NA}
						}
					})

		data.table::as.data.table(l)
	}

#' Extracts one data frame out of one bundle
#' @param bundle A xml object containing one FHIR bundle
#' @param design.df On element of the design from \code{\link{fhir_crack}}, i.e. a list of length 1
#' or 2, where the first element is a XPath expression to the ressource and the (optional)
#' second element is either a XPath expression or a named list containing column names and XPath expressions
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
#'      resource = ".//MedicationStatement",
#'      cols = list(
#' 	           SYSTEM  = "medicationCodeableConcept/coding/system/@value",
#' 	           CODE    = "medicationCodeableConcept/coding/code/@value",
#' 	           DISPLAY = "medicationCodeableConcept/coding/display/@value"
#' 	          )
#' 	 )
#'
#' #convert bundle to data frame
#' result <- fhircrackr:::bundle2df(bundle, design)
bundle2df <- function(bundle,
			 df_desc,
			 verbose = 2) {

		xml2::xml_ns_strip(bundle)

		xpath <- df_desc$resource

		children <- xml2::xml_find_all(bundle, xpath)

		df.list <- if (length(children) == 0) {

			list()

		} else {
			lapply(children,
				   function(child) {
				   	#dbg
				   	#child <- children[[ 1 ]]

				   	#if multiple columns are defined
				   	if (!is.null(df_desc$cols) && is.list(df_desc$cols)) {

				   		df.columns <- df_desc$cols

				   		res <- xtrct_columns(
				   				child,
				   				df.columns,
				   				sep = df_desc$style$sep,
				   				brackets = df_desc$style$brackets
				   				)

				   		if (1 < verbose) {
				   			if (all(sapply(res, is.na))) {
				   				cat("x")
				   			} else {
				   				cat(".")
				   			}
				   		}

				   	} else {

				   		xp <- if (!is.null(df_desc$cols)) {#if cols is character

				   			df_desc$cols

				   		} else {#if cols is NULL

				   			".//@*"

				   		}

				   		res <-
				   			xtrct_all_columns(
				   				child = child,
				   				sep = df_desc$style$sep,
				   				xpath = xp,
				   				brackets = df_desc$style$brackets
				   			)

				   		if (1 < verbose) {
				   			if (nrow(res) < 1) {
				   				cat("x")
				   			} else {
				   				cat(".")
				   			}
				   		}
				   	}

				   	res
				   })
		}

		data.table::rbindlist(l = df.list, fill=TRUE)
	}

#' Convert several bundles to one data frame
#'
#' @param bundles A list of xml objects containing FHIR bundles
#' @param design.df On element of the design from \code{\link{fhir_crack}}, i.e. a list of length 1
#' or 2, where the first element is a XPath expression to the ressource and the (optional)
#' second element is either a XPath expression or a named list containing column names and XPath expressions
#' @param verbose An Integer Scalar.  If > 1, extraction progress will be printed. Defaults to 2.
#' @noRd
#' @examples
#' #unserialize example bundle
#' bundles <- fhir_unserialize(medication_bundles)
#'
#' #define design
#' df_desc <- list(
#'      resource = ".//MedicationStatement",
#'      cols = list(
#' 	      SYSTEM  = "medicationCodeableConcept/coding/system/@value",
#' 	      CODE    = "medicationCodeableConcept/coding/code/@value",
#' 	      DISPLAY = "medicationCodeableConcept/coding/display/@value"
#' 	      )
#' 	 )
#'
#'
#' #convert bundles to data frame
#' result <- fhircrackr:::bundles2df(bundles, df_desc)

bundles2df <- function(bundles,
					   df_desc,
					   verbose = 2) {

	ret <- data.table::rbindlist(lapply(seq_along(bundles),
										function(i) {
											#dbg
											#i<-1

											if (1 < verbose) {
												cat("\n", i)
											}

											bundle <- bundles[[i]]

											bundle2df(
												bundle,
												df_desc,
												verbose = verbose
											)
										}), fill=TRUE)

	if(nrow(ret > 0)) {ret <- ret[rowSums(!is.na(ret)) > 0, ]}

	if (1 < verbose) {
		cat("\n")
	}

	ret
}

#' Flatten list of FHIR bundles
#' @description Converts all FHIR bundles (the result of \code{\link{fhir_search}}) to a list of data frames.
#'
#' @param bundles A FHIR search result as returned by \code{\link{fhir_search}}.
#' @param design A named list specifying which data frame should contain which entries of the bundle.
#' The names correspond to the names of the resulting data frames.
#'
#' Each element of design is a list of length 1 or 2, where the first element is a XPath expression to locate the entry in a
#' FHIR bundle page. There are 3 options for the second element of that list:
#'
#' - There is no second element: all attributes of the resource are extracted
#' - The second element is string containing a XPath expression to all the values that should be extracted. "./@value" e.g. would extract all
#'   values on the root level.
#' - The second element is a named list where the elements are XPath expressions indicating the specific position of values to extract, where the names of the
#' list elements are the column names of the resulting data frame.
#'
#' For a more detailed explanation see the package vignette.
#' @param data.table Logical scalar. Return list of data.tables instead of data.frames? Defaults to FALSE.
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
#' design <- list(
#'
#'  #define specifically which elements to extract
#' 	MedicationStatement = list(
#'
#' 		resource = ".//MedicationStatement",
#'
#' 		cols= list(
#' 			MS.ID              = "id/@value",
#' 			STATUS.TEXT        = "text/status/@value",
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
#' 		resource = ".//Patient"
#' 	)
#' )
#'
#' #convert fhir to data frames
#' list_of_tables <- fhircrackr:::bundles2dfs(bundles, design)

bundles2dfs <-
	function(bundles,
			 design,
			 data.table = FALSE,
			 verbose = 2) {

		dfs <- lapply(lst(names(design)),
					  function(n) {
					  	#dbg
					  	#n <- names(design)[1]

					  	df_desc <- design[[n]]

					  	if (1 < verbose) {
					  		cat("\n", n)
					  	}

					  	if(is.null(df_desc)){

					  		NULL

					  	}else{

					  		bundles2df(
					  			bundles = bundles,
					  			df_desc = df_desc,
					  			verbose = verbose
					  		)
					  	}

					  })

		if (1 < verbose) {
			cat("\n")
		}

		#remove empty columns for all data.frames with rm_empty_cols=TRUE, keep others as is
		remove <- sapply(design, function(x){
			if(is.null(x$style$rm_empty_cols)) {
				FALSE
			}else{
				x$style$rm_empty_cols
			}
			})

		dfs_cleaned <- lapply(seq_along(dfs),
					  function(i) {

					  	if(remove[i] && ncol(dfs[[i]]) > 0){

					  		dfs[[i]][, colSums(!is.na(dfs[[i]]))>0, with=F]

					  	} else {

					  		dfs[[i]]

					  	}
					  })


		names(dfs_cleaned) <- names(dfs)

		if(data.table){

			return(dfs_cleaned)

		}else{
			return(lapply(dfs_cleaned, data.frame))
		}
	}

#' Escape special characters
#' @param s A string in which the characters should be escaped
#' @return A string with all special characters escaped
#' @example esc(c("(",")"))
#' @noRd
#'
esc <- function(s) {
	gsub("([\\.|\\^|\\$|\\*|\\+|\\?|\\(|\\)|\\[|\\{|\\\\\\|\\|])",
		 "\\\\\\1",
		 s)
}

#' Turn a row with multiple entries into a data frame
#'
#' @param row One row of an indexed data frame
#' @param columns A character vector specifying the names of all columns that should be molten simultaneously.
#' It is advisable to only melt columns simultaneously that belong to the same (repeating) attribute!
#' @param brackets A character vector of length 2, defining the brackets used for the indices.
#' @param sep A string, the separator.
#' @param all_columns A logical scalar. Return all columns or only the ones specified in \code{columns}?
#' @return A data frame with nrow > 1
#' @noRd


melt_row <-
	function(row,
			 columns,
			 brackets = c("<", ">"),
			 sep = " ",
			 all_columns = FALSE) {

		row <- as.data.frame(row)

		col.names.mutable  <- columns

		col.names.constant <- setdiff(names(row), col.names.mutable)

		row.mutable  <- row[col.names.mutable]

		row.constant <- row[col.names.constant]

		#dbg
		#row <- d3.3$Entries[ 1, ]

		brackets.escaped <- esc(brackets)

		pattern.ids <- paste0(brackets.escaped[1], "([0-9]+\\.*)+", brackets.escaped[2])

		ids <- stringr::str_extract_all(row.mutable, pattern.ids)

		names(ids) <- col.names.mutable

		pattern.items <- paste0(brackets.escaped[1], "([0-9]+\\.*)+", brackets.escaped[2])

		items <- stringr::str_split(row.mutable, pattern.items)

		items <-
			lapply(items, function(i) {
				if (!all(is.na(i)) && i[1] == "") {
					i[2:length(i)]
				} else {
					i
				}
			})

		names(items) <- col.names.mutable

		d <-
			if (all_columns) {
				row[0, , FALSE]
			} else {
				row[0, col.names.mutable, FALSE]
			}

		for (i in names(ids)) {
			#dbg
			#i<-names( ids )[1]

			id <- ids[[i]]

			if (!all(is.na(id))) {
				it <- items[[i]]

				new.rows        <-
					gsub(paste0(brackets.escaped[1], "([0-9]+)\\.*.*"),
						 "\\1",
						 id)
				new.ids         <-
					gsub(
						paste0(
							"(",
							brackets.escaped[1],
							")([0-9]+)\\.*(.*",
							brackets.escaped[2],
							")"
						),
						"\\1\\3",
						id
					)
				unique.new.rows <- unique(new.rows)

				set <- paste0(new.ids, it)

				f <- sapply(unique.new.rows,
							function(unr) {
								#dbg
								#unr <- unique.new.rows[1]

								fltr <- unr == new.rows

								paste0(set[fltr], collapse = "")
							})

				for (n in unique.new.rows) {
					d[n, i] <- gsub(paste0(esc(sep), "$"), "", f[n], perl = TRUE)
				}
			}
		}

		if (0 < length(col.names.constant) && all_columns) {
			if (0 < nrow(d))
				d[, col.names.constant] <-
					dplyr::select(row, col.names.constant)
			else
				d[1, col.names.constant] <-
					dplyr::select(row, col.names.constant)
		}

		data.table::data.table(d)
	}

#to ensure data.table version of d[] is called, even though it is not explicitly stated in
#import section of NAMESPACE file (https://cran.r-project.org/web/packages/data.table/vignettes/datatable-importing.html)
.datatable.aware = TRUE