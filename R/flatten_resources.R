## This file contains all functions needed for flattening ##
## Exported functions are on top, internal functions below ##


#' Flatten list of FHIR bundles
#' @description Converts all FHIR bundles (the result of \code{\link{fhir_search}}) to a list of data frames.
#'
#' @param bundles A FHIR search result as returned by \code{\link{fhir_search}}.
#' @param design A named list of data frame descriptions.
#' Each data frame description will produce one data frame in the list of data frames returned by \code{fhir_crack}, where the data frame has the same name as the data frame description in \code{design}.
#'
#' Each data frame description is a list of 3 named elements:
#'
#' 1) \code{design$resource}: Mandatory. A string with an XPath expression locating the entries for this data frame in a FHIR bundle page. This is usually the path to a resource tpye
#'  such as \code{"//Patient"} or \code{"//Observation"}.
#'
#' 2) \code{design$cols}: Optional. Either a string containing an XPath expression referencing a certain level of attributes that should be extracted (
#'  \code{"./@value"} e.g. would extract all values on the root level) or a named list where the elements are XPath expressions indicating the specific
#'   position of attributes to extract and the names of the list elements are the column names of the resulting data frame. If \code{design$cols} is \code{NULL},
#'   all available attributes will be extracted.
#'
#' 3) \code{design$style}: Optional. This can be used instead of the function arguments \code{sep}, \code{brackets} and \code{remove_empty_columns}, but will be
#' overwritten if the corresponding function arguments are not \code{NULL}.
#'
#' A named list with the following optional elements:
#'
#'    * \code{design$style$sep} : A string to separate pasted multiple entries.
#'
#'    * \code{design$style$brackets}: A character vector of length two defining the brackets surrounding indices for multiple entries, e.g. \code{c( "<", ">")}.
#'       If \code{NULL}, no indices will be added to multiple entries.
#'
#'    * \code{design$style$rm_empty_cols}: Logical scalar. Remove empty columns?
#'
#' For a more detailed explanation and comprehensive examples of \code{design}, please see the package vignette.
#'
#' @param sep A string to separate pasted multiple entries. NULL means \code{sep} is looked up in design, if it is \code{NULL} there too, \code{sep} will be set to \code{" "} as the default.
#'
#' @param remove_empty_columns Logical scalar. Remove empty columns? \code{NULL} means \code{remove_empty_columns} is looked up in \code{design}, if it is \code{NULL} there too, \code{remove_empty_columns}
#'  will be set to \code{TRUE} as the default.
#'
#' @param brackets A character vector of length two defining the brackets surrounding indices for multiple entries, e.g. \code{c( "<", ">")}.
#'  If \code{NULL}, no indices will be added to multiple entries. \code{NULL} means \code{brackets} is looked up in design, if it is \code{NULL} there too, no indices are added.
#'
#' @param verbose An Integer Scalar.  If 0, nothing is printed, if 1, only finishing message is printed, if > 1,
#' extraction progress will be printed. Defaults to 2.
#'
#' @param data.table Logical scalar. Should tables be returned in data.table format instead of data.frame?
#' defaults to FALSE.
#'
#'
#' @param add_indices Deprecated. This argument was used to control adding of indices for multiple entries. This is now
#' done via the brackets argument. If brackets is \code{NULL}, no indices are added, if brackets is not \code{NULL}, indices are added to multiple entries.
#'
#' @return A list of data frames (if \code{data.table = FALSE}) or a list of data.tables
#' if \code{data.table = TRUE}.
#'
#' @export
#' @import data.table
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
#' 		cols = list(
#' 				MS.ID              = "id",
#' 				STATUS.TEXT        = "text/status",
#' 				STATUS             = "status",
#' 				MEDICATION.SYSTEM  = "medicationCodeableConcept/coding/system",
#' 				MEDICATION.CODE    = "medicationCodeableConcept/coding/code",
#' 				MEDICATION.DISPLAY = "medicationCodeableConcept/coding/display",
#' 				DOSAGE             = "dosage/text",
#' 				PATIENT            = "subject/reference",
#' 				LAST.UPDATE        = "meta/lastUpdated"
#' 		),
#'
#' 		style = list(
#' 				sep = " ",
#' 				brackets = c("[", "]"),
#' 				rm_empty_cols= FALSE
#' 				)
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
#' list_of_tables <- fhir_crack(bundles, design)
#'
#' #check results
#' head(list_of_tables$MedicationStatement)
#' head(list_of_tables$Patients)
#'
#'
#' @export

fhir_crack <- function(
	bundles,
	design,
	sep = NULL,
	remove_empty_columns = NULL,
	brackets = NULL,
	verbose = 2,
	data.table = FALSE,
	add_indices) {

	#-----------------------# remove once add_indices is removed:
	if (!missing("add_indices")) {
		warning("Argument add_indices is deprecated and will be removed eventually.\n In future versions indices will automatically be added when brackets are provided.")
		if(add_indices && is.null(brackets)) {brackets <- c("<", ">")}
		if(!add_indices && !is.null(brackets)) {brackets <- NULL}
	}

	#-----------------------#

	#check input validity
	design_validity <- is_valid_design(design)
	#IF general problems with design
	if (!design_validity[[1]] && is.null(design_validity[[2]])){return(NULL)}
	#If single invalid data.frame descriptions
	if (!design_validity[[1]] && !is.null(design_validity[[2]])) {design[design_validity[[2]]] <- "invalid"}
	#If invalid bundle list
	if (is_invalid_bundles_list(bundles)) {return(NULL)}
	#complete design
	design <- fix_design(design)
	#overwrite design with function arguments
	if (!is.null(sep)) {
		design <- lapply(
			design,
			function(x){
				x$style$sep <- sep
				x
			}
		)
	}
	if (!is.null(brackets)) {
		brackets <- fix_brackets(brackets)
		design <-lapply(
			design,
			function(x){
				x$style$brackets <- brackets
				x
			}
		)
	}
	if (!is.null(remove_empty_columns)) {
		design <- lapply(
			design,
			function(x){
				x$style$rm_empty_cols <- remove_empty_columns
				x
			}
		)
	}
	#Add attributes to design
	design <- add_attribute_to_design(design)
	#crack
	dfs <- bundles2dfs(bundles = bundles, design = design, data.table = data.table, verbose = verbose)
	if (0 < verbose) {message("FHIR-Resources cracked. \n")}
	assign(x = "canonical_design", value = design, envir = fhircrackr_env)
	dfs
}


############################################################################################
##############################################################################################

#' Check List of Bundles
#' @description Checks whether a List of Bundles provided to \code{\link{fhir_crack}} is invalid and
#' issues a warning if it is.
#' @param bundles_list The List of Bundles to be checked
#' @return TRUE if bundles_list is invalid, FALSE if not
#' @noRd
#'
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
		warning("Argument bundles seems to contain serialized bundles. Use fhir_unserialize() before proceeding. Returning NULL")
		return(TRUE)
	}
	valid.doc.types <- all(
		sapply(
			bundles_list,
			function(b) {
				if (is.null(b)) {FALSE} else {
			  		cl <- class(b)
			  		length(cl) == 2 || cl[1] == "xml_document" || cl[2] == "xml_node"
			  	}
			}
		)
	)
	if (!valid.doc.types) {
		warning("Argument bundles contains at least one invalid Bundle. Bundles have to be of Class 'xml_document' and 'xml_node'. Returning NULL")
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
xtrct_all_columns <- function(
	child,
	sep = NULL,
	xpath = ".//@*",
	brackets = NULL) {

	tree <- xml2::xml_find_all(child, xpath)
	if (length(tree) < 1) {return(data.table::data.table())}
	xp.child  <- xml2::xml_path(child)
	xp.remain <- xml2::xml_path(tree)
	xp.rel    <- substr(xp.remain, nchar(xp.child) + 2, nchar(xp.remain))
	xp.cols   <- gsub("/", ".", gsub("@", "", unique(gsub("\\[[0-9]+\\]", "", xp.rel))))
	d <- lapply(1:length(xp.cols), function(dummy)character(0))
	names(d) <- xp.cols
	val  <- xml2::xml_text(tree)
	s <- stringr::str_split(xp.rel, "/")
	o <- sapply(
		seq_along(s),
		function(i) {
			s. <- s[[i]]
			i.f <- !grepl("\\[[0-9]+\\]", s.)
			if (any(i.f)) {s.[i.f] <- paste0(s.[i.f], "[1]")}
			c(
				paste0(gsub("]$","",gsub(".*\\[","",s.[-length(s.)])), collapse = "."),
				gsub("@", "", gsub("\\[[0-9]+\\]", "", paste0(s., collapse = ".")))
			)
		}
	)
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

xtrct_columns <- function(
	child,
	df.columns,
	sep = NULL,
	brackets = NULL) {

	xp <- xml2::xml_path(child)
	l <- lapply(
		lst(names(df.columns)),
		function(column.name)  {
			i.srch <- df.columns[[column.name]]
			loc <- xml2::xml_find_all(x = child, xpath = i.srch)
			val <- xml2::xml_text(loc)
			if (!is.null(brackets)) {
				loc.xp <- xml2::xml_path(loc)
				loc.xp.rel <- substr(loc.xp, nchar(xp) + 2, nchar(loc.xp))
				s <- stringr::str_split(loc.xp.rel, "/")
				o <- sapply(
					seq_along(s),
					function(i) {
						s. <- s[[i]]
						i.f <- !grepl("\\[[0-9]+\\]", s.)
						if (any(i.f)) {s.[i.f] <- paste0(s.[i.f], "[1]")}
						gsub(".1$", "", paste0(gsub("[^0-9]", "", s.), collapse = "."))
					}
				)

				if (0 < length(val)) {
					is_av <- ! is.na(val)
					paste0(brackets[1], o[is_av], brackets[2], val[is_av], collapse = sep)
				}
				else {NA}
			}
			else {
				if (0 < length(val)) {paste0(val, collapse = sep)} else {NA}
			}
		}
	)
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
bundle2df <- function(
	bundle,
	df_desc,
	verbose = 2) {

	xml2::xml_ns_strip(bundle)
	xpath <- df_desc$resource
	children <- xml2::xml_find_all(bundle, xpath)
	df.list <- if (length(children) == 0) {
		list()
	} else {
		lapply(
			children,
			function(child) {
		   	#dbg
		   	#child <- children[[ 1 ]]

		   	#if multiple columns are defined
			   	if (!is.null(df_desc$cols) && is.list(df_desc$cols)) {
			   		df.columns <- df_desc$cols
			   		res <- xtrct_columns(child, df.columns, sep = df_desc$style$sep, brackets = df_desc$style$brackets)
			   		if (1 < verbose) {
			   			if (all(sapply(res, is.na))) {cat("x")} else {cat(".")}
			   		}
			   	} else {#if cols is character
			   		xp <- if (!is.null(df_desc$cols)) {df_desc$cols} else {".//@*"} #else cols is NULL
			   		res <- xtrct_all_columns( child = child, sep = df_desc$style$sep, xpath = xp, brackets = df_desc$style$brackets)
			   		if (1 < verbose) {
			   			if (nrow(res) < 1) {cat("x")} else {cat(".")}
			   		}
			   	}
				res
			}
		)
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

bundles2df <- function(
	bundles,
	df_desc,
	verbose = 2) {

	ret <- data.table::rbindlist(
		lapply(
			seq_along(bundles),
			function(i) {
				#dbg
				#i<-1
				if (1 < verbose) {cat("\n", i)}
				bundle <- bundles[[i]]
				bundle2df(bundle, df_desc, verbose = verbose)
			}
		),
		fill = TRUE
	)
	if(nrow(ret > 0)) {ret <- ret[rowSums(!is.na(ret)) > 0, ]}
	if (1 < verbose) {cat("\n")}
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

bundles2dfs <- function(
	bundles,
	design,
	data.table = FALSE,
	verbose = 2) {

	dfs <- lapply(
		lst(names(design)),
		function(n) {
			df_desc <- design[[n]]
		  	if (1 < verbose) {cat("\n", n)}

		  	if(is.null(df_desc)){NULL} else {bundles2df(bundles = bundles, df_desc = df_desc, verbose = verbose)}
		}
	)
	if (1 < verbose) {cat("\n")}
	#remove empty columns for all data.frames with rm_empty_cols=TRUE, keep others as is
	remove <- sapply(
		design,
		function(x){
			if(is.null(x$style$rm_empty_cols)) {FALSE} else {x$style$rm_empty_cols}
		}
	)
	dfs_cleaned <- lapply(
		seq_along(dfs),
		function(i) {
			if(remove[i] && ncol(dfs[[i]]) > 0){
				dfs[[i]][, colSums(!is.na(dfs[[i]]))>0, with=F]} else {dfs[[i]]}
		}
	)
	names(dfs_cleaned) <- names(dfs)
	if(data.table){return(dfs_cleaned)} else {return(lapply(dfs_cleaned, data.frame))}
}

