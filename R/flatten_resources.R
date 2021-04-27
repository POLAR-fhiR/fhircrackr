## This file contains all functions needed for flattening ##
## Exported functions are on top, internal functions below ##


#' Flatten list of FHIR bundles
#' @description Converts a [fhir_bundle_list-class] (the result of [fhir_search()] to a list of data.frames/data.tables,
#' i.e. a [fhir_df_list-class]/[fhir_dt_list-class]
#'
#' @param bundles A FHIR search result as returned by [fhir_search()].
#' @param design A [fhir_design-class] object. See `?fhir_design` and the corresponding vignette
#' (`vignette("flattenResources", package ="fhircrackr")`) for a more detailed explanation and
#' comprehensive examples of a [fhir_design-class].
#'
#' @param sep Optional. A string to separate pasted multiple entries which will overwrite the `sep` defined in the
#' `design`. If `sep=NULL`, it is looked up in the `design`, where the default is `" "`.
#'
#' @param remove_empty_columns Optional. Remove empty columns? Logical scalar which will overwrite the `rm_empty_cols` defined in the
#' `design`. If `remove_empty_columns=NULL`, it is looked up in the `design`, where the default is `TRUE`.
#'
#' @param brackets Optional. A character vector of length two defining the brackets surrounding indices for multiple entries, e.g. \code{c( "<", ">")},
#' which will overwrite the `brackets` defined in the `design`. If `brackets=NULL`, it is looked up in the `design`, where the default is `character(0)`,
#' i.e. no indices are added to multiple entries.
#'
#' @param verbose An Integer Scalar.  If 0, nothing is printed, if 1, only finishing message is printed, if > 1,
#' extraction progress will be printed. Defaults to 2.
#'
#' @param data.table Logical scalar. Should tables be returned in data.table format instead of data.frame?
#' defaults to FALSE.
#'
#' @return A list of data.frames, i.e. a [fhir_df_list-class] object, or a list of data.tables, i.e. a [fhir_dt_list-class] object.
#'
#' @export
#' @import data.table
#' @include fhir_design.R fhir_bundle_list.R fhir_table_list.R
#' @examples
#' #unserialize example bundle
#' bundles <- fhir_unserialize(medication_bundles)
#'
#' #define attributes to extract
#' medications <- fhir_df_description(
#'    resource = "MedicationStatement",
#'    cols = c(
#' 			    	MS.ID              = "id",
#' 				    STATUS.TEXT        = "text/status",
#' 			    	STATUS             = "status",
#' 			    	MEDICATION.SYSTEM  = "medicationCodeableConcept/coding/system",
#' 			    	MEDICATION.CODE    = "medicationCodeableConcept/coding/code",
#' 			    	MEDICATION.DISPLAY = "medicationCodeableConcept/coding/display",
#' 			    	DOSAGE             = "dosage/text",
#' 			    	PATIENT            = "subject/reference",
#' 			    	LAST.UPDATE        = "meta/lastUpdated"
#' 	    	),
#' 	 style = fhir_style(
#'  			sep = " ",
#' 	        	brackets = c("[", "]"),
#'     		    rm_empty_cols= FALSE
#'     		)
#' )
#'
#' patients <- fhir_df_description(
#'    resource = "Patient"
#' )
#'
#' design <- fhir_design(medications, patients)
#'
#' #convert bundles to data frames
#' tables <- fhir_crack(bundles, design)
#'
#' #check results
#' head(tables$medications)
#' head(tables$patients)
#'
#' #The design that was used can be extracted from the tables like this:
#' fhir_design(tables)
#'
#' @export

fhir_crack <- function(
	bundles,
	design,
	sep = NULL,
	remove_empty_columns = NULL,
	brackets = NULL,
	verbose = 2,
	data.table = FALSE) {


	if(!is(design, "fhir_design")){
		warning("The use of an old-style design will be disallowed in the future. ",
			 "Please consider building the design with the function fhir_design().\n",
			 "Converting design to fhir_design object.")
		suppressMessages(design <- fhir_design(design))
	}
	#overwrite design with function arguments
	if (!is.null(sep)) {
		design <- fhir_design(lapply(
			design,
			function(x){
				x@style@sep <- sep
				x
			}
		))
	}
	if (!is.null(brackets)) {
		brackets <- fix_brackets(brackets)
		design <-fhir_design(lapply(
			design,
			function(x){
				x@style@brackets <- brackets
				x
			}
		))
	}
	if (!is.null(remove_empty_columns)) {
		design <- fhir_design(lapply(
			design,
			function(x){
				x@style@rm_empty_cols <- remove_empty_columns
				x
			}
		))
	}

	#Check for dangerous XPath expressions ins cols
	cols <- lapply(design, function(x){c(x@cols)})
	dangerCols <- sapply(cols, function(x){any(grepl(esc("//"), x))})
	if(any(dangerCols)){
		warning("In the cols element of the design, you specified XPath expressions containing '//' which point to an ",
		"arbitrary level in the resource. \nThis can result in unexpected behaviour, e.g. when the searched element appears ",
		"on different levels of the resource. \n", "We strongly advise to only use the fully specified relative XPath in the cols ",
		"element, e.g. 'ingredient/strength/numerator/code' instead of search paths like '//code'. \n",
		"This warning is thrown for the following data.frame descriptions: ", paste(names(cols)[dangerCols], collapse=", "))
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

	if(length(brackets)==0){brackets <- NULL}

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
#' @param df.columns A [fhir_columns-class] object describing which elements to extract
#' from the resource
#' @param sep A string to separate pasted multiple entries.
#' @param brackets A character vector defining the brackets surrounding the indices. e.g. c( "<", ">").
#' `character(0)` means no brackets.
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
#' cols <-fhir_columns(c(
#' 	SYSTEM  = "medicationCodeableConcept/coding/system/@value",
#' 	CODE    = "medicationCodeableConcept/coding/code/@value",
#' 	DISPLAY = "medicationCodeableConcept/coding/display/@value"
#' 	)
#' )
#'
#' #Extract columns
#' result <- fhircrackr:::xtrct_columns(child, cols)

xtrct_columns <- function(
	child,
	cols,
	sep = NULL,
	brackets = NULL) {

	if(length(brackets)==0){brackets <- NULL}

	xp <- xml2::xml_path(child)
	l <- lapply(
		lst(names(cols)),
		function(column.name)  {
			i.srch <- cols[[column.name]]
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
#' @param df_desc An object of class [fhir_df_description-class].
#' @param verbose An integer scalar.  If > 1, extraction progress will be printed. Defaults to 2.
#' @noRd
#' @examples
#' #unserialize example bundle
#' bundles <- fhir_unserialize(medication_bundles)
#'
#' #extract first bundle
#' bundle <- bundles[[1]]
#'
#' #define df_description
#' df_desc <- fhir_df_description(
#'      resource = "MedicationStatement",
#'      cols = list(
#' 	           SYSTEM  = "medicationCodeableConcept/coding/system/@value",
#' 	           CODE    = "medicationCodeableConcept/coding/code/@value",
#' 	           DISPLAY = "medicationCodeableConcept/coding/display/@value"
#' 	          ),
#' 	    style = fhir_style(
#' 	          sep=" ",
#' 	          brackets = c("[","]"),
#' 	          rm_empty_cols =T
#' 	    )
#' 	 )
#'
#' #convert bundle to data frame
#' result <- fhircrackr:::bundle2df(bundle, df_desc)
bundle2df <- function(
	bundle,
	df_desc,
	verbose = 2) {

	xpath <- paste0("//", df_desc@resource)
	children <- xml2::xml_find_all(bundle, xpath)
	df.list <- if (length(children) == 0) {
		list()
	} else {
		lapply(
			children,
			function(child) {
		   	#dbg
		   	#child <- children[[ 1 ]]


			   	if (length(df_desc@cols)>0) {#if cols is not empty
			   		cols <- df_desc@cols
			   		res <- xtrct_columns(child, cols, sep = df_desc@style@sep, brackets = df_desc@style@brackets)
			   		if (1 < verbose) {
			   			if (all(sapply(res, is.na))) {cat("x")} else {cat(".")}
			   		}
			   	} else {#if cols empty
			   		xp <- ".//@*"
			   		res <- xtrct_all_columns( child = child, sep = df_desc@style@sep, xpath = xp, brackets = df_desc@style@brackets)
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
#' @param bundles A [fhir_bundle_list-class] object
#' @param df_desc A [fhir_df_desc-class] object
#' @param verbose An Integer Scalar.  If > 1, extraction progress will be printed. Defaults to 2.
#' @noRd
#' @examples
#' #unserialize example bundle
#' bundles <- fhir_unserialize(medication_bundles)
#'
#' df_desc <- fhir_df_description(
#'      resource = "MedicationStatement",
#'      cols = list(
#' 	           SYSTEM  = "medicationCodeableConcept/coding/system/@value",
#' 	           CODE    = "medicationCodeableConcept/coding/code/@value",
#' 	           DISPLAY = "medicationCodeableConcept/coding/display/@value"
#' 	          ),
#' 	    style = fhir_style(
#' 	          sep=" ",
#' 	          brackets = c("[","]"),
#' 	          rm_empty_cols =T
#' 	    )
#' 	 )
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
#' @description Converts all FHIR bundles (the result of `fhir_search`) to a list of data frames.
#'
#' @param bundles A [fhir_bundle_list-class] object
#' @param design A [fhir_design-class] object
#' @param data.table Logical scalar. Return list of data.tables instead of data.frames? Defaults to FALSE.
#' @param verbose An Integer Scalar.  If > 1, extraction progress will be printed. Defaults to 2.
#' @noRd
#' @return A [fhir_df_list-class]/[fhir_dt_list-class] object as specified by `design`.
#'
#'
#' @examples
#' #unserialize example bundle
#' bundles <- fhir_unserialize(medication_bundles)
#'
#' #define attributes to extract
#' design <- fhir_design(
#'
#' 	 fhir_df_description(
#'
#' 		resource = "MedicationStatement",
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
#'  fhir_df_description(
#'
#' 		resource = "Patient"
#' 	),
#' 	names = c("Medications", "Patients")
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
		function(x){x@style@rm_empty_cols}
	)
	dfs_cleaned <- lapply(
		seq_along(dfs),
		function(i) {
			if(remove[i] && ncol(dfs[[i]]) > 0){
				dfs[[i]][, colSums(!is.na(dfs[[i]]))>0, with=F]} else {dfs[[i]]}
		}
	)
	names(dfs_cleaned) <- names(dfs)
	if(data.table){
		fhir_dt_list(dt_list = dfs_cleaned, design=design)
	} else {
		fhir_df_list(lapply(dfs_cleaned, data.frame), design)
	}
}

