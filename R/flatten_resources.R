## This file contains all functions needed for flattening ##
## Exported functions are on top, internal functions below ##


#' Flatten list of FHIR bundles
#' @description Converts a [fhir_bundle_list-class] (the result of [fhir_search()] to a list of data.frames/data.tables,
#' i.e. a [fhir_df_list-class]/[fhir_dt_list-class] if a [fhir_design-class] is given in the argument `design`.
#' Creates a single data.frame/data.table, if only a [fhir_table_description-class] is given in the argument `design`.
#'
#' @param bundles A FHIR search result as returned by [fhir_search()].
#' @param design A [fhir_design-class] or [fhir_table_description-class] object. See [fhir_design()]/[fhir_table_description()]
#' and the corresponding vignette (`vignette("flattenResources", package ="fhircrackr")`) for a more detailed explanation and
#' comprehensive examples of both.
#'
#' @param sep Optional. A character vector of length ones to separate pasted multiple entries which will overwrite the `sep` defined in
#' `design`. If `sep = NULL`, it is looked up in `design`, where the default is `":::"`.
#'
#' @param brackets Optional. A character vector of length two defining the brackets surrounding indices for multiple entries, e.g. \code{c("<|", "|>")},
#' which will overwrite the `brackets` defined in `design`. If `brackets = NULL`, it is looked up in `design`, where the default is `character(0)`,
#' i.e. no indices are added to multiple entries. Empty strings (`""`) are not allowed.
#'
#' @param remove_empty_columns Optional. Remove empty columns? Logical scalar which will overwrite the `rm_empty_cols` defined in
#' `design`. If `remove_empty_columns = NULL`, it is looked up in `design`, where the default is `FALSE`.
#'
#' @param verbose An integer vector of length one. If 0, nothing is printed, if 1, only finishing message is printed, if > 1,
#' extraction progress will be printed. Defaults to 2.
#'
#' @param data.table A logical vector of length one. If it is set to TRUE the fhir_crack-function returns a data.table, otherwise a data.frame.
#' Defaults to FALSE.
#'
#' @param format A character of length 1 containing the format of the cracked table. Possible formats
#' are "compact" and "wide". Defaults to "compact"
#'
#' @param keep_attr A locigcal of length 1. Should column names be extended by the attribute names? Defaults to FALSE.
#'
#' @param ncores Either NULL (1 core) or an integer of length 1 containing the number of
#'  cpu cores that should be used for cracking. Defaults to NULL.
#'
#' @return If a [fhir_design-class] was used, the result is a list of data.frames, i.e. a [fhir_df_list-class] object, or a list of data.tables,
#' i.e. a [fhir_dt_list-class] object. If a [fhir_table_description-class] was used, the result is a single data.frame/data.table.
#'
#' @export
#' @rdname fhir_crack-methods
#' @docType methods
#' @include fhir_design.R fhir_bundle_list.R fhir_table_list.R
#' @seealso
#' - Downloading bundles from a FHIR server: [fhir_search()]
#' - Creating designs/table_descriptions: [fhir_table_description()] and [fhir_design()]
#' - Dealing with multiple entries: [fhir_melt()],  [fhir_rm_indices()]
#' @examples
#' #unserialize example bundle
#' bundles <- fhir_unserialize(medication_bundles)
#'
#'
#' ###Example 1###
#' #Extract just one resource type
#'
#' #define attributes to extract
#' medications <- fhir_table_description(
#'    resource = "MedicationStatement",
#'    cols     = c(
#'    	MS.ID              = "id",
#'    	STATUS.TEXT        = "text/status",
#'    	STATUS             = "status",
#'    	MEDICATION.SYSTEM  = "medicationCodeableConcept/coding/system",
#'    	MEDICATION.CODE    = "medicationCodeableConcept/coding/code",
#'    	MEDICATION.DISPLAY = "medicationCodeableConcept/coding/display",
#'    	DOSAGE             = "dosage/text",
#'     	PATIENT            = "subject/reference",
#'     	LAST.UPDATE        = "meta/lastUpdated"
#'   ),
#'   style     = fhir_style(
#'    	sep           = " ",
#'    	brackets      = c("[", "]"),
#'    	rm_empty_cols = FALSE
#'   )
#' )
#'
#' med_df <- fhir_crack(bundles = bundles, design = medications)
#'
#' head(med_df) #data.frame
#'
#'
#' ###Example 2###
#' #extract more resource types
#'
#' patients <- fhir_table_description(
#'    resource = "Patient"
#' )
#'
#' design <- fhir_design(medications, patients)
#'
#' df_list <- fhir_crack(bundles = bundles, design = design)
#'
#' #list of data.frames/fhir_df_list
#' head(df_list$medications)
#' head(df_list$patients)
#'
#' #The design that was used can be extracted from a fhir_df_list
#' fhir_design(df_list)
#'

setGeneric(
	name = "fhir_crack",
	def = function(
		bundles,
		design,
		sep                  = NULL,
		brackets             = NULL,
		remove_empty_columns = NULL,
		verbose              = 2,
		data.table           = FALSE,
		format               = "compact",
		keep_attr            = FALSE,
		ncores               = NULL) {

		standardGeneric("fhir_crack")
	}
)

#' @rdname fhir_crack-methods
#' @aliases fhir_crack,fhir_design-method
setMethod(
	f = "fhir_crack",
	signature = c(design = "fhir_design"),
	definition = function(
		bundles,
		design,
		sep                  = NULL,
		brackets             = NULL,
		remove_empty_columns = NULL,
		verbose              = 2,
		data.table           = FALSE,
		format               = "compact",
		keep_attr            = FALSE,
		ncores               = NULL) {

		#overwrite design with function arguments
		if(!is.null(sep)) {
			design <- fhir_design(lapply(
				design,
				function(x) {
					x@style@sep <- sep
					x
				})
			)
		}

		if(!is.null(brackets)) {
			brackets <- fix_brackets(brackets = brackets)
			design <-fhir_design(lapply(
				design,
				function(x) {
					x@style@brackets <- brackets
					x
				}
			))
		}

		if(!is.null(remove_empty_columns)) {
			design <- fhir_design(lapply(
				design,
				function(x) {
					x@style@rm_empty_cols <- remove_empty_columns
					x
				}
			))
		}

		validObject(object = design, complete = TRUE)
		#Check for dangerous XPath expressions ins cols
		cols <- lapply(design, function(x) {c(x@cols)})
		dangerCols <- sapply(cols, function(x) {any(grepl(esc("//"), x))})

		if(any(dangerCols)) {
			warning(
				"In the cols element of the design, you specified XPath expressions containing '//' which point to an ",
				"arbitrary level in the resource. \nThis can result in unexpected behaviour, e.g. when the searched element appears ",
				"on different levels of the resource. \n", "We strongly advise to only use the fully specified relative XPath in the cols ",
				"element, e.g. 'ingredient/strength/numerator/code' instead of search paths like '//code'. \n",
				"This warning is thrown for the following data.frame descriptions: ", paste(names(cols)[dangerCols], collapse = ", ")
			)
		}

		#Add attributes to design
		design <- add_attribute_to_design(design = design)
		os <- get_os()
		ncores <- if(is.null(ncores)) 1 else min(c(get_ncores(os), ncores))
		message(paste0("Cracking under OS ", os, " using ", ncores, if(1 < ncores) " cores." else " core."))

		dfs <- bundles2dfs(bundles = bundles, design = design, data.table = data.table, verbose = verbose, ncores = ncores)
		if(0 < verbose) {message("FHIR-Resources cracked. \n")}
		assign(x = "canonical_design", value = design, envir = fhircrackr_env)
		dfs
	}
)

#' @rdname fhir_crack-methods
#' @aliases fhir_crack,fhir_table_description-method
setMethod(
	f = "fhir_crack",
	signature = c(design = "fhir_table_description"),
	definition = function(
		bundles,
		design,
		sep                  = NULL,
		brackets             = NULL,
		remove_empty_columns = NULL,
		verbose              = 2,
		data.table           = FALSE,
		format               = "compact",
		keep_attr            = FALSE,
		ncores               = NULL) {

		#overwrite design with function arguments
		if(!is.null(sep)) {design@style@sep <- sep}

		if(!is.null(brackets)) {
			brackets <- fix_brackets(brackets = brackets)
			design@style@brackets <- brackets
		}

		if(!is.null(remove_empty_columns)) {
			design@style@rm_empty_cols <- remove_empty_columns
		}

		validObject(object = design, complete = TRUE)
		#Check for dangerous XPath expressions ins cols
		cols <- design@cols
		dangerCols <- sapply(cols, function(x) {any(grepl(esc("//"), x))})

		if(any(dangerCols)) {
			warning(
				"In the cols element of the design, you specified XPath expressions containing '//' which point to an ",
				"arbitrary level in the resource. \nThis can result in unexpected behaviour, e.g. when the searched element appears ",
				"on different levels of the resource. \n", "We strongly advise to only use the fully specified relative XPath in the cols ",
				"element, e.g. 'ingredient/strength/numerator/code' instead of search paths like '//code'. \n",
				"This warning is thrown for the following data.frame descriptions: ", paste(names(cols)[dangerCols], collapse = ", ")
			)
		}

		#Add attributes to design
		design <- add_attribute_to_design(design = design)
		os <- get_os()
		ncores <- if(is.null(ncores)) 1 else min(c(get_ncores(os), ncores))
		message(paste0("Cracking under OS ", os, " using ", ncores, if(1 < ncores) " cores." else " core."))

		#crack
		df <- bundles2df(bundles = bundles, df_desc = design, verbose = verbose, format = format, keep_attr = keep_attr, ncores = ncores)
		#remove empty columns for all data.frames with rm_empty_cols=TRUE, keep others as is
		remove <- design@style@rm_empty_cols

		if(remove && 0 < ncol(df)) {
			df_cleaned <- df[, 0 < colSums(!is.na(df)), with = FALSE]
		} else {
			df_cleaned <- df
		}

		if(0 < verbose) {message("FHIR-Resources cracked.")}
		assign(x = "canonical_design", value = design, envir = fhircrackr_env)
		if(data.table) {df} else {data.frame(df)}
	}
)

#' @rdname fhir_crack-methods
#' @aliases fhir_crack,list-method
setMethod(
	f          = "fhir_crack",
	signature  = c(design = "list"),
	definition = function(
		bundles,
		design,
		sep                  = NULL,
		brackets             = NULL,
		remove_empty_columns = NULL,
		verbose              = 2,
		data.table           = FALSE,
		format               = "compact",
		keep_attr            = FALSE,
		ncores               = NULL) {

		warning(
			"The use of an old-style design will be disallowed in the future. ",
			"Please consider building the design with the function fhir_design().\n",
			"Converting design to fhir_design object."
		)
		suppressMessages(design <- fhir_design(design))
		fhir_crack(
			bundles              = bundles,
			design               = design,
			sep                  = sep,
			brackets             = brackets,
			remove_empty_columns = remove_empty_columns,
			verbose              = verbose,
			data.table           = data.table,
			format               = format,
			keep_attr            = keep_attr,
			ncores               = ncores
		)
	}
)

############################################################################################
##############################################################################################


#' Extract all columns
#'
#' Extracts all available values from a single resource
#'
#' @param child A xml child object, representing one FHIR resource
#' from the resource
#' @param sep A character vector of length one to separate pasted multiple entries.
#' @param xpath A character vector of length one to locate data in tree via xpath.
#' @param brackets A character vector of length one or two defining the brackets
#' surrounding the indices. e.g. c( "<", ">") NULL means no brackets.
#' A vector of length one like c("|") means that the "|"-sign will be used as opening and closing Brackets.
#' @param format A character of length 1 containing the format of the cracked table. Possible formats
#' are "compact" and "wide". Defaults to "compact"
#' @param keep_attr A locigcal of length 1. Should column names be extended by the attribute names? Defaults to FALSE.
#' @noRd
#'
#' @examples
#' #unserialize example bundle
#' bundles <- fhir_unserialize(bundles = medication_bundles)
#'
#' #extract child
#' child <- xml2::xml_find_first(x = bundles[[1]], xpath = ".//MedicationStatement")
#'
#' #Extract all columns
#' result <- fhircrackr:::xtrct_all_columns(child = child)
#'
xtrct_all_columns <- function(
	child,
	xpath = ".//@*",
	sep = NULL,
	brackets = NULL,
	format = "compact",
	keep_attr = FALSE) {

	if(length(brackets) == 0) {brackets <- NULL}
	tree <- xml2::xml_find_all(x = child, xpath = xpath)
	if(length(tree) < 1) {return(data.table::data.table())}
	xp.child  <- xml2::xml_path(x = child)
	xp.remain <- xml2::xml_path(x = tree)
	xp.rel    <- substr(x = xp.remain, start = nchar(xp.child) + 2, stop = nchar(xp.remain))
	xp.cols   <- gsub(
		pattern = "/",
		replacement = ".",
		x = gsub(
			pattern = "@",
			replacement = "",
			x = unique(gsub(pattern = "\\[[0-9]+\\]", replacement = "", x = xp.rel))
		)
	)
	d <- lapply(seq_len(length(xp.cols)), function(dummy) character(0))
	names(d) <- xp.cols
	val <- xml2::xml_text(x = tree)
	s <- stringr::str_split(string = xp.rel, pattern = "/")
	o <- sapply(
		seq_along(s),
		function(i) {
			s. <- s[[i]]
			i.f <- !grepl("\\[[0-9]+\\]", s.)
			if (any(i.f)) {s.[i.f] <- paste0(s.[i.f], "[1]")}
			c(
				paste0(gsub(pattern = "]$",replacement = "", x = gsub(pattern = ".*\\[",replacement = "", x = s.[-length(s.)])), collapse = "."),
				gsub(pattern = "@", replacement = "", x = gsub(pattern = "\\[[0-9]+\\]", replacement = "", x = paste0(s., collapse = ".")))
			)
		}
	)
	if(!is.null(brackets)) {
		is_av_val <- ! is.na(val)
		o. <- o[1, ]
		val[is_av_val] <- paste0(brackets[1], o.[is_av_val], brackets[2], val[is_av_val])
	}
	for(col in xp.cols) {
		d[[col]] <- paste0(val[col == o[2, ]], collapse = sep)
	}
	result <- data.table::as.data.table(x = d)
	names(result) <- gsub(pattern = "(\\.\\w+)$", replacement = "", x = names(result))
	result
}

#' Extract columns
#'
#' Extracts defined values from a single resource
#'
#' @param child A xml child object, representing one FHIR resource.
#' @param df.columns A [fhir_columns-class] object describing which elements to extract
#' from the resource.
#' @param sep A string to separate pasted multiple entries.
#' @param brackets A character vector defining the brackets surrounding the indices. e.g. c( "<", ">").
#' `character(0)` means no brackets.
#' @param format A character of length 1 containing the format of the cracked table. Possible formats
#' are "compact" and "wide". Defaults to "compact"
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
#' result <- fhircrackr:::xtrct_columns(child = child, cols = cols)

xtrct_columns <- function(child, cols, sep = NULL, brackets = NULL, format = "compact") {
	if(length(brackets) == 0) {brackets <- NULL}
	xp <- xml2::xml_path(x = child)
	l <- lapply(
		lst(names(cols)),
		function(column.name) {
			i.srch <- cols[[column.name]]
			loc <- xml2::xml_find_all(x = child, xpath = i.srch)
			val <- xml2::xml_text(x = loc)
			if(!is.null(brackets)) {
				loc.xp <- xml2::xml_path(x = loc)
				loc.xp.rel <- substr(loc.xp, nchar(xp) + 2, nchar(loc.xp))
				s <- stringr::str_split(string = loc.xp.rel, pattern = "/")
				o <- sapply(
					seq_along(s),
					function(i) {
						s. <- s[[i]]
						i.f <- !grepl("\\[[0-9]+\\]", s.)
						if(any(i.f)) {s.[i.f] <- paste0(s.[i.f], "[1]")}
						gsub(".1$", "", paste0(gsub(pattern = "[^0-9]", replacement = "", x = s.), collapse = "."))
					}
				)
				if(0 < length(val)) {
					is_av <- ! is.na(val)
					paste0(brackets[1], o[is_av], brackets[2], val[is_av], collapse = sep)
				}
				else {NA}
			} else {
				if(0 < length(val)) {paste0(val, collapse = sep)} else {NA}
			}
		}
	)
	data.table::as.data.table(x = l)
}

#' Extracts one data frame out of one bundle
#' @param bundle A xml object containing one FHIR bundle
#' @param df_desc An object of class [fhir_table_description-class].
#' @param verbose An integer scalar.  If > 1, extraction progress will be printed. Defaults to 2.
#' @param format A character of length 1 containing the format of the cracked table. Possible formats
#' are "compact" and "wide". Defaults to "compact"
#' @param keep_attr A locigcal of length 1. Should column names be extended by the attribute names? Defaults to FALSE.
#' @param ncores Either NULL (1 core) or an integer of length 1 containing the number of
#'  cpu cores that should be used for cracking. Defaults to NULL.
#' @noRd
#' @examples
#' #unserialize example bundle
#' bundles <- fhir_unserialize(bundles = medication_bundles)
#'
#' #extract first bundle
#' bundle <- bundles[[1]]
#'
#' #define table_description
#' df_desc <- fhir_table_description(
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
#' result <- fhircrackr:::bundle2df(bundle = bundle, df_desc = df_desc)
bundle2df <- function(bundle, df_desc, verbose = 2, format = "compact", keep_attr = FALSE, ncores) {
	xpath <- paste0("//", df_desc@resource)
	children <- xml2::xml_find_all(x = bundle, xpath = xpath)
	ncores <- if(is.null(ncores)) 1 else min(c(ncores, length(children)))
	df.list <- if(length(children) == 0) {
		list()
	} else if(1 < ncores) {
		## does not work for 'Windows' because windows cannot fork
		parallel::mclapply(
			children,
			function(child) {
				if(0 < length(df_desc@cols)) {#if cols is not empty
					cols <- df_desc@cols
					res <- xtrct_columns(child = child, cols = cols, sep = df_desc@style@sep, brackets = df_desc@style@brackets, format = format)
				} else {#if cols empty
					xp <- ".//@*"
					res <- xtrct_all_columns(child = child, xpath = xp, sep = df_desc@style@sep, brackets = df_desc@style@brackets, format = format, keep_attr = keep_attr)
				}
				res
			},
			mc.cores = ncores
		)
	} else {
		lapply(
			children,
			function(child) {
			   	if(0 < length(df_desc@cols)) {#if cols is not empty
			   		cols <- df_desc@cols
			   		res <- xtrct_columns(child = child, cols = cols, sep = df_desc@style@sep, brackets = df_desc@style@brackets, format = format)
			   	} else {#if cols empty
			   		xp <- ".//@*"
			   		res <- xtrct_all_columns(child = child, xpath = xp, sep = df_desc@style@sep, brackets = df_desc@style@brackets, format = format, keep_attr = keep_attr)
			   	}
				res
			}
		)
	}
	data.table::rbindlist(l = df.list, fill = TRUE)
}

#' Convert several bundles to one data frame
#'
#' @param bundles A [fhir_bundle_list-class] object
#' @param df_desc A [fhir_df_desc-class] object
#' @param verbose An Integer Scalar.  If > 1, extraction progress will be printed. Defaults to 2.
#' @param format A character of length 1 containing the format of the cracked table. Possible formats
#' are "compact" and "wide". Defaults to "compact"
#' @param keep_attr A locigcal of length 1. Should column names be extended by the attribute names? Defaults to FALSE.
#' @param ncores Either NULL (1 core) or an integer of length 1 containing the number of
#'  cpu cores that should be used for cracking. Defaults to NULL.
#' @noRd
#' @examples
#' #unserialize example bundle
#' bundles <- fhir_unserialize(medication_bundles)
#'
#' df_desc <- fhir_table_description(
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
#' result <- fhircrackr:::bundles2df(bundles = bundles, df_desc = df_desc)

bundles2df <- function(bundles, df_desc, verbose = 2, format = "compact", keep_attr = FALSE, ncores) {
	ret <- data.table::rbindlist(
		lapply(
			seq_along(bundles),
			function(i) {
				if (1 < verbose) {message(paste0("Bundle ", i, ": "))}
				bundle <- bundles[[i]]
				bundle2df(bundle = bundle, df_desc = df_desc, verbose = verbose, format = format, keep_attr = keep_attr, ncores = ncores)
			}
		),
		fill = TRUE
	)
	if(nrow(0 < ret)) {ret <- ret[0 < rowSums(!is.na(ret)), ]}
	ret
}

#' Flatten list of FHIR bundles
#' @description Converts all FHIR bundles (the result of `fhir_search`) to a list of data frames.
#'
#' @param bundles A [fhir_bundle_list-class] object
#' @param design A [fhir_design-class] object
#' @param data.table Logical scalar. Return list of data.tables instead of data.frames? Defaults to FALSE.
#' @param verbose An Integer Scalar.  If > 1, extraction progress will be printed. Defaults to 2.
#' @param format A character of length 1 containing the format of the cracked table. Possible formats
#' are "compact" and "wide". Defaults to "compact"
#' @param keep_attr A locigcal of length 1. Should column names be extended by the attribute names? Defaults to FALSE.
#' @param ncores Either NULL (1 core) or an integer of length 1 containing the number of
#'  cpu cores that should be used for cracking. Defaults to NULL.
#' @noRd
#' @return A [fhir_df_list-class]/[fhir_dt_list-class] object as specified by `design`.
#'
#'
#' @examples
#' #unserialize example bundle
#' bundles <- fhir_unserialize(bundles = medication_bundles)
#'
#' #define attributes to extract
#' design <- fhir_design(
#'
#' 	 Medications = fhir_table_description(
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
#'  Patients = fhir_table_description(
#'
#' 		resource = "Patient"
#' 	)
#' )
#'
#' #convert fhir to data frames
#' list_of_tables <- fhircrackr:::bundles2dfs(bundles = bundles, design = design)

bundles2dfs <- function(bundles, design, data.table = FALSE, verbose = 2, format = "compact", keep_attr = FALSE, ncores = NULL) {

	dfs <- lapply(
		lst(names(design)),
		function(n) {
			df_desc <- design[[n]]
		  	if(is.null(df_desc)) {NULL} else {bundles2df(bundles = bundles, df_desc = df_desc, format = format, keep_attr = keep_attr, ncores = ncores, verbose = verbose)}
		}
	)
	#remove empty columns for all data.frames with rm_empty_cols=TRUE, keep others as is
	remove <- sapply(
		design,
		function(x) {x@style@rm_empty_cols}
	)
	dfs_cleaned <- lapply(
		seq_along(dfs),
		function(i) {
			if(remove[i] && 0 < ncol(dfs[[i]])) {
				dfs[[i]][, 0 < colSums(!is.na(dfs[[i]])), with = FALSE]
			} else {
				dfs[[i]]
			}
		}
	)
	names(dfs_cleaned) <- names(dfs)
	if(data.table) {
		fhir_dt_list(dt_list = dfs_cleaned, design = design)
	} else {
		fhir_df_list(lapply(dfs_cleaned, data.frame), design)
	}
}
