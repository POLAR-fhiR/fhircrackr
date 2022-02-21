## This file contains all functions needed for flattening ##
## Exported functions are on top, internal functions below ##

path <- node <- value <- attrib <- entry <- spath <- xpath <- column <- id <- NULL #To stop "no visible binding" NOTE in check()


#' Flatten list of FHIR bundles
#' @description Converts a [fhir_bundle_list-class] (the result of [fhir_search()] to a list of data.frames/data.tables,
#' i.e. a [fhir_df_list-class]/[fhir_dt_list-class] if a [fhir_design-class] is given in the argument `design`.
#' Creates a single data.frame/data.table, if only a [fhir_table_description-class] is given in the argument `design`.
#'
#' There are two main output formats for the table: compact and wide. They differ regarding their handling of multiple entries for
#' the same FHIR element (e.g. `Patient.adress`). In the compact format multiple entries are pasted together into one cell/column,
#' in the wide format multiple entries are distributed over several (indexed) columns. If none of the resources contains any multiple
#' values on the extracted elements, the two formats will result in the same basic structure.
#'
#' To increase speed with larger amounts of data the cracking process can be parallelised over a number of cores defined in the
#' `ncores` argument.
#'
#' @param bundles A FHIR search result as returned by [fhir_search()].
#' @param design A [fhir_design-class] or [fhir_table_description-class] object. See [fhir_design()]/[fhir_table_description()]
#' and the corresponding vignette (`vignette("flattenResources", package ="fhircrackr")`) for a more detailed explanation and
#' comprehensive examples of both.
#'
#' @param sep Optional. A character of length one containing the separator string used for separating multiple entries in cells when `format = "compact"`.
#' Will overwrite the `sep` defined in `design`. If `sep = NULL`, it is looked up in `design`, where the default is `":::"`.
#'
#' @param brackets Optional. A character of length one or two used for the indices of multiple entries, which will overwrite the `brackets` defined in `design`.
#' If `brackets = NULL`, it is looked up in `design`, where the default is `character(0)`,i.e. no indices are added to multiple entries.
#' Empty strings (`""`) are not allowed.
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
#' @param format Optional. A character of length one indicating whether the resulting table should be cracked to a `wide` or `compact` format. Will overwrite the `format` defined
#' in `design` which defaults to `compact`. `wide` means multiple entries will be distributed over several columns with indexed names. `compact` means multiple entries will be pasted into one cell/column separated by `sep`.
#'
#' @param keep_attr Optional. A logical of length one indicating whether the attribute name of the respective element (`@value` in most cases)
#' should be attached to the name of the variable in the resulting table. Will overwrite `keep_attr` in `design` which defaults to `FALSE`.
#'
#' @param ncores Either NULL (no parallelisation) or an integer of length 1 containing the number of
#'  cpu cores that should be used for parallelised cracking. Defaults to NULL.
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
#' - Dealing with multiple entries: [fhir_melt()], [fhir_cast()], [fhir_rm_indices()]
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
#'   sep           = " ",
#'   brackets      = c("[", "]"),
#'   rm_empty_cols = FALSE
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
		sep                     = NULL,
		brackets                = NULL,
		remove_empty_columns    = NULL,
		verbose                 = 2,
		data.table              = FALSE,
		format                  = NULL,
		keep_attr               = NULL,
		ncores                  = 1) {

		standardGeneric("fhir_crack")
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
		format               = NULL,
		keep_attr            = NULL,
		ncores               = 1) {

		#overwrite design with function arguments
		if(!is.null(sep)) {
			design@sep <- sep
		}

		if(!is.null(brackets)) {
			brackets <- fix_brackets(brackets = brackets)
			design@brackets <- brackets
		}

		if(!is.null(remove_empty_columns)) {
			design@rm_empty_cols <- remove_empty_columns
		}

		if(!is.null(format)) {
			design@format <- format
		}

		if(!is.null(keep_attr)) {
			design@keep_attr <- keep_attr
		}

		validObject(object = design, complete = TRUE)
		#Check for dangerous XPath expressions ins cols
		cols <- design@cols
		dangerCols <- sapply(cols, function(x) {any(grepl(esc("//"), x))})

		if(any(dangerCols)) {
			warning(
				"In the cols element of the table description, you specified XPath expressions containing '//' which point to an ",
				"arbitrary level in the resource. \nThis can result in unexpected behaviour, e.g. when the searched element appears ",
				"on different levels of the resource. \n", "We strongly advise to only use the fully specified relative XPath in the cols ",
				"element, e.g. 'ingredient/strength/numerator/code' instead of search paths like '//code'. \n",
				"This warning is thrown for the following data.frame descriptions: ", paste(names(cols)[dangerCols], collapse = ", ")
			)
		}

		df <- crack_bundles_to_one_table(bundles = bundles, table_description = design, data.table = data.table, ncores = ncores, verbose = verbose)

		if(0 < verbose) {cat('finished.\n')}

		assign(x = "canonical_design", value = design, envir = fhircrackr_env)

		df
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
		sep                     = NULL,
		brackets                = NULL,
		remove_empty_columns    = NULL,
		verbose                 = 2,
		data.table              = FALSE,
		format                  = NULL,
		keep_attr               = NULL,
		ncores                  = 1) {

		#overwrite design with function arguments
		if(!is.null(sep)) {
			design <- fhir_design(lapply(
				design,
				function(x) {
					x@sep <- sep
					x
				})
			)
		}

		if(!is.null(brackets)) {
			brackets <- fix_brackets(brackets = brackets)
			design <- fhir_design(
				lapply(
					design,
					function(x) {
						x@brackets <- brackets
						x
					}
				)
			)
		}

		if(!is.null(remove_empty_columns)) {
			design <- fhir_design(
				lapply(
					design,
					function(x) {
						x@rm_empty_cols <- remove_empty_columns
						x
					}
				)
			)
		}

		if(!is.null(format)) {
			design <- fhir_design(lapply(
				design,
				function(x) {
					x@format <- format
					x
				})
			)
		}

		if(!is.null(keep_attr)) {
			design <- fhir_design(lapply(
				design,
				function(x) {
					x@keep_attr <- keep_attr
					x
				})
			)
		}


		validObject(object = design, complete = TRUE)
		#Check for dangerous XPath expressions ins cols
		cols <- lapply(
			X   = design,
			FUN = function(x) {c(x@cols)}
		)
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

		if(length(bundles) < 1) {
			warning('No bundles present in bundles list \'bundles\'. NULL will be returned.')
			return(NULL)
		}

		if(length(design) < 1) {
			warning('No table descriptions present in design \'design\'. NULL will be returned.')
			return(NULL)
		}

		dfs <- crack_bundles_to_tables(bundles = bundles, design = design, data.table = data.table, ncores = ncores, verbose = verbose)

		#if(0 < verbose) {message("FHIR-Resources cracked. \n")}
		assign(x = "canonical_design", value = design, envir = fhircrackr_env)
		dfs
	}
)

##################################################################################################################################################


#' Create fhir_table_list from bundles and design
#' @param bundles A FHIR search result as returned by [fhir_search()].
#' @param design A [fhir_design-class] object. See [fhir_table_description()]
#' and the corresponding vignette (`vignette("flattenResources", package ="fhircrackr")`) for a more detailed explanation and
#' comprehensive examples of both.
#' @param data.table A logical vector of length one. If it is set to TRUE the fhir_crack-function returns a data.table, otherwise a data.frame.
#' Defaults to FALSE.
#' @param ncores Either NULL (no parallelisation) or an integer of length 1 containing the number of
#'  cpu cores that should be used for parallelised cracking. Defaults to NULL.
#' @param verbose An integer vector of length one. If 0, nothing is printed, if 1, only finishing message is printed, if > 1,
#' extraction progress will be printed. Defaults to 2.
#' @noRd

crack_bundles_to_tables <- function(bundles, design, data.table = FALSE, ncores = 1, verbose = 0) {
	ncores <- limit_ncores(ncores)
	tables <- lapply(
		X   = design,
		FUN = function(table_description) {
			crack_bundles_to_one_table(
				bundles           = bundles,
				table_description = table_description,
				ncores            = ncores,
				verbose           = verbose
			)
		}
	)

	if(data.table) {
		fhir_dt_list(dt_list = tables, design = design)
	} else {
		fhir_df_list(df_list = tables, design = design)
	}
}


#' Create DF/DT from bundles and fhir_table_description
#' @param bundles A FHIR search result as returned by [fhir_search()].
#' @param table_description A [fhir_table_description-class] object. See [fhir_table_description()]
#' and the corresponding vignette (`vignette("flattenResources", package ="fhircrackr")`) for a more detailed explanation and
#' comprehensive examples of both.
#' @param data.table A logical vector of length one. If it is set to TRUE the fhir_crack-function returns a data.table, otherwise a data.frame.
#' Defaults to FALSE.
#' @param ncores Either NULL (no parallelisation) or an integer of length 1 containing the number of
#'  cpu cores that should be used for parallelised cracking. Defaults to NULL.
#' @param verbose An integer vector of length one. If 0, nothing is printed, if 1, only finishing message is printed, if > 1,
#' extraction progress will be printed. Defaults to 2.
#' @noRd
crack_bundles_to_one_table <- function(bundles, table_description, data.table = FALSE, ncores = 1, verbose = 0) {
	os <- get_os()
	available_cores <- get_ncores(os)
	ncores <- limit_ncores(ncores)
	if(0 < verbose) cat(
		paste0(
			'Cracking ',
			length(bundles),
			' ',
			table_description@resource,
			's\' ',
			if(1 == length(bundles)) 'Bundle' else 'Bundles',
			' on a ',
			toupper(os),
			'-Mashine using ',
			ncores,
			'/',
			available_cores,
			if(1 == available_cores) ' CPU' else ' CPUs',
			' ... \n'
		)
	)

	table <- if(0 < length(table_description@cols)) {
		if(table_description@format == 'wide') {
			crack_wide_given_columns(bundles = bundles, table_description = table_description, ncores = ncores)
		} else {
			crack_compact_given_columns(bundles = bundles, table_description = table_description, ncores = ncores)
		}
	} else {
		if(table_description@format == 'wide') {
			crack_wide_all_columns(bundles = bundles, table_description = table_description, ncores = ncores)
		}
		else {
			crack_compact_all_columns(bundles = bundles, table_description = table_description, ncores = ncores)
		}
	}

	###Deal with resources/elements that were not found
	#given columns
	if(0 < length(table_description@cols)){
		#rm_emtpy_cols=FALSE
		if(!table_description@rm_empty_cols){
			#nothing was found. Return empty table with appropriate names
			if(nrow(table)==0){
				table <- data.table::setnames(data.table(matrix(nrow = 0, ncol = length(table_description@cols))), names(table_description@cols))
				warning("The resource type or columns you are looking for don't seem to be present in the bundles.\n ",
						"Returning an empty table.")
				#some items were found: Fill missing with NA
			}else{
				regexpr_ids <- paste0(esc(table_description@brackets[1]), "([0-9]+(\\.[0-9]+)*)", esc(table_description@brackets[2]))
				names <- unique(gsub(regexpr_ids, "", names(table)))
				empty_cols <- setdiff(names(table_description@cols), names)
				if(0 < length(empty_cols)){table[,(empty_cols):=NA]}
			}
			#rm_empty_cols=TRUE
		}else{
			if(nrow(table)==0){
				warning("The resource type or columns you are looking for don't seem to be present in the bundles.\n ",
						"Returning an empty table.")
			}
		}
		#set column order to order stated in table_description (only works for compact table)
		data.table::setcolorder(x = table, neworder = names(table_description@cols)[names(table_description@cols) %in% names(table)])

		#all columns
	}else{
		if(nrow(table)==0){warning("The resource type you are looking for (",table_description@resource ,") doesn't seem to be present in the bundles.\n ",
								   "Returning an empty table.")}
	}

	if(!data.table) {
		data.table::setDF(table)
	}

	table
}


#' Convert Bundles to a wide table when all elements should be extracted
#' @param bundles A fhir_bundle_list
#' @param table_description A fhir_table_description with an empty cols element
#' @param ncores The number of cores for parallelisation
#' @noRd
#'
crack_wide_all_columns <- function(bundles, table_description, ncores = 1) {
	if(2 == length(table_description@brackets)) {
		bra <- table_description@brackets[[1]]
		ket <- table_description@brackets[[2]]
	} else {
		stop("You need to provide brackets if you want to crack to format 'wide'")
	}
	result <- data.table::rbindlist(
		parallel::mclapply(
			seq_along(bundles),
			function(bundle_id) {
				#bundle_id <- 1
				d <- data.table(
					node   = xml2::xml_find_all(
						bundles[[bundle_id]],
						paste0('./entry/resource/', table_description@resource, '//@*')
					) # intermediate save entries
				)
				if(0 < nrow(d)){
					d <-(d[, path     := xml2::xml_path(node) |> busg('/Bundle/', '')|> busg('([^]])/', '\\1[1]/')] # add missing indices
						 [, value    := xml2::xml_text(node)] # get value
						 [, attrib   := path |> busg('.*@', '')] # get attribute
						 [, path     := path |> busg('@.*', '')] # remove attribute from path
						 [, entry    := path |> busg('entry\\[([0-9]+)].*', '\\1') |> as.integer()] # enumerate entry
						 [, spath    := path |> busg('^[^/]+/[^/]+/[^/]+/','')] # remove 'Bundle/entry/resource' from paths
						 [, id       := spath |> busg('[^0-9]+', '.') |> busg('(^\\.)|(\\.$)', '')] # extract ids
						 [, xpath    := spath |> busg('\\[[0-9]+]*/', '/') |> busg('\\/$', '')] # remove ids
						 [, column   := paste0(bra, id, ket, xpath) |>
						 		busg('/', '.') |>
						 		paste0(if(table_description@keep_attr) paste0('@', attrib) else '')
						 ] # create column name
						 [, -c('node', 'xpath', 'spath', 'attrib', 'id')]) # remove unnecessary columns

					d <- dcast(d, entry ~ column) # cast columns by bundle and entry
				}
				d
			},
			mc.cores = ncores
		),
		use.names = TRUE,
		fill = TRUE
	)
	if(nrow(result)==0){result}else{unique(result[, -c('entry')])}

}

#' Convert Bundles to a compact table when all elements should be extracted
#' @param bundles A fhir_bundle_list
#' @param table_description A fhir_table_description with an empty cols element
#' @param ncores The number of cores for parallelisation
#' @noRd
#'
crack_compact_all_columns <- function(bundles, table_description, ncores = 1) {
	use_indices <- FALSE
	if(2 == length(table_description@brackets)) {
		bra <- table_description@brackets[[1]]
		ket <- table_description@brackets[[2]]
		use_indices <- TRUE
	}
	unique(
		data.table::rbindlist(
			parallel::mclapply(
				seq_along(bundles),
				function(bundle_id) { #bundle_id <- 1
					d <- data.table(
						node   = xml2::xml_find_all(
							bundles[[bundle_id]],
							paste0('./entry/resource/', table_description@resource, '//@*')
						) # intermediate save entries
					)
					if(0 < nrow(d)){
						(d[, path     := xml2::xml_path(node) |> busg('/Bundle/', '')|> busg('([^]])/', '\\1[1]/')] # add missing indices
						 [, value    := xml2::xml_text(node)] # get value
						 [, attrib   := path |> busg('.*@', '')] # get attribute
						 [, path     := path |> busg('@.*', '')] # remove attribute from path
						 [, entry    := path |> busg('entry\\[([0-9]+)].*', '\\1') |> as.integer()] # enumerate entry
						 [, spath    := path |> busg('^[^/]+/[^/]+/[^/]+/','')] # remove 'Bundle/entry/resource' from paths
						 [, xpath    := spath |> busg('\\[[0-9]+]*/', '/') |> busg('\\/$', '')]
						 [, column   := paste0(
						 	xpath |> busg('/', '.') |> paste0(if(table_description@keep_attr) paste0('@', attrib) else '')
						 )])
						if(use_indices) {
							d <- (d[, id := spath |> busg('[^0-9]+', '.') |> busg('(^\\.)|(\\.$)', '')][, paste0(bra, id, ket, value, collapse = table_description@sep), by=c('entry', 'column')] |>
								  	dcast(entry ~ column, value.var = 'V1'))[,-c('entry')]
						} else {
							d <- (d[, paste0(value, collapse = table_description@sep), by=c('entry', 'column')] |> dcast(entry ~ column, value.var = 'V1'))[,-c('entry')]
						}
					}
					d
				},
				mc.cores = ncores
			),
			use.names = TRUE,
			fill = TRUE
		)
	)
}

#' Convert Bundles to a wide table when only some elements should be extracted
#' @param bundles A fhir_bundle_list
#' @param table_description A fhir_table_description with a non-empty cols element
#' @param ncores The number of cores for parallelisation
#' @noRd
#'
crack_wide_given_columns <- function(bundles, table_description, ncores = 1) {
	if(2 == length(table_description@brackets)) {
		bra <- table_description@brackets[[1]]
		ket <- table_description@brackets[[2]]
	} else {
		stop("You need to provide brackets if you want to crack to format 'wide'")
	}
	result <- data.table::rbindlist(
		parallel::mclapply(
			seq_along(bundles),
			function(bundle_id) {# bundle_id <- 1
				nodes <- xml_nodeset(
					unlist(
						recursive = FALSE,
						lapply(
							table_description@cols,
							function(xpath) {# xpath <- table_description@cols[[2]]
								xml2::xml_find_all(
									bundles[[bundle_id]],
									paste0('./entry/resource/', table_description@resource, '/', xpath, '/@*')
								)
							}
						)
					)
				)
				d <- data.table(
					node   = nodes # intermediate save entries
				)
				if(0 < nrow(d)){
					d <- (d[, path     := xml2::xml_path(node) |> busg('/Bundle/', '') |> busg('([^]])/', '\\1[1]/')] # add missing indices
						  [, value    := xml2::xml_text(node)] # get value
						  [, attrib   := path |> busg('.*@', '')] # get attribute
						  [, path     := path |> busg('@.*', '')] # remove attribute from path
						  [, entry    := path |> busg('entry\\[([0-9]+)].*', '\\1') |> as.integer()] # enumerate entry
						  [, spath    := path |> busg('^[^/]+/[^/]+/[^/]+/','')] # remove 'Bundle/entry/resource' from paths
						  [, id       := spath |> busg('[^0-9]+', '.') |> busg('(^\\.)|(\\.$)', '')] # extract ids
						  [, xpath    := spath |> busg('\\[[0-9]+]*/', '/') |> busg('\\/$', '')] # remove ids
						  [, column   := paste0(bra, id, ket, names(table_description@cols)[match(xpath, table_description@cols)]) |>
						  		busg('/', '.') |>
						  		paste0(if(table_description@keep_attr) paste0('@', attrib) else '')
						  ] # create column name
						  [, -c('node', 'xpath', 'spath', 'attrib', 'id')] # remove unnecessary columns
					)
					cols <- unique(d$column)
					d <- dcast(d, entry ~ column) # cast columns by bundle and entry
					data.table::setcolorder(x = d, neworder = cols)
				}
				d
			},
			mc.cores = ncores
		),
		use.names = TRUE,
		fill = TRUE
	)
	if(nrow(result)==0){result}else{unique(result[, -c('entry')])}
}

#' Convert Bundles to a compact table when only some elements should be extracted
#' @param bundles A fhir_bundle_list
#' @param table_description A fhir_table_description with a non-empty cols element
#' @param ncores The number of cores for parallelisation
#' @noRd
#'
crack_compact_given_columns <- function(bundles, table_description, ncores = 1) {
	use_indices <- FALSE
	bra <- ket <- ''
	if(2 == length(table_description@brackets)) {
		bra <- table_description@brackets[[1]]
		ket <- table_description@brackets[[2]]
		use_indices <- TRUE
	}
	unique(
		data.table::rbindlist(
			parallel::mclapply(
				seq_along(bundles),
				function(bundle_id) {# bundle_id <- 2
					nodes <- xml_nodeset(
						unlist(
							recursive = FALSE,
							lapply(
								table_description@cols,
								function(xpath) {# xpath <- table_description@cols[[2]]
									xml2::xml_find_all(
										bundles[[bundle_id]],
										paste0('./entry/resource/', table_description@resource, '/', xpath, '/@*')
									)
								}
							)
						)
					)
					d <- data.table(
						node   = nodes
					)
					if(0 < nrow(d)){
						(d[, path     := xml2::xml_path(node) |> busg('/Bundle/', '')|> busg('([^]])/', '\\1[1]/')] # add missing indices
						 [, value    := xml2::xml_text(node)] # get value
						 [, attrib   := path |> busg('.*@', '')] # get attribute
						 [, path     := path |> busg('@.*', '')] # remove attribute from path
						 [, entry    := path |> busg('entry\\[([0-9]+)].*', '\\1') |> as.integer()] # enumerate entry
						 [, spath    := path |> busg('^[^/]+/[^/]+/[^/]+/','')] # remove 'Bundle/entry/resource' from paths
						 [, xpath    := spath |> busg('\\[[0-9]+]*/', '/') |> busg('\\/$', '')]
						 [, column   := paste0(names(table_description@cols)[match(xpath, table_description@cols)]) |>
						 		busg('/', '.') |>
						 		paste0(if(table_description@keep_attr) paste0('@', attrib) else '')
						 ] # create column name
						)
						if(use_indices) {
							d <- (d[, id := spath |> busg('[^0-9]+', '.') |> busg('(^\\.)|(\\.$)', '')][, paste0(bra, id, ket, value, collapse = table_description@sep), by=c('entry', 'column')] |>
								  	dcast(entry ~ column, value.var = 'V1'))[,-c('entry')]
						} else {
							d <- (d[, paste0(value, collapse = table_description@sep), by=c('entry', 'column')] |> dcast(entry ~ column, value.var = 'V1'))[,-c('entry')]
						}
					}
					d
				},
				mc.cores = ncores
			),
			use.names = TRUE,
			fill = TRUE
		)
	)
}


