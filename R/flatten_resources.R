crack_wide_all_columns <- function(bundles, table_description, ncores = 1) {
	#ncores <- limit_ncores(ncores)
	unique(
		data.table::rbindlist(
			parallel::mclapply(
				seq_along(bundles),
				function(bundle_id) {
					#bundle_id <- 1
					(data.table(
						bundle = as.integer(bundle_id), #enumerate bundles
						node   = xml2::xml_find_all(
							bundles[[bundle_id]],
							paste0('./entry/resource/', table_description@resource, '//@*')
						) # intermediate save entries
					)
					[, path     := xml2::xml_path(node) |> busg('/Bundle/', '')|> busg('([^]])/', '\\1[1]/')] # add missing indices
					[, value    := xml2::xml_text(node)] # get value
					[, attrib   := path |> busg('.*@', '')] # get attribute
					[, path     := path |> busg('@.*', '')] # remove attribute from path
					[, entry    := path |> busg('entry\\[([0-9]+)].*', '\\1') |> as.integer()] # enumerate entry
					[, spath    := path |> busg('^[^/]+/[^/]+/[^/]+/','')] # remove 'Bundle/entry/resource' from paths
					[, id       := spath |> busg('[^0-9]+', '.') |> busg('(^\\.)|(\\.$)', '')] # extract ids
					[, xpath    := spath |> busg('\\[[0-9]+]*/', '/') |> busg('\\/$', '')] # remove ids
					[, column   := paste0('[', id, ']', xpath) |>
							busg('/', '.') |>
							paste0(if(table_description@keep_attr) paste0('@', attrib) else '')
					] # create column name
					[, -c('node', 'xpath', 'spath', 'attrib', 'id')] # remove unnecessary colums
					) |> dcast(bundle + entry ~ column) # cast columns by bundle and entry
				},
				mc.cores = ncores
			),
			use.names = TRUE,
			fill = TRUE
		)[, -c('bundle', 'entry')]
	)
}
crack_wide_given_columns <- function(bundles, table_description, ncores = 1) {
	unique(
		data.table::rbindlist(
			parallel::mclapply(
				seq_along(bundles),
				function(bundle_id) {
					#bundle_id <- 260
					nodes <- xml2:::xml_nodeset(
						unlist(
							recursive = F,
							lapply(
								table_description@cols,
								function(xpath) {
									#col <- table_description@cols[[1]]
									xml2::xml_find_all(
										bundles[[bundle_id]],
										paste0('./entry/resource/', table_description@resource, '/', xpath, '/@*')
									)
								}
							)
						)
					)
					(data.table(
						bundle = as.integer(bundle_id), #enumerate bundles
						node   = nodes # intermediate save entries
					)
						[, path     := xml2::xml_path(node) |> busg('/Bundle/', '') |> busg('([^]])/', '\\1[1]/')] # add missing indices
						[, value    := xml2::xml_text(node)] # get value
						[, attrib   := path |> busg('.*@', '')] # get attribute
						[, path     := path |> busg('@.*', '')] # remove attribute from path
						[, entry    := path |> busg('entry\\[([0-9]+)].*', '\\1') |> as.integer()] # enumerate entry
						[, spath    := path |> busg('^[^/]+/[^/]+/[^/]+/','')] # remove 'Bundle/entry/resource' from paths
						[, id       := spath |> busg('[^0-9]+', '.') |> busg('(^\\.)|(\\.$)', '')] # extract ids
						[, xpath    := spath |> busg('\\[[0-9]+]*/', '/') |> busg('\\/$', '')] # remove ids
						[, column   := paste0('[', id, ']', names(table_description@cols)[match(xpath, table_description@cols)]) |>
								busg('/', '.') |>
								paste0(if(table_description@keep_attr) paste0('@', attrib) else '')
						] # create column name
						[, -c('node', 'xpath', 'spath', 'attrib', 'id')] # remove unnecessary colums
					) |> dcast(bundle + entry ~ column) # cast columns by bundle and entry
				},
				mc.cores = ncores
			),
			use.names = TRUE,
			fill = TRUE
		)[, -c('bundle', 'entry')]
	)
}
convert_wide_table_to_a_compact_one <- function(wide, table_description, ncores = Inf) {
	ncores <- limit_ncores(ncores)
	pastl <- function(l, pre = '[', ids, suf = ']', sep = ' ~ ') {
		pastv <- function(v, pre = '[', ids, suf = ']', sep = ' ~ ') {
			len <- length(v)
			pre <- rep_len(pre, len)
			ids <- rep_len(ids, len)
			suf <- rep_len(suf, len)
			flt <- !is.na(v)
			paste0(pre[flt], ids[flt], suf[flt], v[flt], collapse = sep)
		}
		apply(
			X      = l,
			MARGIN = 1,
			FUN    = pastv,
			pre    = pre,
			ids    = ids,
			suf    = suf,
			sep    = sep
		)
	}
	#ncores <- limit_ncores(ncores)
	names <- names(wide)
	ids <- names |> busg('(^\\[)|(].+$)', '')
	short_names <- names |> busg('^.+]', '')
	unique_short_names <- unique(short_names)
	sep <- table_description@sep
	if(0 < length(table_description@brackets)) {
		bra <- table_description@brackets[[1]]
		ket <- table_description@brackets[[2]]
	} else {
		bra <- ''
		ket <- ''
		ids <- ''
	}
	d <- parallel::mclapply(
		X        = lst(unique_short_names),
		FUN      = function(unique_short_name) {
			#unique_short_name <- lst(unique_short_names)[[2]]
			i <- which(unique_short_name == short_names)
			pastl(l = wide[,..i,with=TRUE], pre = bra, ids = ids, suf = ket, sep = sep)
		},
		mc.cores = ncores
	)
	as.data.table(d)
	#cbind(wide[,c('bundle', 'entry')], as.data.table(d))
}
convert_wide_tables_to_compact_ones <- function(wide, design, ncores = Inf) {
	ncores <- limit_ncores(ncores)
	lapply(
		X   = design,
		FUN = function(d) {
			convert_wide_table_to_a_compact_one(
				wide              = wide[[paste0(d@resource, 's')]],
				table_description = d,
				ncores            = ncores
			)
		}
	)
}
crack_bundles_to_one_table <- function(bundles, table_description, ncores = Inf, verbose = 0) {
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
		crack_wide_given_columns(bundles = bundles, table_description = table_description, ncores = ncores)
	} else {
		crack_wide_all_columns(bundles = bundles, table_description = table_description, ncores = ncores)
	}
	if(table_description@format == 'compact') {
		table <- convert_wide_table_to_a_compact_one(
			wide              = table,
			table_description = table_description,
			ncores            = ncores
		)
	}
	#unique(table[,lapply(.SD, function(x)x[[1]]), by = setdiff(names(table), c('bundle', 'entry'))])
	table
}
crack_bundles_to_tables <- function(bundles, design, ncores = Inf, verbose = 0) {
	ncores <- limit_ncores(ncores)
	lapply(
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
}

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
		ncores                  = NULL) {

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
		ncores               = NULL) {

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

		df <- crack_bundles_to_one_table(bundles = bundles, table_description = design, ncores = ncores, verbose = verbose)
		#df <- bundles2table(bundles = bundles, table_description = design, ncores = ncores, verbose = verbose)

		if(design@rm_empty_cols && 0 < ncol(df)) {
			df <- df[, 0 < colSums(!is.na(df)), with = FALSE]
		}

		#df <- unique(df[,lapply(.SD, function(x)x[[1]]),by=setdiff(names(df), c('bundle', 'entry'))])

		if(0 < verbose) {cat('finished.\n')}

		assign(x = "canonical_design", value = design, envir = fhircrackr_env)

		if(data.table) {
			data.table::setDF(df)
		}
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
		ncores                  = NULL) {

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

		#######################################################################
		if(length(bundles) < 1) {
			warning('No bundles present in bundles list \'bundles\'. NULL will be returned.')
			return(NULL)
		}

		if(length(design) < 1) {
			warning('No table descriptions present in design \'design\'. NULL will be returned.')
			return(NULL)
		}

		if(is.null(ncores) || is.na(ncores) || ncores < 1) ncores <- 1
		# os <- get_os()
		# available_cores <- get_ncores(os)
		# ncores <- min(c(available_cores, ncores))
		# if(0 < verbose) message(
		# 	paste0(
		# 		'Cracking ',
		# 		length(bundles),
		# 		if(1 < length(bundles)) ' bundles.' else ' bundle.'
		# 	)
		# )

		# dfs <- lapply(
		# 	X   = design,
		# 	FUN = function(table_description) {
		# 		#table_description <- design[[2]]
		# 		fhir_crack(
		# 			bundles              = bundles,
		# 			design               = table_description,
		# 			ncores               = ncores,
		# 			sep                  = sep,
		# 			brackets             = brackets,
		# 			remove_empty_columns = remove_empty_columns,
		# 			verbose              = verbose,
		# 			data.table           = data.table,
		# 			format               = format,
		# 			keep_attr            = keep_attr
		# 		)
		# 	}
		# )
		dfs <- crack_bundles_to_tables(bundles = bundles, design = design, ncores = ncores, verbose = verbose)

		#if(0 < verbose) {message("FHIR-Resources cracked. \n")}
		assign(x = "canonical_design", value = design, envir = fhircrackr_env)
		dfs
	}
)
