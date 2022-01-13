# unloadNamespace('fhircrackr')
# rm(list = ls())

add_missing_indices <- function(names, bra, ket, sep) {
	gsub(
		paste0('(\\w$)'),
		paste0('\\1', bra, '1', ket),
		gsub(
			paste0('(\\w)(', sep, ')'),
			paste0('\\1', bra, '1', ket, sep),
			names
	))
}

# add_missing_indices <- function(names, bra, ket, sep) {
# 	gsub(
# 		paste0('(\\w)', sep, '(\\w)'),
# 		paste0('\\1', bra, '1', ket, '\\.\\2'),
# 		gsub(
# 			'(\\w)@(\\w)',
# 			paste0('\\1', bra, '1', ket, '@\\2'),
# 			names
# 		)
# 	)
# }
#
replace_brackets <- function(names, bra, ket) {
	gsub('\\[', bra, gsub(']', ket, names))
}

bundles2table <- function(bundles, table_description, ncores = 1, verbose = 2) {

	xpath_resource <- paste0('//', table_description@resource)

	extract_compact_with_given_columns <- function(bundle) {
		#bundle <- bundles[[1]]
		resources <- xml2::xml_find_all(x = bundle, xpath = xpath_resource)
		data.table::rbindlist(
			use.names = TRUE,
			fill = TRUE,
			l = lapply(
				X   = resources,
				FUN = function(resource) {
					#resource <- resources[[1]]
					resource_xpath <- xml2::xml_path(resource)
					d <- data.table::data.table()
					d[,	unlist(
						recursive = FALSE,
						lapply(
							X   = seq_along(table_description@cols),
							FUN = function(column_id) {
								#column_id <- 2
								# resource_xpath <- gsub(
								# 	pattern     = paste0(table_description@resource, '$'),
								# 	replacement = '',
								# 	x = xml2::xml_path(resource)
								# )
								column_name <- names(table_description@cols)[[column_id]]
								nodes <- xml2::xml_find_all(x = resource, xpath = table_description@cols[[column_id]])
								values <- xml2::xml_attrs(nodes)
								paths <- xml2::xml_path(nodes)
								paths <- gsub(
									pattern     = esc(resource_xpath),
									replacement = '',
									x           = paths
								)

								# if(0 < length(table_description@brackets)) {
								# 	replace_brackets(names = paths_with_indices, bra = '<', ket = '>')
								# }
								# if(table_description@keep_attr) {
								# 	attribs <- unique(sapply(values, names))
								# 	if(1 < length(unique(attribs))) {
								# 		attribs <- attribs[[1]]
								# 		warning(
								# 			paste0(
								# 				'The names of the attributes are not unique for tag ',
								# 				table_description@cols[[column_id]],
								# 				'. Using the first one \'', attribs, '\' for Column Name Indexing.'
								# 			)
								# 		)
								# 	}
								# 	paths_with_indices, bra = '<', ket = '>')
								# }

								values_combinded <- if(0 < length(table_description@brackets)) {
									column_indices <- gsub(
										pattern     = '\\.$',
										replacement =  '',
										x           = gsub(
											pattern     = '([^0-9\\.]+)',
											replacement =  '',
											x           =  gsub(
												pattern     = '([0-9]+)',
												replacement = '\\1\\.',
												x           = add_missing_indices(
													names = paths,
													bra   = '[',
													ket   = ']',
													sep   =  '/'
												)
											)
										)
									)
									paste0(
										table_description@brackets[1],
										column_indices,
										table_description@brackets[2],
										values,
										collapse = table_description@sep
									)
								} else {
									paste0(values, collapse = table_description@sep)
								}
								l <- list(x = values_combinded)
								names(l) <- if(table_description@keep_attr) {
									attribs <- unique(sapply(values, names))
									if(1 < length(unique(attribs))) {
										attribs <- attribs[[1]]
										warning(
											paste0(
												'The names of the attributes are not unique for tag ',
												table_description@cols[[column_id]],
												'. Using the first one \'', attribs, '\' for Column Name Indexing.'
											)
										)
									}
									paste0(column_name, '@', attribs)
								} else column_name
								l
							}
						)
					)]
				}
			)
		)
	}

	extract_wide_with_given_columns <- function(bundle) {
		#bundle <- bundles[[2]]
		resources <- xml2::xml_find_all(x = bundle, xpath = xpath_resource)
		data.table::rbindlist(
			use.names = TRUE,
			fill      = TRUE,
			l         = lapply(
				X   = resources,
				FUN = function(resource) {
					#resource <- resources[[1]]
					resource_xpath <- xml2::xml_path(resource)
					d <- data.table::data.table()
					d[,	unlist(
						recursive = FALSE,
						lapply(
							X   = seq_along(table_description@cols),
							FUN = function(column_id) {
								#column_id <- 2
								# resource_xpath <- gsub(
								# 	pattern     = paste0(table_description@resource, '$'),
								# 	replacement = '',
								# 	x = xml2::xml_path(resource)
								# )
								column_name <- names(table_description@cols)[[column_id]]
								nodes <- xml2::xml_find_all(x = resource, xpath = table_description@cols[[column_id]])
								values <- xml2::xml_attrs(nodes)
								paths <- xml2::xml_path(nodes)
								paths <- gsub(
									pattern     = paste0(esc(resource_xpath), '/'),
									replacement = '',
									x           = paths
								)
								paths_with_indices <- add_missing_indices(
									names = paths,
									bra   = '[',
									ket   = ']',
									sep   = '/'
								)
								paths_with_indices <- if(0 < length(table_description@brackets)) {
									paste0(
										table_description@brackets[1],
										gsub(
											pattern     = '\\.$',
											replacement =  '',
											x           = gsub(
												pattern     = '([^0-9\\.]+)',
												replacement =  '',
												x           =  gsub(
													pattern     = '([0-9]+)',
													replacement = '\\1\\.',
													x           = paths_with_indices
												)
											)
										),
										table_description@brackets[2],
										column_name
									)
								}
								paths_with_indices <- gsub(
									pattern     = '/',
									replacement = '\\.',
									x           = paths_with_indices
								)

								if(table_description@keep_attr) {
									attribs <- unique(sapply(values, names))
									if(1 < length(unique(attribs))) {
										attribs <- attribs[[1]]
										warning(
											paste0(
												'The names of the attributes are not unique for tag ',
												table_description@cols[[column_id]],
												'. Using the first one \'', attribs, '\' for Column Name Indexing.'
											)
										)
									}
									paths_with_indices <- paste0(paths_with_indices, '@', attribs)
								}

								names(values) <- paths_with_indices
								values
							}
						)
					)]
				}
			)
		)
	}

	d <- if(0 < length(table_description@cols)) {   # GIVEN COLUMNS
		if(table_description@format == 'compact') { # GIVEN COLUMNS - COMPACT
			data.table::rbindlist(
				use.names = TRUE,
				fill      = TRUE,
				l         = if(ncores == 1) {
					lapply(
						X   = bundles,
						FUN = extract_compact_with_given_columns
					)
				} else {
					parallel::mclapply(
						mc.cores = ncores,
						X        = bundles,
						FUN      = extract_compact_with_given_columns
					)
				}
			)
		} else {                                    # GIVEN COLUMNS - WIDE
			data.table::rbindlist(
				use.names = TRUE,
				fill      = TRUE,
				l         = if(ncores == 1) {
					lapply(
						X   = bundles,
						FUN = extract_wide_with_given_columns
					)
				} else {
					parallel::mclapply(
						mc.cores = ncores,
						X        = bundles,
						FUN = extract_wide_with_given_columns
					)
				}
			)
		}
	} else {                                        # ALL COLUMNS
		if(table_description@format == 'compact') { # ALL COLUMNS - COMPACT
			data.table::rbindlist(
				use.names = TRUE,
				fill      = TRUE,
				l         = if(ncores == 1) {
					lapply(
						X   = bundles,
						FUN = function(bundle, table_description, vebose = verbose) {

						}
					)
				} else {
					parallel::mclapply(
						mc.cores = ncores,
						X        = bundles,
						FUN      = function(bundle, table_description, vebose = verbose) {

						}
					)
				}
			)
		} else {                                    # ALL COLUMNS - WIDE
			data.table::rbindlist(
				use.names = TRUE,
				fill      = TRUE,
				l         = if(ncores == 1) {
					lapply(
						X   = bundles,
						FUN = function(bundle, table_description, vebose = verbose) {

						}
					)
				} else {
					parallel::mclapply(
						mc.cores = ncores,
						X        = bundles,
						FUN      = function(bundle, table_description, vebose = verbose) {

						}
					)
				}
			)
		}
	}
	if(table_description@rm_empty_cols) {
		d <- d[, 0 < colSums(!is.na(d)), with = FALSE]
	}
	d[]
}

# bundles2tables <- function(bundles, design, ncores = 1, split_threads_on_bundle = TRUE, verbose = 2) {
#
# 	#######################################################################
# 	if(length(bundles) < 1) {
# 		warning('No bundles present in bundles list \'bundles\'. NULL will be returned.')
# 		return(NULL)
# 	}
#
# 	if(length(design) < 1) {
# 		warning('No table descriptions present in design \'design\'. NULL will be returned.')
# 		return(NULL)
# 	}
#
# 	if(is.null(ncores) || is.na(ncores) || ncores < 1) ncores <- 1
# 	os <- get_os()
# 	available_cores <- get_ncores(os)
# 	ncores <- min(c(available_cores, ncores))
# 	message(
# 		paste0(
# 			'Cracking under OS ',
# 			os,
# 			' having ',
# 			available_cores,
# 			if(1 < available_cores) " cores" else " core",
# 			' available using ', ncores, if(1 < ncores) ' cores.' else ' core.'
# 		)
# 	)
#
# 	lapply(
# 		X   = design,
# 		FUN = function(table_description) {
# 			#table_description <- design[[2]]
# 			bundles2table(bundles = bundles, table_description = table_description, ncores = ncores, verbose = verbose)
# 		}
# 	)
# }

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
		format                  = "compact",
		keep_attr               = FALSE,
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
		format               = "compact",
		keep_attr            = FALSE,
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

		os <- get_os()
		available_cores <- get_ncores(os)
		ncores <- if(is.null(ncores)) 1 else min(c(available_cores, ncores))
		message(
			paste0(
				'Cracking under OS ',
				os,
				' having ',
				available_cores,
				if(1 < available_cores) " cores" else " core",
				' available using ', ncores, if(1 < ncores) ' cores.' else ' core.'))

		df <- bundles2table(bundles = bundles, table_description = design, verbose = verbose, ncores = ncores)

		if(design@rm_empty_cols && 0 < ncol(df)) {
			df <- df[, 0 < colSums(!is.na(df)), with = FALSE]
		}

		if(0 < verbose) {message("FHIR-Resources cracked.")}

		assign(x = "canonical_design", value = design, envir = fhircrackr_env)

		if(data.table) {df} else {data.frame(df)}
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
		format                  = "compact",
		keep_attr               = FALSE,
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
		os <- get_os()
		available_cores <- get_ncores(os)
		ncores <- min(c(available_cores, ncores))
		message(
			paste0(
				'Cracking under OS ',
				os,
				' having ',
				available_cores,
				if(1 < available_cores) " cores" else " core",
				' available using ', ncores, if(1 < ncores) ' cores.' else ' core.'
			)
		)

		dfs <- lapply(
			X   = design,
			FUN = function(table_description) {
				#table_description <- design[[2]]
				fhir_crack(bundles = bundles, design = table_description, ncores = ncores, verbose = verbose)
			}
		)

		if(0 < verbose) {message("FHIR-Resources cracked. \n")}
		assign(x = "canonical_design", value = design, envir = fhircrackr_env)
		dfs
	}
)
