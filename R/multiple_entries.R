## This file contains all functions dealing with multiple entries/indices##
## Exported functions are on top, internal functions below ##

#' Cast table with multiple entries
#' This function divides multiple entries in a compact indexed table as produced by [fhir_crack()] into separate columns.
#'
#' This function turns a table from compact format into wide format. Every column containing multiple entries will be turned into multiple columns.
#' The number of columns created from a single column in the original table is determined by the maximum number of
#' multiple entries occurring in this column. Rows with less than the maximally occurring number of entries will
#' be filled with NA values.
#'
#' For [fhir_cast()] to work properly, column names of the input data must reflect the Xpath to the corresponding resource element
#' with `.` as a separator, e.g. `code.coding.system`. These names are produced automatically by [fhir_crack()]
#' when the names are not explicitly set in the `cols` element of the [fhir_table_description()]/[fhir_design()].
#'
#' In the names of the newly created columns the indices will be added in front of the column names, similar to the result of [fhir_crack()] with
#' `format="wide"`. See examples and the corresponding package vignette for a more detailed description.
#'
#' @param indexed_df A compact data.frame/data.table with indexed multiple entries. Column names should reflect the XPath expression of the respective element.
#' @param brackets A character vector of length two, defining the brackets used for the indices.
#' @param sep A character vector of length one defining the separator that was used when pasting together multiple entries in [fhir_crack()].
#' @param verbose An integer vector of length one. If 0, nothing is printed, if 1, only general progress is printed, if > 1,
#' progress for each variable is printed. Defaults to 1.
#' @export
#' @examples
#'
#' #unserialize example
#' bundles <- fhir_unserialize(bundles = example_bundles1)
#'
#' #crack fhir resources
#' table_desc <- fhir_table_description(
#'     resource = "Patient",
#'     brackets = c('[', ']'),
#'     sep      = " ",
#'     keep_attr=TRUE
#' )
#' df <- fhir_crack(bundles = bundles, design = table_desc)
#'
#' #original df
#' df
#'
#' #cast
#' fhir_cast(df, brackets=c('[', ']'), sep = ' ', verbose = 0)
#'
#' @seealso [fhir_crack()], [fhir_melt()], [fhir_build_bundle()]
#'
fhir_cast <- function(
		indexed_df,
		brackets,
		sep,
		verbose = 1) {

	if (is.null(indexed_df)) stop("indexed_df is NULL.")
	if (nrow(indexed_df) < 1) stop("indexed_df doesn't contain any data.")

	is_DT <- data.table::is.data.table(x = indexed_df)
	if (!is_DT) data.table::setDT(x = indexed_df)

	col_names <- names(indexed_df)
	sep_ <- esc(sep)
	brackets <- fix_brackets(brackets)
	bra_ <- esc(brackets[1])
	ket_ <- esc(brackets[2])
	regexpr_ids <- stringr::str_c(bra_, "([0-9]+(\\.[0-9]+)*)", ket_, "(.*$)")

	if (!any(grepl(regexpr_ids, indexed_df[1, ]))) {
		stop("Cannot find ids with the specified brackets in the table.")
	}

	if (0 < verbose) message("Expanding table...\n")

	map <- sapply(
		col_names,
		function(name) {
			#name <- names(indexed_df)[[6]]
			if (1 < verbose) {message(name)}

			warning_given <- FALSE
			entries <- strsplit(indexed_df[[name]], sep_)
			ids <- lapply(entries, function(entry) gsub(regexpr_ids, "\\1", entry))
			name_vec <- strsplit(name, "\\.")[[1]]

			name_expanded <- unlist(
				lapply(
					ids[sapply(ids, function(i) all(!is.na(i)))],
					function(id) {
						#id <- ids[sapply(ids, function(i) all(!is.na(i)))][[1]]
						id_ <- strsplit(id, "\\.")
						names(id_) <- id

						if (length(id_[[1]]) != length(name_vec) && !warning_given) {
							warning(
								"Column name '", stringr::str_c(name_vec, collapse = "."),
								"' doesn't fit the id pattern found in this column.",
								"The column name should be build the way ",
								"fhir_crack() automatically builds it. See ?fhir_cast."
							)
							warning_given <<- TRUE
						}

						if (1 < length(name_vec)) {
							sapply(
								id_,
								function(i_) {
									i_ <- as.numeric(i_)
									stringr::str_c(stringr::str_c(brackets[1],paste(i_, collapse="."), brackets[2]), paste(name_vec, collapse = "."))

								},
								simplify = FALSE
							)
						} else {
							i <- as.numeric(id)
							a <- stringr::str_c(brackets[1], i, brackets[2], name_vec)
							names(a) <- id
							a
						}
					}
				),
				use.names = T
			)
			sort(name_expanded[unique(names(name_expanded))])
		},
		simplify = FALSE
	)

	df_new_names <- unlist(map, use.names = FALSE)
	d <- data.table(matrix(data = rep_len(character(), nrow(indexed_df) * length(df_new_names)), nrow = nrow(indexed_df), ncol = length(df_new_names)))
	setnames(d, df_new_names)

	if (0 < verbose) message("\nFilling table...\n")

	for (name in names(map)) {
		#name <- names(map)[[1]]
		if (1 < verbose) message(name, ":")

		for (id in names(map[[name]])) {
			#id <- names(map[[name]])[[1]]
			sname <- map[[name]][[id]]
			if (1 < verbose) message("   ", sname)
			id_str <- stringr::str_c(bra_, id, ket_)
			row_with_id <- grep(id_str, indexed_df[[name]], perl = TRUE)
			entries <- strsplit(indexed_df[[name]][row_with_id], sep_)
			values <- gsub(
				id_str,
				"",
				sapply(
					entries,
					function(entry) {
						entry[grep(id_str, entry, perl = TRUE)]
					},
					simplify = FALSE
				)
			)
			d[row_with_id, (sname) := values]
		}
	}
	if (!is_DT) data.table::setDF(d)
	d[]#to avoid problems with printing
	d
}

#' Find common columns
#'
#' This is a convenience function to find all column names in a data frame starting with the same string that can
#' then be used for [fhir_melt()].
#'
#' It is intended for use on data frames with column names that have been automatically produced by [fhir_design()]/[fhir_crack()]
#' and follow the form `level1.level2.level3` such as `name.given` or `code.coding.system`.
#' Note that this function will only work on column names following exactly this scheme.
#'
#' The resulting character vector can be used for melting all columns belonging to the same attribute in an indexed data frame,
#' see `?fhir_melt`.
#'
#' @param data_frame A data.frame/data.table with automatically named columns as produced by [fhir_crack()].
#' @param column_names_prefix A string containing the common prefix of the desired columns.
#' @return A character vector with the names of all columns matching `column_names_prefix`.
#' @examples
#' #unserialize example bundles
#' bundles <- fhir_unserialize(bundles = medication_bundles)
#'
#' #crack Patient Resources
#' pats <- fhir_table_description(resource = "Patient")
#'
#' df <- fhir_crack(bundles = bundles, design = pats)
#'
#' #look at automatically generated names
#' names(df)
#'
#' #extract all column names beginning with the string "name"
#' fhir_common_columns(data_frame = df, column_names_prefix = "name")
#' @export
#' @seealso [fhir_melt()], [fhir_rm_indices()]
#'
fhir_common_columns <- function(data_frame, column_names_prefix) {
	if (!is.data.frame(data_frame)) {
		stop(
			"You need to supply a data.frame or data.table to the argument indexed_data_frame.",
			"The object you supplied is of type ", class(data_frame), "."
		)
	}
	pattern_column_names  <- stringr::str_c("^", column_names_prefix, "($|\\.+)")
	column_names <- names(data_frame)
	hits <- grepl(pattern_column_names, column_names)
	if (!any(hits)) stop("The column prefix you gave doesn't appear in any of the column names.")
	column_names[hits]
}


#' Melt multiple entries
#'
#' This function divides multiple entries in an indexed data frame as produced by [fhir_crack()]
#' into separate rows.
#'
#' Every row containing values that consist of multiple entries on the variables specified by the argument `columns`
#' will be turned into multiple rows, one for each entry. Values on other variables will be repeated in all the new rows.
#'
#' The new data.frame will contain only the molten variables (if `all_cloumns = FALSE`) or all variables
#' (if `all_columns = TRUE`) as well as an additional variable `resource_identifier` that maps which rows came
#' from the same origin. The name of this column can be changed in the argument `id_name`.
#'
#' For a more detailed description on how to use this function please see the corresponding package vignette.
#'
#' @param indexed_data_frame A data.frame/data.table with indexed multiple entries.
#' @param columns A character vector specifying the names of all columns that should be molten simultaneously.
#' It is advisable to only melt columns simultaneously that belong to the same (repeating) attribute!
#' @param brackets A character vector of length two, defining the brackets used for the indices.
#' @param sep A character vector of length one defining the separator that was used when pasting together multiple entries in [fhir_crack()].
#' @param id_name A character vector of length one, the name of the column that will hold the identification of the origin of the new rows.
#' @param all_columns Return all columns? Defaults to `FALSE`, meaning only those specified in `columns` are returned.
#'
#' @return A data.frame/data.table where each entry from the variables in `columns` appears in a separate row.
#'
#' @examples
#' #unserialize example
#' bundles <- fhir_unserialize(bundles = example_bundles1)
#'
#' #crack fhir resources
#' table_desc <- fhir_table_description(
#'     resource = "Patient",
#'     brackets = c("[", "]"),
#'     sep = " "
#' )
#'
#' df <- fhir_crack(bundles = bundles, design = table_desc)
#'
#' #find all column names associated with attribute address
#' col_names <- fhir_common_columns(df, "address")
#'
#' #original data frame
#' df
#'
#' #only keep address columns
#' fhir_melt(
#'      indexed_data_frame = df,
#'      columns            = col_names,
#'      brackets           = c("[", "]"),
#'      sep = " "
#'  )
#'
#' #keep all columns
#' fhir_melt(indexed_data_frame = df, columns = col_names,
#'           brackets = c("[","]"), sep = " ", all_columns = TRUE)
#' @export
#' @seealso [fhir_common_columns()], [fhir_rm_indices()]
#'
fhir_melt <- function(
		indexed_data_frame,
		columns,
		brackets = c("<", ">"),
		sep = " ",
		id_name = "resource_identifier",
		all_columns = FALSE) {

	if (!is.data.frame(indexed_data_frame)) {
		stop(
			"You need to supply a data.frame or data.table to the argument indexed_data_frame.",
			"The object you supplied is of type ", class(indexed_data_frame), "."
		)
	}

	if (!all(columns %in% names(indexed_data_frame))) {
		stop("Not all column names you gave match with the column names in the data frame.")
	}

	table <- copy(indexed_data_frame) #copy to avoid side effects
	is_DT <- data.table::is.data.table(x = table)
	if (!is_DT) data.table::setDT(x = table)
	brackets <- fix_brackets(brackets = brackets)

	table <- fhir_melt_internal(indexed_dt = table, columns = columns, brackets = brackets,
								sep = sep, id_name = id_name, all_columns = all_columns)

	if (nrow(table) == 0) warning("The brackets you specified don't seem to appear in the indices of the provided data.frame. Returning NULL.")

	if (!is_DT) data.table::setDF(table)

	return(table)
}


#' Melt all columns with multiple entries
#'
#' This function divides all multiple entries in an indexed data frame as produced by [fhir_crack()]
#' into separate rows.
#'
#' The function repeatedly calls [fhir_melt()] on groups of columns that belong to the same
#' FHIR element (e.g. `address.city`, `address.country` and `address.type`) until every cell contains a single value.
#' If there is more than one FHIR element with multiple values (e.g. multiple address elements and multiple name
#' elements), every possible combination of the two elements will appear in the resulting table.
#' Caution! This creates something like a cross product of all values and can multiply the number of rows from the original
#' table considerably.
#'
#' @param indexed_data_frame A data.frame/data.table with indexed multiple entries.
#' @param brackets A character vector of length two, defining the brackets used for the indices.
#' @param sep A character vector of length one defining the separator that was used when pasting together multiple entries in [fhir_crack()].
#' @param column_name_separator A character string that separates element levels column names. Defaults to ".", which is used when
#' column names were generated automatically with [fhir_crack()].
#'
#' @return A completely molten data.table.
#'
#' @examples
#' #unserialize example
#' bundles <- fhir_unserialize(bundles = example_bundles1)
#'
#' #crack fhir resources
#' table_desc <- fhir_table_description(
#'     resource = "Patient",
#'     brackets = c("[", "]"),
#'     sep = " "
#' )
#'
#' df <- fhir_crack(bundles = bundles, design = table_desc)
#'
#' #original data frame
#' df
#'
#' #melt all multiple entries
#' fhir_melt_all(
#'      indexed_data_frame = df,
#'      brackets           = c("[", "]"),
#'      sep = " "
#'  )
#' @export
fhir_melt_all <- function(indexed_data_frame, brackets, sep, column_name_separator = ".") {

	# brackets should be something like c("[", "]")
	brackets <- fix_brackets(brackets = brackets)
	brackets.escaped <- esc(s = brackets)
	non_number_one_indices_pattern <- paste0(brackets.escaped[1], "(\\d+(?:\\.\\d+)*)", brackets.escaped[2])
	split_brackets_pattern <- paste0(brackets.escaped[1], "|", brackets.escaped[2])

	# Any cell where the index does not consist exclusively of the digit 1 is meltable.
	is_meltable_cell <- function(cell) {
		if (is.na(cell)) return(FALSE)  # Ignore NA values

		# Regular expression to extract numbers inside square brackets
		matches <- regmatches(cell, gregexpr(non_number_one_indices_pattern, cell))[[1]]
		if (length(matches) == 0) return(FALSE)  # Return FALSE if no matches are found

		# Extract and check all numbers
		for (match in matches) {
			# Remove square brackets and split by periods
			numbers <- unlist(strsplit(gsub(split_brackets_pattern, "", match), "\\."))

			# Check if any number is not equal to 1
			if (any(as.numeric(numbers) != 1)) {
				return(TRUE)
			}
		}

		return(FALSE)
	}

	# Function that terminates immediately if a number != 1 is found in a column index
	is_meltable_column <- function(col) {
		for (cell in col) {
			if (is_meltable_cell(cell)) {
				return(TRUE)
			}
		}
		return(FALSE)
	}

	get_meltable_columns <- function(indexed_data_frame) {
		# Apply the function to each column and find the relevant columns
		meltable_column <- sapply(indexed_data_frame, is_meltable_column)
		# Names of the columns that fulfill the condition
		selected_columns <- names(indexed_data_frame)[meltable_column]
	}

	column_name_separator_escaped <- esc(column_name_separator)

	if (!is.data.frame(indexed_data_frame)) {
		stop(
			"You need to supply a data.frame or data.table to the argument indexed_data_frame.",
			"The object you supplied is of type ", class(indexed_data_frame), "."
		)
	}

	table <- copy(indexed_data_frame) #copy to avoid side effects
	is_DT <- data.table::is.data.table(x = table)
	if (!is_DT) data.table::setDT(x = table)

	column_names <- names(table)

	# Split each column name by the separator
	meltable_column_names <- get_meltable_columns(indexed_data_frame)
	split_names <- strsplit(meltable_column_names, column_name_separator_escaped)

	get_columns <- function(prefix) {
		pattern <- paste0("^", prefix, "($|", column_name_separator_escaped, ")")
		grep(pattern, column_names, value = TRUE)
	}

	get_unique_prefixes <- function(step) {

		# Initialize a vector to store the prefixes
		prefixes <- c()

		for (name_parts in split_names) {
			# Check if the number of parts is sufficient for the given step
			if (length(name_parts) >= step) {
				# Create the prefix by joining the first `step` parts with the separator
				prefix <- paste(name_parts[1:step], collapse = column_name_separator)
				prefixes <- c(prefixes, prefix)
			}
		}
		# Get unique prefixes
		prefixes <- unique(prefixes)
		return(prefixes)
	}

	step <- 1
	repeat {
		prefixes <- get_unique_prefixes(step)
		if (!rlang::is_empty(prefixes)) {
			for (prefix in prefixes) {
				columns <- get_columns(prefix)
				table <- fhir_melt_internal(table, columns, brackets, sep, id_name = "resource_identifier", all_columns = TRUE)
			}
		} else {
			break
		}
		step <- step + 1
	}

	if (nrow(table) == 0) warning("The brackets you specified don't seem to appear in the indices of the provided data.frame. Returning NULL.")

	if (!is_DT) data.table::setDF(table)
	table <- fhir_rm_indices(table, brackets, names(table))

	return(table)
}

#' Turn a row with multiple entries into a data frame
#'
#' @param row One row of an indexed data frame. Each cell in the row may contain multiple values,
#' indexed by identifiers that follow a specific pattern.
#' @param columns A character vector specifying the names of all columns that should be molten simultaneously.
#' It is advisable to only melt columns simultaneously that belong to the same (repeating) attribute!
#' @param pattern.rows A regular expression pattern to identify row indices within the `row` cells.
#' @param pattern.rows.next.start A regular expression pattern to identify the delimiter or end of
#' each entry within a cell.
#' @param pattern.ids A regular expression defining the structure of unique identifiers in the `row`
#' cells, used to extract individual entry IDs.
#' @param pattern.ids_2 A regular expression pattern defining an alternative ID structure used within
#' the `row` cells.
#' @return A data frame with nrow > 1.
#' @noRd
melt_row <- function(row, columns, pattern.rows, pattern.rows.next.start, pattern.ids, pattern.ids_2) {
	row <- as.data.frame(row)
	ids <- stringr::str_extract_all(string = row, pattern = pattern.ids)
	names(ids) <- columns
	items <- stringr::str_split(string = row, pattern = pattern.ids)
	items <- lapply(
		items,
		function(i) {
			if (!all(is.na(i)) && i[1] == "") i[2:length(i)] else i
		}
	)
	names(items) <- columns
	d <- row[0, , drop = FALSE]
	data.table::setDF(d)

	for(i in names(ids)) {
		id <- ids[[i]]
		if (!all(is.na(id))) {
			it <- items[[i]]
			new.rows <- gsub(pattern.rows, "\\1", id)
			new.ids <- gsub(pattern.ids_2, "\\1\\3", id)
			unique.new.rows <- unique(new.rows)
			set <- paste0(new.ids, it)
			f <- sapply(
				unique.new.rows,
				function(unr) {
					fltr <- unr == new.rows
					paste0(set[fltr], collapse = "")
				}
			)
			for (n in unique.new.rows) {
				d[as.numeric(n), i]<- gsub(pattern = pattern.rows.next.start, replacement = "", x = f[names(f) == n], perl = TRUE)
			}
		}
	}
	data.table::setDT(d)
}

#' Internal function to melt multiple entries in a data.table
#'
#' This function handles the core melting operation for multiple entries in an indexed data.table.
#' It is used internally by [fhir_melt()] to separate multiple entries in a given set of columns
#' into individual rows.
#'
#' @param indexed_dt A data.table with indexed multiple entries.
#' @param columns A character vector specifying the names of all columns that should be melted simultaneously.
#' It is advisable to only melt columns that belong to the same repeating attribute.
#' @param brackets A character vector of length two, defining the brackets used for the indices.
#' @param sep A character vector of length one, the separator that was used when pasting together multiple entries.
#' @param id_name A character vector of length one, the name of the column that will hold the identification
#' of the origin of the new rows.
#' @param all_columns A boolean indicating whether all columns should be returned (default is FALSE).
#'
#' @return A data.table where each entry from the variables in `columns` appears in a separate row.
#'
#' @seealso [fhir_melt()], [fhir_rm_indices()]
fhir_melt_internal <- function(indexed_dt, columns, brackets, sep, id_name, all_columns) {

	brackets.escaped <- esc(s = brackets)
	pattern.ids <- paste0(brackets.escaped[1], "([0-9]+\\.*)+", brackets.escaped[2])
	pattern.rows <- paste0(brackets.escaped[1], "([0-9]+)\\.*.*")
	pattern.rows.next.start <- paste0(esc(sep), "$")
	pattern.ids_2 <- paste0("(", brackets.escaped[1], ")([0-9]+)\\.*(.*", brackets.escaped[2], ")")

	# this setDT() must be in any case (even if it is already a data.table!) to force a complete
	# loading of the table into memory and to avoid potential warnings, even if it is already a
	# data.table
	data.table::setDT(indexed_dt)
	# add column with column index to separate each row
	data.table::set(indexed_dt, j = id_name, value = 1:nrow(indexed_dt))
	expanded <- indexed_dt[, melt_row(.SD, columns = columns, pattern.rows, pattern.rows.next.start, pattern.ids, pattern.ids_2),
						   by = eval((id_name)), .SDcols = columns]

	if (all_columns) {
		rest <- setdiff(names(indexed_dt), columns)
		expanded <- merge.data.table(
			x = expanded,
			y = indexed_dt[, rest, with = FALSE],
			by = id_name,
			all = TRUE
		)
		data.table::setcolorder(expanded, names(indexed_dt))
	}

	expanded[[id_name]] <- NULL

	return(expanded)
}

#' Remove indices from data.frame/data.table
#'
#' Removes the indices in front of multiple entries as produced by [fhir_crack()] when brackets are provided in
#' the design.
#' @param indexed_data_frame A data frame with indices for multiple entries as produced by [fhir_crack()]
#' @param brackets A character vector of length two defining the brackets that were used in [fhir_crack()]
#' @param columns A character vector of column names, indicating from which columns indices should be removed.
#' Defaults to all columns.
#'
#' @return A data frame without indices.
#' @export
#'
#' @examples
#'
#' #unserialize example
#' bundles <- fhir_unserialize(bundles = example_bundles1)
#'
#' patients <- fhir_table_description(resource = "Patient")
#'
#' df <- fhir_crack(bundles = bundles,
#'                   design = patients,
#'                   brackets = c("[", "]"))
#'
#' df_indices_removed <- fhir_rm_indices(indexed_data_frame = df, brackets=c("[", "]"))
#'
#' @seealso [fhir_melt()]
fhir_rm_indices <- function(indexed_data_frame, brackets = c("<", ">"), columns = names(indexed_data_frame)) {
	if (!is.data.frame(indexed_data_frame)) {
		stop(
			"You need to supply a data.frame or data.table to the argument indexed_data_frame.",
			"The object you supplied is of type ", class(indexed_data_frame), "."
		)
	}
	indexed_dt <- copy(indexed_data_frame) # copy to avoid side effects
	is_DT <- data.table::is.data.table(x = indexed_dt)
	if (!is_DT) data.table::setDT(x = indexed_dt)
	brackets <- fix_brackets(brackets = brackets)
	brackets.escaped <- esc(s = brackets)
	pattern.ids <- stringr::str_c(brackets.escaped[1], "([0-9]+\\.*)*", brackets.escaped[2])
	if (!any(grepl(pattern.ids, indexed_dt))) {
		warning("The brackets you specified don't seem to appear in the data.frame.")
	}
	result <- data.table::data.table(gsub(pattern = pattern.ids, replacement = "", x = as.matrix(indexed_dt[,columns, with = FALSE])))
	indexed_dt[,columns] <- result
	if (!is_DT) data.table::setDF(x = indexed_dt)
	indexed_dt
}

#' Collapse multiple entries
#'
#' This function collapses multiple entries that belong to the same higher level FHIR element (see examples).
#'
#' Currently this function is only needed for very few FHIR elements where multiple values should be kept together
#' in the melting process. To our knowledge, this is only true for address.line elements and name.given elements.
#' Rather than building the cross product with all other elements in the resource as done by [fhir_melt()], these
#' elements should be collapsed into a single entry before melting. See examples to get a better idea of this.
#'
#' @param indexed_data_frame A data.frame/data.table with indexed multiple entries.
#' @param columns A character vector of column names where values should be collapsed
#' @param brackets A character vector of length two, defining the brackets used for the indices.
#' @param sep A character vector of length one defining the separator that was used when pasting together multiple entries in [fhir_crack()].
#' @param collapse A character vector of length one used to separate the collapsed fields. Defaults to blank space.
#' @return The modified data.table/data.frame with collapsed multiple entries
#' @export
#' @examples
#' ### First example: Keep name.given elements together
#' #unserialize example
#' bundles <- fhir_unserialize(bundles = example_bundles7)
#'
#' #Have a look at the structure of example_bundles7
#' ?example_bundles7
#'
#' #Define sep and brackets
#' sep <- "|"
#' brackets <- c("[", "]")
#'
#' #crack fhir resources
#' table_desc <- fhir_table_description(
#'     resource = "Patient",
#'     brackets = brackets,
#'     sep = sep
#' )
#'
#' df <- fhir_crack(bundles = bundles, design = table_desc)
#' df
#'
#' #name.given elements from the same name (i.e. the official vs. the nickname)
#' #should be collapsed
#'
#' df2 <- fhir_collapse(df, columns = "name.given", sep = sep, brackets = brackets)
#' df2
#'
#' #Next the name can be molten
#' fhir_melt(df2, brackets = brackets, sep = sep, columns = fhir_common_columns(df2,"name"))
#'
#'
#' ### Second: Keep address line elements together
#' #unserialize example
#' bundles <- fhir_unserialize(bundles = example_bundles6)
#'
#' #Have a look at the structure of example_bundles6
#' ?example_bundles6
#'
#' #Define sep and brackets
#' sep <- "|"
#' brackets <- c("[", "]")
#'
#' #crack fhir resources
#' table_desc <- fhir_table_description(
#'     resource = "Patient",
#'     brackets = brackets,
#'     sep = sep
#' )
#'
#' df <- fhir_crack(bundles = bundles, design = table_desc)
#' df
#'
#' #Address.line elements from the same address (i.e. the work vs. the home address)
#' #should be collapsed
#'
#' df2 <- fhir_collapse(df, columns = "address.line", sep = sep, brackets = brackets, collapse = ", ")
#' df2
#'
#' #Next the address can be molten
#' fhir_melt(df2, brackets = brackets, sep = sep, columns = fhir_common_columns(df2,"address"))
#'
#'
#' @seealso [fhir_melt()]
fhir_collapse <- function(indexed_data_frame, columns, sep, brackets, collapse = " ") {
	for (column_name in columns) {
		for (i in 1:nrow(indexed_data_frame)) {
			# Check if the cell is not empty
			if (length(indexed_data_frame[[column_name]][i])) {
				# Check if the cell starts with sep
				if (grepl("^\\[", indexed_data_frame[[column_name]][i])) {
					#split the string by sep
					split_string <- strsplit(indexed_data_frame[[column_name]][i], sep, fixed = TRUE)[[1]]
					# Initialize the index vector and previous index
					indices <- c()
					prev_index <- NULL
					# Iterate through the vector and find the first indices for each new pattern
					for (vec in seq_along(split_string)) {
						index <- as.numeric(stringr::str_extract(split_string[vec], paste0("(?<=\\", brackets[1], ")\\d")))
						if (is.null(prev_index) || is.na(prev_index) || index != prev_index) {
							indices <- c(indices, vec)
							prev_index <- index
						}
					}
					# Remove brackets for all subsequent elements except the first occurrence an collapse values
					result <- paste(ifelse(seq_along(split_string) %in% indices, split_string,
										   gsub(paste0("^\\", brackets[1], ".*\\", brackets[2]), "",
										   	 split_string, perl = TRUE)), collapse = collapse)
					# Replace the original cell value with the modified result
					indexed_data_frame[[column_name]][i] <- result
				}
			}
		}
	}
	return(indexed_data_frame)
}


########################################################################################
########################################################################################

