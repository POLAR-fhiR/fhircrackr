#' Represend wide cast table as a tree
#'
#' This function takes a wide table as created by [fhir_crack()] with `format="wide"`and creates the tree structure implicit in the
#' column names of the tables. It is useful to get an overview over the implied structure when planning to create
#' FHIR bundles from this table using [fhir_build_bundle()].
#'
#' @param table A data.frame or data.table as produced by [fhir_crack()] with `format="wide"` or [fhir_cast()]
#' @param resource A character vector of length one or [fhir_resource_type-class] object indicating which resource type the table is build from.
#' @param brackets A character vector of length two. The brackets used in the table.
#' @param format The format of the tree. One of `"plain"`, `"fancy"` or `"xml"`.
#' @param keep_attr A logical vector of length one indicating whether attributes should be displayed or not.
#' Only used for formats `"plain"` and `"fancy"`.
#' @param keep_ids A logical vector of length one indicating whether inidices should be displayed or not.
#' Only used for formats `"plain"` and `"fancy"`.
#' @param skip_one A logical vector of length one indicating whether first index 1 should be displayed or not.
#' Only used for formats `"plain"` and `"fancy"`.
#' @param prompt A character vector of length one use as prompt
#' Only used for formats `"plain"` and `"fancy"`.
#'
#' @return A string that can be used with `cat()` or can be written to a text file.
#'
#' @examples
#' #' #unserialize example
#' bundles <- fhir_unserialize(bundles = example_bundles1)
#'
#' #crack fhir resources
#' table_desc <- fhir_table_description(
#'     resource = "Patient",
#'     brackets = c("[", "]"),
#'     sep      = " ",
#'     format   = "wide"
#' )
#'
#' df <- fhir_crack(bundles = bundles, design = table_desc)
#'
#'
#' ###show tree
#'
#' #plain format
#' cat(fhir_tree(
#'        table = df,
#'        brackets = c("[", "]"),
#'        resource = "Patient"
#'     )
#'  )
#'
#' #fancy format with indices
#' cat(fhir_tree(
#'        table = df,
#'        brackets = c("[", "]"),
#'        resource = "Patient",
#'        format = "fancy",
#'        keep_ids = TRUE
#'      )
#' )
#'
#' #xml format
#' cat(fhir_tree(
#'        table = df,
#'        brackets = c("[", "]"),
#'        resource = "Patient",
#'        format = "xml"
#'      )
#'  )

#' @export
#' @seealso [fhir_cast()], [fhir_build_bundle()]
#'
fhir_tree <- function(table,
					  brackets,
					  resource   = 'Resource',
					  keep_attr  = FALSE,
					  keep_ids   = FALSE,
					  skip_one   = FALSE,
					  format     = "plain",
					  prompt     = ': ') {
	if (format == "fancy") {
		fhir_tree.as_string(
			tree = fhir_tree.new(
				table     = table,
				brackets  = brackets,
				root      = resource
			),
			keep_attr = keep_attr,
			keep_ids = keep_ids,
			skip_one = skip_one,
			prompt = prompt
		)
	} else if (format == "plain") {
		fhir_tree.as_text(
			tree = fhir_tree.new(
				table     = table,
				brackets  = brackets,
				root      = resource
			),
			keep_attr = keep_attr,
			keep_ids = keep_ids,
			skip_one = skip_one,
			prompt = prompt
		)
	} else if (format == "xml") {
		tree <- fhir_tree.new(table     = table,
							  brackets  = brackets,
							  root      = resource)
		fhir_tree.as_xml(tree = fhir_tree.rm_ids(tree))

	} else{
		stop("Format must be one of 'plain', 'fancy' or 'xml'")
	}
}


#####################################################################################################
#####################################################################################################

#' Create text version of tree
#'
#' @param tree A tree as produced by [fhir_tree.new()]
#' @param tabs A string that is put at the beginning of each line
#' @param tab The string used for indentation of each line
#' @param keep_attr A logical of length one indication whether attributes should be keeped in names. Defaults to `FALSE`.
#' @param keep_ids A logical of length one indication whether attributes should be keeped in names. Defaults to `TRUE`.
#' @param skip_one A logical of length one indication whether attributes should be keeped in names. Defaults to `TRUE`. Has no effect if `keep_ids` is `FALSE`.
#'
#' @noRd
#' @examples
#' #unserialize example
#' bundles <- fhir_unserialize(bundles = example_bundles1)
#'
#' #crack fhir resources
#' table_desc <- fhir_table_description(
#'     resource = "Patient",
#'     brackets = c("[", "]"),
#'     sep      = " "
#' )
#'
#' df <- fhir_crack(bundles = bundles, design = table_desc)
#'
#' #cast
#' cast_df <- fhir_cast(df, brackets = table_desc@brackets, sep = table_desc@sep, verbose = 0)
#'
#' #build tree
#' tree <- fhir_tree.new(cast_df, brackets = table_desc@brackets, root = "Patient")
#'
#' cat(fhir_tree.as_text(fhir_tree.rm_ids(tree)))
fhir_tree.as_text <- function(tree,
							  prompt    = ': ',
							  tab       = '  ',
							  keep_attr = FALSE,
							  keep_ids  = TRUE,
							  skip_one  = TRUE,
							  tabs      = '') {
	str = ''
	for (i in seq_along(tree)) {
		s <- names(tree)[i]
		if (!keep_ids)
			s <-
				gsub('[0-9]+$', '', s)
		else if (skip_one)
			s <- gsub('1$', '', s)
		s <- paste0(tabs, s)
		a <- fhir_tree.get_attr(tree[[i]])
		if (0 < length(a$names)) {
			s <-
				paste0(s, if (keep_attr)
					paste0('@', a$names)
					else
						'', prompt, a$values)
		}
		str <- paste0(
			str,
			s,
			'\n',
			fhir_tree.as_text(
				tree      = tree[[i]],
				prompt    = prompt,
				tabs      = inc_tab(tabs, tab),
				tab       = tab,
				keep_attr = keep_attr,
				keep_ids  = keep_ids,
				skip_one  = skip_one
			)
		)
	}
	str
}

#' Create string for printing of tree
#'
#' @param tree A tree as produced by [fhir_tree.new()]
#' @param prompt A string that is put between each element and its value. Defaults to a semicolon.
#' @param keep_attr A logical of length one indication whether attributes should be keeped in names. Defaults to `FALSE`.
#' @param keep_ids A logical of length one indication whether attributes should be keeped in names. Defaults to `TRUE`.
#' @param skip_one A logical of length one indication whether attributes should be keeped in names. Defaults to `TRUE`. Has no effect if `keep_ids` is `FALSE`.
#'
#' @noRd
#' @examples
#' #unserialize example
#' bundles <- fhir_unserialize(bundles = example_bundles1)
#'
#' #crack fhir resources
#' table_desc <- fhir_table_description(
#'     resource = "Patient",
#'     brackets = c("[", "]"),
#'     sep      = " "
#' )
#'
#' df <- fhir_crack(bundles = bundles, design = table_desc)
#'
#' #cast
#' cast_df <- fhir_cast(df, brackets = table_desc@brackets, sep = table_desc@sep, verbose = 0)
#'
#' #build tree
#' tree <- fhir_tree.new(cast_df, brackets = table_desc@brackets, root = "Patient")
#'
#' tree <- fhir_tree.rm_ids(tree)
#' cat(fhir_tree.as_string(tree, prompt="\u2500"))
#' @noRd
fhir_tree.as_string <-
	function(tree,
			 prompt = ' \u2500 ',
			 keep_attr = TRUE,
			 keep_ids = TRUE,
			 skip_one = TRUE) {
		t2s <- function(tree,
						pre,
						prompt,
						keep_attr,
						keep_ids,
						skip_one) {
			if (is.null(tree))
				return(NULL)
			rows <- list()
			len  <- length(tree)
			nms  <- names(tree)
			if (!keep_ids) {
				nms <- gsub('[0-9]+$', '', nms)
			}
			for (i in seq_len(len)) {
				#i <- 1
				n <- nms[i]
				tr <- tree[[i]]
				if (skip_one)
					n <- gsub('1$', '', n)
				if (!keep_ids)
					n <- gsub('[0-9]+$', '', n)
				s <-
					paste0(
						pre,
						(if (i == len)
							"\u2514"
						 else
						 	"\u251C"),
						"\u2500",
						(if (length(tr) == 0)
							'\u2500'
						 else
						 	"\u2510"),
						' ',
						n
					)
				attribs <- attributes(tr)
				attribs_names <- names(attribs)
				attribs_names <- attribs_names[attribs_names != 'names']
				if (0 < length(attribs_names)) {
					s <- if (keep_attr) {
						paste0(s, '@', attribs_names, prompt, attribs[[attribs_names]])
					} else {
						paste0(s, prompt, attribs[[attribs_names]])
					}
				}
				rows[[i]] <- paste0(
					s,
					"\n",
					t2s(
						tree      = tr,
						pre       = if (i < len)
							paste0(pre, "\u2502", " ")
						else
							paste0(pre, "  "),
						prompt    = prompt,
						keep_attr = keep_attr,
						keep_ids  = keep_ids,
						skip_one  = skip_one
					)
				)
			}

			paste0(rows, collapse = "")
		}
		t2s(
			tree = tree,
			pre = "",
			prompt = prompt,
			keep_attr = keep_attr,
			keep_ids = keep_ids,
			skip_one = skip_one
		)
	}


#' Create xml version of tree
#'
#' @param tree A tree as produced by [fhir_tree.new()]
#' @param escaped Escape special xml characters? Defaults to `TRUE`
#' @param tabs A string that is put at the beginning of each line
#' @param tab The string used for indentation of each line
#'
#' @return
#' A string representing an xml
#'
#' @noRd
#' @examples
#' #unserialize example
#' bundles <- fhir_unserialize(bundles = example_bundles1)
#'
#' #crack fhir resources
#' table_desc <- fhir_table_description(
#'     resource = "Patient",
#'     brackets = c("[", "]"),
#'     sep      = " "
#' )
#'
#' df <- fhir_crack(bundles = bundles, design = table_desc)
#'
#' #cast
#' cast_df <- fhir_cast(df, brackets = table_desc@brackets, sep = table_desc@sep, verbose = 0)
#'
#' #build tree
#' tree <- fhir_tree.new(cast_df, brackets = table_desc@brackets, root = "Patient")
#'
#' tree <- fhir_tree.rm_ids(tree)
#' cat(fhir_tree.as_xml(tree, escaped = T))

#' @noRd
fhir_tree.as_xml <-
	function(tree,
			 escaped = TRUE,
			 tabs = "",
			 tab = "  ") {
		str = ""
		for (i in seq_along(tree)) {
			s <- ""
			n <- names(tree)[i]
			tr <- tree[[i]]
			s <- paste0(tabs, "<", n)
			attrib <- fhir_tree.get_attr(tr)
			if (0 < length(attrib$names)) {
				s <-
					paste0(s, " ", attrib$names, "=\"", if (escaped) {
						esc_xml(attrib$values)
					} else {
						attrib$values
					}, "\"")
			}
			s <- if (length(tr) == 0)
				paste0(s, "/>")
			else
				paste0(s, ">")
			s = paste0(s,
					   "\n",
					   fhir_tree.as_xml(
					   	tree = tr,
					   	escaped = escaped,
					   	tabs = inc_tab(tabs, tab),
					   	tab = tab
					   ))
			if (0 < length(tr))
				s <- paste0(s, tabs, "</", n, ">\n")
			str <- paste0(str, s)
		}
		str
	}


#' Build Tree from Table
#'
#' @param table A wide cracked table.
#' @param brackets A character of length one or two containing the brackets used in the column names of the table.
#' @param skip_one A logical of length one indicating whether index 1 is skipped or not.
#' @param root A character of length one, the resource name.
#'
#' @return A tree.
#' @noRd
#' @examples
#' #unserialize example
#' bundles <- fhir_unserialize(bundles = example_bundles1)
#'
#' #crack fhir resources
#' table_desc <- fhir_table_description(
#'     resource = "Patient",
#'     brackets = c("[", "]"),
#'     sep      = " "
#' )
#' df <- fhir_crack(bundles = bundles, design = table_desc)
#'
#' #cast silently using bracktets and separator definitions from table_desc
#' cast_df <- fhir_cast(df, brackets = table_desc@brackets, sep = table_desc@sep, verbose = 0)
#'
#' #build tree
#' tree <- fhir_tree.new(cast_df, brackets = table_desc@brackets, root = "Patient")
#' cat(fhir_tree.as_text(tree))
fhir_tree.new <- function(table, brackets, root) {
	tree.new <- function(tree, names, attrib, value) {
		if (is.null(tree)) {
			tree <- list()
		}
		if (0 < length(names)) {
			name <- names[1]
			tree[[name]] <- tree.new(
				tree   = if (!is.null(tree[[name]]))
					tree[[name]],
				names  = names[-1],
				attrib = attrib,
				value  = value
			)
		} else {
			attr(tree, attrib) <- value
		}
		tree
	}

	bra <- esc(brackets[1])
	ket <- esc(brackets[2])
	full_names <- names(table)

	names <- separate_names(names = full_names,
							bra = bra,
							ket = ket)

	n.attrs <-
		sapply(strsplit(x = names, split = '@'), function(x)
			if (is.na(x[2]))
				'value'
			else
				x[2])

	names <- gsub('@.*', '', names)

	n.parts <- strsplit(x = names, split = '\\.')

	indices <-
		separate_indices(names = full_names,
						 bra = bra,
						 ket = ket)

	final_names <-
		if (length(indices) == length(names) && !all(is.na(indices))) {
			i.parts <- strsplit(x = indices, split = '\\.')
			sapply(seq_along(n.parts),
				   function(i) {
				   	paste0(n.parts[[i]],
				   		   i.parts[[i]])
				   })
		} else {
			n.parts
		}

	col_map <- lapply(seq_len(length(full_names)),
					  function(x)
					  	list(n = final_names[[x]],
					  		 a = n.attrs[[x]]))
	names(col_map) <- full_names

	tree <- list()
	for (row in seq_len(nrow(table))) {
		# row <- seq_len(nrow(table))[[1]]
		resource_tree <- list()
		for (col in names(col_map)) {
			# col <- names(col_map)[[2]]
			if (!is.na(v <- table[[col]][row])) {
				m <- col_map[[col]]
				resource_tree <-
					tree.new(
						tree = resource_tree,
						names = m$n,
						attrib = m$a,
						value = v
					)
			}
		}
		resource_tree <- resource_tree[order(names(resource_tree))]
		tree[[paste0(root, row)]] <- resource_tree
	}
	tree
}
#'
#' #' Apply Functions on a Tree
#' #'
#' #' @param tree A tree as build by fhir_tree.new() from a wide cracked table.
#' #' @param fun.start A Function called before going deeper into the tree.
#' #' @param fun.finish A Function called after going deeper into the tree.
#' #'
#' #' @return A Tree.
#' #' @export
#' #'
#' #' @examples
#' #' #unserialize example
#' #' bundles <- fhir_unserialize(bundles = example_bundles1)
#' #'
#' #' #crack fhir resources
#' #' table_desc <- fhir_table_description(
#' #'     resource = "Patient",
#' #'     brackets = c("[", "]"),
#' #'     sep      = " "
#' #' )
#' #' df <- fhir_crack(bundles = bundles, design = table_desc)
#' #'
#' #' #cast silently using bracktets and separator definitions from table_desc
#' #' cast_df <- fhir_cast(df, brackets = table_desc@brackets, sep = table_desc@sep, verbose = 0)
#' #'
#' #' #build tree
#' #' tree <- fhir_tree.new(cast_df, brackets = table_desc@brackets, root = "Patient")
#' #' tree_applied <- fhir_tree.apply(tree, fhir_tree.fun.rm_ids)
#' #' tree_faster <- fhir_tree.rm_ids(tree)
#' #' cat(fhir_tree.as_text(tree_applied))
#' #' cat(fhir_tree.as_text(tree_faster))
#' fhir_tree.apply <- function(tree, fun.start = NULL, fun.finish = NULL) {
#' 	if(!is.null(fun.start)) tree <- fun.start(node = tree)
#' 	for(n in seq_along(tree)) {
#' 		tree[[n]] <- fhir_tree.apply(tree = tree[[n]], fun.start = fun.start, fun.finish = fun.finish)
#' 	}
#' 	if(!is.null(fun.finish)) tree <- fun.finish(node = tree)
#' 	tree
#' }
#'
#' ###
#' # Closure Functions for fhir_tree.apply
#' ###
#' #' more docu needed
#' #' @param node the node
#' #' @noRd
#' fhir_tree.fun.rm_ids <- function(node) {
#' 	names(node) <- gsub('[0-9]+$', '', names(node))
#' 	node
#' }
#'
#' #' more docu needed
#' #' @param node the node
#' #' @noRd
#' fhir_tree.fun.skip_one <- function(node) {
#' 	names(node) <- gsub('([^0-9]+)(1$)', '\\1',  names(node))
#' 	node
#' }

#' Remove ids from tree
#' Removes the ids leftover from the casted table
#' @param tree A tree as produced by [fhir_tree.new()]
#'
#' @examples
#' #unserialize example
#' bundles <- fhir_unserialize(bundles = example_bundles1)
#'
#' #crack fhir resources
#' table_desc <- fhir_table_description(
#'     resource = "Patient",
#'     brackets = c("[", "]"),
#'     sep      = " "
#' )
#'
#' df <- fhir_crack(bundles = bundles, design = table_desc)
#'
#' #cast
#' cast_df <- fhir_cast(df, brackets = table_desc@brackets, sep = table_desc@sep, verbose = 0)
#'
#' #build tree
#' tree <- fhir_tree.new(cast_df, brackets = table_desc@brackets, root = "Patient")
#'
#' tree <- fhir_tree.rm_ids(tree)
#' cat(fhir_tree.as_text(tree))
#' @noRd
fhir_tree.rm_ids <- function(tree) {
	tree.names <- names(tree)
	for (n in tree.names) {
		tree[[n]] <- fhir_tree.rm_ids(tree = tree[[n]])
	}
	names(tree) <- gsub('[0-9]+', '', tree.names)
	tree
}



#' Get Attributes of a Tree Node
#'
#' @param tree A tree as build by fhir_tree.new
#'
#' @return A list of attributes' names and values
#' @noRd
fhir_tree.get_attr <- function(tree) {
	attribs <- attributes(tree)
	attribs_names <- names(attribs)
	attribs <- attribs[attribs_names != 'names']
	attribs_names <- names(attribs)
	list(names = attribs_names, values = attribs)
}


#' Increment tab
#' @param tab A character vector with strings to increment
#' @param add The string to use for incrementation
#' @return The incremented tab
#' @noRd
inc_tab <- function(tab, add = "....") {
	paste0(tab, add)
}

#' Decrement tab
#' @param tab A character vector with strings to decrement
#' @param add The string to remove
#' @return The decremented tab
#' @noRd
dec_tab <-
	function(tab, sub = "....") {
		substr(tab, 1, nchar(tab) - nchar(sub))
	}


#' Separate indices from column names
#'
#' @param names A character of length one.
#' @param bra A character of length one.
#' @param ket A character of length one.
#'
#' @return A character.
#' @noRd
#'
separate_indices <- function(names, bra, ket) {
	result <- gsub(
		pattern     = paste0(bra, '|', ket, '| '),
		replacement = '',
		x = stringr::str_extract(
			pattern     = paste0(bra, '([0-9]+\\.*)+ *', ket),
			string      = names
		)
	)
	result[is.na(result)] <- ""
	result
}

#' Separate names from column names with indices
#'
#' @param names A character of length one.
#' @param bra A character of length one.
#' @param ket A character of length one.
#'
#' @return A character.
#' @noRd
#'
separate_names <- function(names, bra, ket) {
	gsub(
		pattern     = paste0(bra, '|', ket, '| '),
		replacement = '',
		x = gsub(
			pattern     = paste0(bra, '([0-9]+\\.*)+ *', ket),
			replacement = '',
			x           = names
		)
	)
}



#' Short form for cat(fhir_tree.as_string(tree, prompt))
#'
#' @param tree A Tree.
#' @param prompt A string that is put between each element and its value. Defaults to a semicolon.
#' @param keep_attr A logical of length one indication whether attributes should be keeped in names. Defaults to `FALSE`.
#' @param keep_ids A logical of length one indication whether attributes should be keeped in names. Defaults to `TRUE`.
#' @param skip_one A logical of length one indication whether attributes should be keeped in names. Defaults to `TRUE`. Has no effect if `keep_ids` is `FALSE`.
#' @noRd
#' @examples
#' #' #unserialize example
#' bundles <- fhir_unserialize(bundles = example_bundles1)
#'
#' #crack fhir resources
#' table_desc <- fhir_table_description(
#'     resource = "Patient",
#'     brackets = c("[", "]"),
#'     sep      = " "
#' )
#'
#' df <- fhir_crack(bundles = bundles, design = table_desc)
#'
#' #cast
#' cast_df <- fhir_cast(df, brackets = table_desc@brackets, sep = table_desc@sep, verbose = 0)
#'
#' #build tree
#' tree <- fhir_tree.new(cast_df, brackets = table_desc@brackets, root = "Patient")
#' fhir_tree.print(tree, '\u2500')
fhir_tree.print <-
	function(tree,
			 prompt = ":",
			 keep_attr = FALSE,
			 keep_ids = FALSE,
			 skip_one = FALSE) {
		cat(
			fhir_tree.as_string(
				tree = tree,
				prompt = prompt,
				keep_attr = keep_attr,
				keep_ids = keep_ids,
				skip_one = skip_one
			)
		)
	}

#' Short form for cat(fhir_tree.as_text(tree, prompt))
#'
#' @param tree A Tree.
#' @param prompt A string that is put between each element and its value. Defaults to a semicolon.
#' @param tab A character of length one.
#' @param keep_attr A logical of length one indication whether attributes should be keeped in names. Defaults to `FALSE`.
#' @param keep_ids A logical of length one indication whether attributes should be keeped in names. Defaults to `TRUE`.
#' @param skip_one A logical of length one indication whether attributes should be keeped in names. Defaults to `TRUE`. Has no effect if `keep_ids` is `FALSE`.
#'
#' @noRd
#' @examples
#' #' #unserialize example
#' bundles <- fhir_unserialize(bundles = example_bundles1)
#'
#' #crack fhir resources
#' table_desc <- fhir_table_description(
#'     resource = "Patient",
#'     brackets = c("[", "]"),
#'     sep      = " "
#' )
#'
#' df <- fhir_crack(bundles = bundles, design = table_desc)
#'
#' #cast
#' cast_df <- fhir_cast(df, brackets = table_desc@brackets, sep = table_desc@sep, verbose = 0)
#'
#' #build tree
#' tree <- fhir_tree.new(cast_df, brackets = table_desc@brackets, root = "Patient")
#' fhir_tree.text(tree)
fhir_tree.text <-
	function(tree,
			 prompt = ': ',
			 tab = '  ',
			 keep_attr = FALSE,
			 keep_ids = FALSE,
			 skip_one = FALSE) {
		cat(
			fhir_tree.as_text(
				tree = tree,
				prompt = prompt,
				tab = tab,
				keep_attr = keep_attr,
				keep_ids = keep_ids,
				skip_one = skip_one
			)
		)
	}
