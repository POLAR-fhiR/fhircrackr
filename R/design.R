## This file contains all functions concerning the design for fhir_crack ##
## Exported functions are on top, internal functions below ##


#' Retrieve design of last call to fhir_crack
#'
#' @description Returns the [fhir_design-class] of the last call to [fhir_crack()].
#' @export
#' @seealso [fhir_design()], [fhir_table_description()]
#' @examples
#' #load example bundles
#' bundles <- fhir_unserialize(bundles = patient_bundles)
#'
#' #define design
#' patients <- fhir_table_description(resource = "Patient")
#' design <- fhir_design(patients)
#'
#' result <- fhir_crack(bundles = bundles, design = design)
#'
#' fhir_canonical_design()
#'

fhir_canonical_design <- function() {
	fhircrackr_env$canonical_design
}


#' Write design to xml
#' @description Writes a [fhir_design-class] for use with [fhir_crack()] to an xml file.
#' @param design A [fhir_design-class] object. See [fhir_design()].
#' @param file A string specifying the file to write to, defaults to writing "design.xml"
#' into the current working directory.
#' @export
#' @seealso [fhir_design()], [fhir_table_description()], [fhir_load_design()]
#'
#' @examples
#' #create design
#' table_desc1 <- fhir_table_description(resource = "Patient",
#'                     cols = c(name = "name/family",
#'                              gender = "gender",
#'                              id = "id"),
#'                     style = fhir_style(sep = "||",
#'                                        brackets = c("[", "]"),
#'                                        rm_empty_cols = FALSE
#'                             )
#'              )
#'
#' table_desc2 <- fhir_table_description(resource = "Observation",
#'                     cols = c("code/coding/system", "code/coding/code")
#'             )
#'
#' design <- fhir_design(Patients = table_desc1, Observations = table_desc2)
#'
#' fhir_save_design(design = design, file = tempfile())

fhir_save_design <- function(design, file = "design.xml") {
	if(!is(design, "fhir_design")) {
		stop("You can only save objects of class fhir_design. See ?fhir_design for how to build them.")
	}
	xml <- design2xml(design = design)
	invisible(x = xml2::write_xml(x = xml, file = file))
}

#' Load design from xml
#' @description Loads a [fhir_design-class] for use with [fhir_crack()] from an xml file into R.
#'
#' @param file A string specifying the file from which to read.
#'
#' @return A [fhir_design-class] object. See `?fhir_design`.
#' @export
#' @seealso [fhir_design()], [fhir_table_description()], [fhir_save_design()]
#' @examples
#'
#' #create and save design
#' table_desc1 <- fhir_table_description(resource = "Patient",
#'                     cols = c(name = "name/family",
#'                              gender = "gender",
#'                              id = "id"),
#'                     style = fhir_style(sep = "||",
#'                                        brackets = c("[", "]"),
#'                                        rm_empty_cols = FALSE
#'                             )
#'              )
#'
#' table_desc2 <- fhir_table_description(resource = "Observation",
#'                     cols = c("code/coding/system", "code/coding/code")
#'             )
#'
#' #create design
#' design <- fhir_design(Patients = table_desc1, Observations = table_desc2)
#'
#' temp <- tempfile()
#'
#' fhir_save_design(design = design, file = temp)
#'
#' design <- fhir_load_design(file = temp)

fhir_load_design <- function (file) {
	xml <- xml2::read_xml(x = file)
	xml2design(xml = xml)
}

##################################################################################################
##################################################################################################


####Fixing designs####

#' Fix list (like design or style) by assigning proper names and defaults
#'
#' @param list The list to fix.
#' @param names The names the list elements should have.
#' @param defaults The default values that should be assigned to empty list elements.
#'
#' @example fix(list = list(resource = "//Patient"), names = c("resource", "cols", "style"))
#'
#' @return A list of length length(names) with proper names in proper order.
#'
#' @noRd

fix <- function (list, names, defaults = NULL) {
	msg <- NULL
	if(is.null(list)) {
		return(list(value = NULL, msg = "is NULL."))

	} else if(!inherits(list, "list")) {
		return(list(value = NULL, msg = paste0("is a ", class(list), " but must be a list.")))
	}
	if(is.null(names(list)) || "" %in% names(list)) {
		msg <- ("There are unnamed elements in this data.frame description. \n Elements are assumed to be in the following order:  resource, cols, style (with elements sep, brackets, rm_empty_cols) \n")
	}
	#append missing elements with values NULL
	if(length(list) < length(names)) {
		list <- append(x = list, values = lapply(seq_len(length(names) - length(list)), function(x) {NULL}))
	}
	lnames <- names(list)
	if (is.null(lnames)) {#if no names in original list add names
		names(list) <- names
	} else {
		wnames <- setdiff(setdiff(lnames, names), "")
		if (0 < length(wnames)) { #if wrong names in original list
			return(list(
				value = NULL,
				msg = paste0(
					"has unknown component ",
					wnames,
					". Names of design components can only be resource, cols, style, sep, brackets and rm_empty_cols\n")
				)
			)
		}
		#if missing names in original list
		lnames[lnames == ""] <-	setdiff(names, lnames)[seq_len(sum(lnames == ""))]
		names(list) <- lnames
		list <- list[names]#order correctly
	}
	#set defaults
	if(!is.null(defaults)) {
		for(i in seq_along(list)) {
			if(is.null(list[[i]]) && !is.null(defaults[[i]])) {
				list[[i]] <- defaults[[i]]
			}
		}
	}
	return(list(value = list, msg = msg))
}


#' Duplicate brackets, if just one string is provided as brackets, truncate if more than two
#' @param brackets A character or NULL.
#'
#' @return A character of length two or NULL.
#'
#' @example fix_brackets(brackets = "|")
#' @noRd
#'

fix_brackets <- function(brackets) {
	if(is.null(brackets) || length(brackets) < 1) {
		character()
	} else if(1 == length(brackets)) {
		c(brackets[1], brackets[1])
	} else if(2 < length(brackets)) {
		warning("brackets has to be of length two, using only the first two elements.")
		brackets[1:2]
	} else {
		brackets
	}
}

#' fix data.frame description
#'
#' This function is only here to allow for old style designs to be fixed before being turned into S4.
#' @param table_desc A data.frame description from an old-style design for fhir_crack().
#' @return A fixed data.frame description with resource, cols, style, sep, brackets and rm_empty_cols.
#' @example fix_table_desc(table_desc = list(resource="//Patient"))
#' @noRd
#'
fix_table_desc <- function (table_desc) {
	msg <- NULL
	fix_res <- fix(list = table_desc, names = c("resource", "cols", "style"))
	if(is.null(fix_res$value)) {
		fix_res$msg <- paste0("data.frame description ", fix_res$msg)
		return(fix_res)
	} else {
		table_desc <- fix_res$value
		msg <- fix_res$msg
	}
	if(is.null(table_desc$style)) {
		table_desc$style <- list(
			sep = " ",
			brackets = NULL,
			rm_empty_cols = FALSE
		)
	} else {
		fix_res <- fix(list = table_desc$style, c("sep", "brackets", "rm_empty_cols"), defaults = list(":::", NULL, FALSE))
		if(is.null(fix_res$value)) {
			fix_res$msg <- paste0("style ", fix_res$msg)
			return(fix_res)
		} else {
			table_desc$style <- fix_res$value
			if(is.null(msg)) {msg <- fix_res$msg}
		}
	}
	table_desc$style["brackets"] <- list(fix_brackets(brackets = table_desc$style$brackets))
	return(list(value = table_desc, msg = msg))
}

#' fix design
#'
#' This function is only here to allow for old style designs to be fixed before being turned into S4
#' @param design  An old style design (list, not S4).
#' @return A fixed design, where all table description have resource, cols, style, sep, brackets and rm_empty_cols.
#' @example fix_design(design=(list(resource="//Patient")))
#' @noRd
#'
fix_design <- function(design) {
	fixed_design <- lapply(
		seq_along(design),
		function(i) {
			fixed <- fix_table_desc(table_desc = design[[i]])
			if(is.null(fixed$value)) {
				#warning("Something is wrong with the table description named", names(design)[i], ":\n", fixed$msg , "\n Returning NULL for this data.frame description. \n")
				return(NULL)
			} else {
				if(!is.null(fixed$msg)) {
					warning("\n For table description ", names(design)[i], ": ", fixed$msg, "\n")
				}
				fixed$value
			}
		}
	)
	names(fixed_design) <- names(design)
	fixed_design
}



#' @description Add attributes to xpath expressions
#' @param design A [fhir_design-class].
#' @param attrib The attribute that should be added to the xpath expressions. Default is 'value'.
#' @return A [fhir_design-class] with attribute `attrib` in all xpath expressions.
#' @noRd
#'
add_attribute_to_design <- function(design, attrib = "value") {
	if(is(design, "fhir_design")) {
		for(n_d in names(design)) { #loop through table_desc
			if(0 < length(design[[n_d]]@cols)) { #Only add attrib if xpath expressions are provided
				for(n_c in names(design[[n_d]]@cols)) { #loop through cols
					txt <- design[[n_d]]@cols[[n_c]]
					if(length(grep("/@(\\w|\\*)+$", txt)) < 1) {
						txt <- paste_paths(path1 = txt, path2 = paste0("@", attrib))
						design[[n_d]]@cols[[n_c]] <- txt
					}
				}
			}
		}
	}
	if(is(design,"fhir_table_description")) {
		if(0 < length(design@cols)) { #Only add attrib if xpath expressions are provided
			for(n_c in names(design@cols)) { #loop through cols
				txt <- design@cols[[n_c]]
				if(length(grep("/@(\\w|\\*)+$", txt)) < 1) {
					txt <- paste_paths(path1 = txt, path2 = paste0("@", attrib))
					design@cols[[n_c]] <- txt
				}
			}
		}
	}
	if(validObject(design)){design} else {stop("Something went wrong with the design.")}
}



####save designs####

#' Convert design into xml format
#' @param design A design for fhir_crack().
#' @return The design as an xml object.
#' @noRd
design2xml <- function (design) {

	if(!is(design, "fhir_design")){
		stop("The design you save must be of class fhir_design. See ?fhir_design.")
	}
	xml  <- xml2::xml_new_document()
	root <- xml2::xml_add_child(.x = xml, .value = "Design")
	for(nms in names(design)) {
		table_desc <- design[[nms]]
		child <- xml2::xml_add_child(.x = root, .value = nms)
		res <- xml2::xml_add_child(.x = child, .value = "resource")
		xml2::xml_set_attr(x = res, attr = "value", value = table_desc@resource)
		cols <- xml2::xml_add_child(.x = child, .value = "cols")
		if(0 < length(table_desc@cols)) {
			for(nms_col in names(table_desc@cols)) {
				col <- xml2::xml_add_child(.x = cols, .value = nms_col)
				xml2::xml_set_attr(x = col, attr = "value", value = table_desc@cols[[nms_col]])
			}
		}
		stl <- xml2::xml_add_child(.x = child, .value = "style")
		sep <- xml2::xml_add_child(.x = stl, .value = "sep")
		bra <- xml2::xml_add_child(.x = stl, .value = "brackets")
		opn <- xml2::xml_add_child(.x = bra, .value = "open")
		cls <- xml2::xml_add_child(.x = bra, .value = "close")
		rme <- xml2::xml_add_child(.x = stl, .value = "rm_empty_cols")
		xml2::xml_set_attr(x = sep, attr = "value", value = table_desc@style@sep)
		if(0 < length(table_desc@style@brackets)) {
			xml2::xml_set_attr(x = opn, attr = "value", value = table_desc@style@brackets[1])
			xml2::xml_set_attr(x = cls, attr = "value", value = table_desc@style@brackets[2])
		}
		xml2::xml_set_attr(x = rme, attr = "value", value = table_desc@style@rm_empty_cols)
	}
	#xml2::xml_ns_strip(x = xml2::xml_root(x = xml))
	xml <- fhir_ns_strip(xml = xml2::xml_root(x = xml))
	xml2::xml_root(x = xml)
}



####read designs####

#' Read design from xml object
#' @param xml An xml object representing a design for fhir_crack().
#' @return An object of class [fhir_design-class].
#' @noRd
#' @examples
#' table_desc1 <- fhir_table_description(resource = "Patient",
#'                     cols = c(name = "name/family",
#'                              gender = "gender",
#'                              id = "id"),
#'                     style = fhir_style(sep = "||",
#'                                        brackets = c("[", "]"),
#'                                        rm_empty_cols = FALSE
#'                             )
#'              )
#'
#' table_desc2 <- fhir_table_description(resource = "Observation",
#'                     cols = c("code/coding/system", "code/coding/code")
#'             )
#'
#' table_desc3 <- fhir_table_description(resource = "Medication")
#'
#' design <- fhir_design(table_desc1, table_desc2, table_desc3)
#'
#' xml <- design2xml(design = design)
#'
#' design2 <- xml2design(xml = xml)
#'
#' identical(design, design2)
#'
xml2design <- function(xml) {
	xml_design <- xml2::xml_find_all(x = xml, xpath = "//Design")
	if (length(xml_design) < 1) {
		warning("The Argument xml does not contain a Design. \n")
		return(NULL)
	}
	if (1 < length(xml_design)) {
		warning("xml2design() does currently not suppurt more than 1 Design per xml. Returning NULL. \n")
		return(NULL)
	}
	xml_design <- xml_design[[1]]
	xml_table_descriptions <- xml2::xml_find_all(x = xml_design, xpath = "*")
	if(length(xml_table_descriptions) < 1) {
		warning("Design does not contain any entries like resource, cols and style. Returning NULL. \n")
		return(NULL)
	}
	resources_names <- sapply(xml_table_descriptions, xml2::xml_name)
	if(length(unique(resources_names)) < length(resources_names)) {
		warning(
			paste0(
				"Names of data.frame descriptions have to be unique. ",
				resources_names[duplicated(resources_names)],
				" are duplicates. Returning NULL. \n"
			)
		)
		return(NULL)
	}
	l <- lapply(
		seq_along(xml_table_descriptions),
		function(i) {
			xml_table_desc <- xml_table_descriptions[[i]]
			resource <- xml2::xml_attr(x = xml2::xml_find_all(x = xml_table_desc, xpath = "resource"), attr = "value")
			if(length(resource) < 1) {
				stop(
					paste0(
						"data.frame description of resource ",
						resources_names[i],
						" needs at least a <Resource value=XPath_To_Resource> entry. Returning empty data.frame description. \n"
					)
				)
			}
			columns <- xml2::xml_find_all(x = xml_table_desc, xpath = "cols")
			if(length(columns) < 1) { #no cols element
				columns <- fhir_columns()
			} else {
				columns_list <- xml2::xml_find_all(x = columns, xpath = "*") #extract cols
			}
			if (length(columns_list) < 1) {#cols is empty
				columns <- fhir_columns()
			} else {
				col_names <- xml2::xml_name(x = columns_list)
				col_values <- xml2::xml_attr(x = columns_list, attr = "value")
				columns <- fhir_columns(xpaths = col_values, colnames = col_names)
			}
			style <- xml2::xml_find_all(x = xml_table_desc, xpath = "style")
			if (length(style) < 1) {#no style info
				style <- fhir_style()
			} else {
				sep <- xml2::xml_attr(x = xml2::xml_find_all(x = style, xpath = "sep"), attr = "value")
				if(length(sep) < 1 || all(is.na(sep))){sep <- ":::"}
				bra_open <- xml2::xml_attr(x = xml2::xml_find_all(x = style, xpath = "brackets/open"), attr = "value")
				if(length(bra_open) < 1 || all(is.na(bra_open))){bra_open <- NULL}
				bra_close <- xml2::xml_attr(x = xml2::xml_find_all(x = style, xpath = "brackets/close"), attr = "value")
				if(length(bra_close) < 1 || all(is.na(bra_close))){bra_close <- NULL}
				rm_empty_cols <- as.logical(xml2::xml_attr(x = xml2::xml_find_all(x = style, xpath = "rm_empty_cols"), attr = "value"))
				if(length(rm_empty_cols) < 1 || all(is.na(rm_empty_cols))){rm_empty_cols <- TRUE}
				style <- fhir_style(sep = sep, brackets = c(bra_open, bra_close), rm_empty_cols = rm_empty_cols)
			}
			fhir_table_description(resource = resource, cols = columns)
		}
	)
	names(l) <- resources_names
	fhir_design(l)
}
