## This file contains all functions concerning the design for fhir_crack##
## Exported functions are on top, internal functions below ##


#' Retrieve design of last call to fhir_crack
#'
#' @description Returns the [fhir_design-class] of the last call to [fhir_crack()] with
#' automatically amended elements, i.e. the canonical form of the design with elements resource, cols, style
#' and respective sub-elements.
#' @export
#' @examples
#TODO
#' #load example bundles
#' bundles <- fhir_unserialize(patient_bundles)
#'
#' #incomplete but valid design
#' design <- list(
#'   Pat = list(
#'     resource = "//Patient"
#'     )
#' )
#'
#' result <- fhir_crack(bundles, design)
#'
#' fhir_canonical_design()
#'

fhir_canonical_design <- function() {

	fhircrackr_env$canonical_design
}


#' Write design to xml
#' @description Writes a [fhir_design-class] for use with [fhir_crack()] to an xml file
#' @param design A [fhir_design-class] object. See `?fhir_design`.
#' @param file A string specifying the file to write to, defaults to writing "design.xml"
#' into the current working directory
#' @export
#'
#' @examples
#' #create design
#' df_desc1 <- fhir_df_description(resource = "Patient",
#'                     cols = c(name = "name/family",
#'                              gender = "gender",
#'                              id = "id"),
#'                     style = fhir_style(sep = "||",
#'                                        brackets = c("[", "]"),
#'                                        rm_empty_cols = FALSE
#'                             )
#'              )
#'
#' df_desc2 <- fhir_df_description(resource = "Observation",
#'                     cols = c("code/coding/system", "code/coding/code")
#'             )
#'
#' design <- fhir_design(df_desc1, df_desc2, names = c("Patients", "Observations"))
#'
#' fhir_save_design(design, file = tempfile())

fhir_save_design <- function (design, file = "design.xml") {

	if(!is(design, "fhir_design")){
		stop("You can only save objects of class fhir_design. See ?fhir_design for how to build them.")}

	xml <- design2xml(design = design)

	invisible(xml2::write_xml(xml, file))
}

#' Load design from xml
#' @description Loads a [fhir_design-class] for use with [fhir_crack()] from an xml file into R
#'
#' @param file A string specifying the file from which to read
#'
#' @return A [fhir_design-class] object. See `?fhir_design`.
#' @export
#'
#' @examples
#'
#' #create and save design
#' df_desc1 <- fhir_df_description(resource = "Patient",
#'                     cols = c(name = "name/family",
#'                              gender = "gender",
#'                              id = "id"),
#'                     style = fhir_style(sep = "||",
#'                                        brackets = c("[", "]"),
#'                                        rm_empty_cols = FALSE
#'                             )
#'              )
#'
#' df_desc2 <- fhir_df_description(resource = "Observation",
#'                     cols = c("code/coding/system", "code/coding/code")
#'             )
#'
#' #create design
#' design <- fhir_design(df_desc1, df_desc2, names = c("Patients", "Observations"))
#'
#' temp <- tempfile()
#'
#' fhir_save_design(design, file = temp)
#'
#' design <- fhir_load_design(temp)

fhir_load_design <- function (file) {
	xml <- xml2::read_xml(file)
	xml2design(xml)
}

##################################################################################################
##################################################################################################


####Fixing designs####

#' Fix list (like design or style) by assigning proper names and defaults
#'
#' @param list The list to fix
#' @param names The names the list elements should have
#' @param defaults The default values that should be assigned to empty list elements
#'
#' @example fix(list = list(resource = "//Patient"), names = c("resource", "cols", "style"))
#'
#' @return A list of length length(names) with proper names in proper order
#'
#' @noRd

fix <- function (list, names, defaults = NULL) {
	msg <- NULL

	if (is.null(list)) {

		return(list(value = NULL, msg = "is NULL."))

		} else if (!inherits(list, "list")) {

		return(list(value = NULL, msg = paste0("is a ", class(list), " but must be a list.")))

	}

	if(is.null(names(list)) || "" %in% names(list)) {
		msg <- ("There are unnamed elements in this data.frame description. \n Elements are assumed to be in the following order:  resource, cols, style (with elements sep, brackets, rm_empty_cols) \n")
	}

	#append missing elements with values NULL
	if (length(list) < length(names)) {
		list <- append(list, lapply(seq_len(length(names) - length(list)), function(x) {NULL}))
	}

	lnames <- names(list)

	if (is.null(lnames)) {#if no names in original list add names

		names(list) <- names

	}else{

		wnames <- setdiff(setdiff(lnames, names), "")

		if (0 < length(wnames)) { #if wrong names in original list

			return(list(value = NULL, msg = paste0("has unknown component ", wnames, ". Names of design components can only be resource, cols, style, sep, brackets and rm_empty_cols\n")))

		}

		#if missing names in original list
		lnames[lnames == ""] <-	setdiff(names, lnames)[seq_len(sum(lnames == ""))]
		names(list) <- lnames
		list <- list[names]#order correctly
	}

	#set defaults
	if (!is.null(defaults)) {
		for (i in seq_along(list)) {
			if (is.null(list[[i]])&&!is.null(defaults[[i]])){
				list[[i]] <- defaults[[i]]
			}

		}
	}

	return(list(value=list, msg = msg))
}


#' Duplicate brackets, if just one string is provided as brackets, truncate if more than two
#' @param brackets a character or NULL
#'
#' @return a character of length two or NULL
#'
#' @example fix_brackets("|")
#' @noRd
#'

fix_brackets <- function(brackets){

	if (1 == length(brackets)) {

		c(brackets[1], brackets[1])

	} else if (length(brackets) > 2) {

		warning("brackets has to be of length 2, using only the first two elements.")
		brackets[1:2]

	} else {
		brackets
	}
}

#' fix data.frame description
#'
#' This function is only here to allow for old style designs to be fixed before being turned into S4
#' @param df_desc A data.frame description from an old-style design for fhir_crack()
#' @return a fixed data.frame description with resource, cols, style, sep, brackets and rm_empty_cols
#' @example fix_df_desc(list(resource="//Patient"))
#' @noRd
#'
fix_df_desc <- function (df_desc) {
	#dbg
	#df_desc <- design[[1]]
	msg <- NULL
	fix_res <- fix(list = df_desc, names = c("resource", "cols", "style"))

	if(is.null(fix_res$value)){

		fix_res$msg <- paste0("data.frame description ", fix_res$msg)

		return(fix_res)

	}else{

		df_desc <- fix_res$value
		msg <- fix_res$msg

	}

	if (is.null(df_desc$style)){

		df_desc$style <- list(
			sep = " ",
			brackets = NULL,
			rm_empty_cols = TRUE
		)

	}else{

		fix_res <- fix(df_desc$style,c("sep", "brackets", "rm_empty_cols"), defaults = list(" ", NULL, TRUE))

		if(is.null(fix_res$value)){

			fix_res$msg <- paste0("style ", fix_res$msg)

			return(fix_res)

		}else{

			df_desc$style <- fix_res$value
			if(is.null(msg)) {msg <- fix_res$msg}
		}
	}

	df_desc$style["brackets"] <- list(fix_brackets(df_desc$style$brackets))

	return(list(value = df_desc, msg=msg))
}

#' fix design
#'
#' This function is only here to allow for old style designs to be fixed before being turned into S4
#' @param design  An old style design (list, not S4)
#' @return a fixed design, where all df description have resource, cols, style, sep, brackets and rm_empty_cols
#' @example fix_design(listpat=(list(resource="//Patient")))
#' @noRd
#'
fix_design <- function(design) {

	fixed_design <-lapply(seq_along(design), function(i){

		fixed <- fix_df_desc(design[[i]])

		if(is.null(fixed$value)){
			#warning("Something is wrong with the data.frame description named", names(design)[i], ":\n", fixed$msg , "\n Returning NULL for this data.frame description. \n")
			return(NULL)
		}else{
			if(!is.null(fixed$msg)) {
				warning("\n For data.frame description ", names(design)[i], ": ", fixed$msg, "\n")
			}
			fixed$value
		}
	})

	names(fixed_design) <- names(design)
	return(fixed_design)
}



#' @description Add attributes to xpath expressions
#' @param design A [fhir_design-class]
#' @param attrib The attribute that should be added to the xpath expressions. Default is 'value'
#' @return A [fhir_design-class] with attribute `attrib` in all xpath expressions.
#' @noRd
#'
add_attribute_to_design <- function(design, attrib = "value"){
	if(!is(design, "fhir_design")){stop("You need to provide an object of class fhir_design here.")}
	for (n_d in names(design)) { #loop through df_desc
		if (length(design[[n_d]]@cols)>0) { #Only add attrib if xpath expressions are provided
			for (n_c in names(design[[n_d]]@cols)) { #loop through cols
				txt <- design[[n_d]]@cols[[n_c]]
				if (length(grep("/@(\\w|\\*)+$", txt)) < 1) {
					txt <- paste_paths(txt, paste0("@", attrib))
					design[[n_d]]@cols[[n_c]] <- txt
				}
			}

		}
	}
	if(validObject(design)){design}else{stop("Something went wrong with the design.")}
}



####save designs####

#' Convert design into xml format
#' @param design a design for fhir_crack()
#' @return the design as an xml object
#' @noRd
design2xml <- function (design) {

	if(!is(design, "fhir_design")){
		stop("The design you save must be of class fhir_design. See ?fhir_design.")
	}

	xml  <- xml2::xml_new_document()
	root <- xml2::xml_add_child(xml, "Design")

	for (nms in names(design)) {

		df_desc <- design[[nms]]

		child <- xml2::xml_add_child(root, nms)

		res <- xml2::xml_add_child(child, "resource")
		xml2::xml_set_attr(res, "value", df_desc@resource)

		cols <- xml2::xml_add_child(child, "cols")

		if (0 < length(df_desc@cols)) {

			for (nms_col in names(df_desc@cols)) {
				col <- xml2::xml_add_child(cols, nms_col)
				xml2::xml_set_attr(col, "value", df_desc@cols[[nms_col]])
			}

		}

		stl <- xml2::xml_add_child(child, "style")
		sep <- xml2::xml_add_child(stl, "sep")
		bra <- xml2::xml_add_child(stl, "brackets")
		opn <- xml2::xml_add_child(bra, "open")
		cls <- xml2::xml_add_child(bra, "close")
		rme <- xml2::xml_add_child(stl, "rm_empty_cols")

		xml2::xml_set_attr(sep, "value", df_desc@style@sep)
		if(length(df_desc@style@brackets)>0){
			xml2::xml_set_attr(opn, "value", df_desc@style@brackets[1])
			xml2::xml_set_attr(cls, "value", df_desc@style@brackets[2])
		}
		xml2::xml_set_attr(rme, "value", df_desc@style@rm_empty_cols)

	}
	xml2::xml_ns_strip(xml2::xml_root(xml))
	xml2::xml_root(xml)
}



####read designs####

#' Read design from xml object
#' @param xml An xml object representing a design for fhir_crack()
#' @return An object of class [fhir_design-class]
#' @noRd
#' @examples
#' df_desc1 <- fhir_df_description(resource = "Patient",
#'                     cols = c(name = "name/family",
#'                              gender = "gender",
#'                              id = "id"),
#'                     style = fhir_style(sep = "||",
#'                                        brackets = c("[", "]"),
#'                                        rm_empty_cols = FALSE
#'                             )
#'              )
#'
#' df_desc2 <- fhir_df_description(resource = "Observation",
#'                     cols = c("code/coding/system", "code/coding/code")
#'             )
#'
#' df_desc3 <- fhir_df_description(resource = "Medication")
#'
#' design <- fhir_design(df_desc1, df_desc2, df_desc3, names = c("Patients", "Observations", "Medications"))
#'
#' xml <- design2xml(design)
#'
#' design2 <- xml2design(xml)
#'
#' identical(design, design2)
#'
xml2design <- function(xml) {

	xml_design <- xml2::xml_find_all(xml, "//Design")

	if (length(xml_design) < 1) {
		warning("The Argument xml does not contain a Design. \n")
		return(NULL)
	}
	if (1 < length(xml_design)) {
		warning("xml2design() does currently not suppurt more than 1 Design per xml. Returning NULL. \n")
		return(NULL)
	}

	xml_design <- xml_design[[1]]
	xml_df_descriptions <- xml2::xml_find_all(xml_design, "*")

	if (length(xml_df_descriptions) < 1) {
		warning("Design does not contain any entries like resource, cols and style. Returning NULL. \n")
		return(NULL)
	}

	resources_names <- sapply(xml_df_descriptions, xml2::xml_name)

	if (length(unique(resources_names)) < length(resources_names)) {

		warning(paste0("Names of data.frame descriptions have to be unique. ", resources_names[duplicated(resources_names)], " are duplicates. Returning NULL. \n"))
		return(NULL)
	}

	l <- lapply(seq_along(xml_df_descriptions), function (i) {

		xml_df_desc <- xml_df_descriptions[[i]]

		resource <- xml2::xml_attr(xml2::xml_find_all(xml_df_desc, "resource"), "value")

		if (length(resource) < 1) {
			stop(paste0("data.frame description of resource ", resources_names[i], " needs at least a <Resource value=XPath_To_Resource> entry. Returning empty data.frame description. \n"))
		}

		columns <- xml2::xml_find_all(xml_df_desc, "cols")

		if (length(columns) < 1) { #no cols element
			columns <- fhir_columns()
		} else {
			columns_list <- xml2::xml_find_all(columns, "*") #extract cols
		}

		if (length(columns_list) < 1) {#cols is empty
			columns <- fhir_columns()
		}else{
			col_names <- xml2::xml_name(columns_list)
			col_values <- xml2::xml_attr(columns_list, "value")
			columns <- fhir_columns(expressions = col_values, colnames = col_names)
		}

		style <- xml2::xml_find_all(xml_df_desc, "style")

		if (length(style) < 1) {#no style info
			style <- fhir_style()
		} else {
			sep <- xml2::xml_attr(xml2::xml_find_all(style, "sep"), "value")
			if (length(sep) < 1 || all(is.na(sep))){sep <- " "}
			bra_open <- xml2::xml_attr(xml2::xml_find_all(style, "brackets/open"), "value")
			if (length(bra_open) < 1 || all(is.na(bra_open))){bra_open <- NULL}
			bra_close <- xml2::xml_attr(xml2::xml_find_all(style, "brackets/close"), "value")
			if (length(bra_close) < 1 || all(is.na(bra_close))){bra_close <- NULL}
			rm_empty_cols <- as.logical(xml2::xml_attr(xml2::xml_find_all(style, "rm_empty_cols"), "value"))
			if (length(rm_empty_cols) < 1 || all(is.na(rm_empty_cols))){rm_empty_cols <- TRUE}

			style <- fhir_style(sep, c(bra_open, bra_close), rm_empty_cols)
		}

		fhir_df_description(resource, columns, style)
	})
	names(l) <- resources_names
	fhir_design(l)
}


