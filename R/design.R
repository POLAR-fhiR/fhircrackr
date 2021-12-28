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
#' patients <- fhir_table_description(resource = 'Patient')
#'
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
#' @param file A string specifying the file to write to, defaults to writing 'design.xml'
#' into the current working directory.
#' @export
#' @seealso [fhir_design()], [fhir_table_description()], [fhir_load_design()]
#'
#' @examples
#' #create and save design
#' table_desc1 <- fhir_table_description(
#'     resource = 'Patient',
#'     cols     = c(
#'         id     = 'id',           # column names with xpaths
#'         name   = 'name/family',
#'         gender = 'gender'
#'     ),
#'     sep           = ':::',
#'     brackets      = c('[', ']'),
#'     rm_empty_cols = FALSE,
#'     format        = 'compact',
#'     keep_attr     = FALSE
#' )
#'
#' table_desc2 <- fhir_table_description(
#'     resource = 'Observation',
#'     cols     = c(
#'         'code/coding/system', # only xpaths
#'         'code/coding/code'
#'     )
#' )
#'
#' design <- fhir_design(
#'     Patients     = table_desc1,
#'     Observations = table_desc2
#' )
#'
#' fhir_save_design(design = design, file = tempfile())

fhir_save_design <- function(design, file = 'design.xml') {
	if(!is(design, 'fhir_design')) {
		stop('You can only save objects of class fhir_design. See ?fhir_design for how to build them.')
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
#' table_desc1 <- fhir_table_description(
#'     resource = 'Patient',
#'     cols     = c(
#'         id     = 'id',
#'         name   = 'name/family',
#'         gender = 'gender'
#'     ),
#'     sep           = ':::',
#'     brackets      = c('[', ']'),
#'     rm_empty_cols = FALSE,
#'     format        = 'compact',
#'     keep_attr     = FALSE
#' )
#'
#' table_desc2 <- fhir_table_description(
#'     resource = 'Observation',
#'     cols     = c(
#'         'code/coding/system',
#'         'code/coding/code'
#'     )
#' )
#'
#' design <- fhir_design(
#'     Patients     = table_desc1,
#'     Observations = table_desc2
#' )

#' temp <- tempfile()
#'
#' fhir_save_design(design = design, file = temp)
#'
#' design <- fhir_load_design(file = temp)
#'
fhir_load_design <- function (file) {
	xml <- xml2::read_xml(x = file)
	xml2design(xml = xml)
}

##################################################################################################
##################################################################################################




#' Duplicate brackets, if just one string is provided as brackets, truncate if more than two
#' @param brackets A character or NULL.
#'
#' @return A character of length two or NULL.
#'
#' @example fix_brackets(brackets = '|')
#' @noRd
#'

fix_brackets <- function(brackets) {
	if(is.null(brackets) || length(brackets) < 1) {
		character()
	} else if(1 == length(brackets)) {
		c(brackets[1], brackets[1])
	} else if(2 < length(brackets)) {
		warning('brackets has to be of length two, using only the first two elements.')
		brackets[1:2]
	} else {
		brackets
	}
}


####save designs####
#' Convert design into xml format
#' @param design A design for fhir_crack().
#' @return The design as an xml object.
#' @noRd
design2xml <- function (design) {

	if(!is(design, 'fhir_design')){
		stop('The design you save must be of class fhir_design. See ?fhir_design.')
	}
	xml  <- xml2::xml_new_document()
	root <- xml2::xml_add_child(.x = xml, .value = 'Design')
	for(nms in names(design)) {
		table_desc <- design[[nms]]
		child <- xml2::xml_add_child(.x = root, .value = nms)
		res <- xml2::xml_add_child(.x = child, .value = 'resource')
		xml2::xml_set_attr(x = res, attr = 'value', value = table_desc@resource)
		cols <- xml2::xml_add_child(.x = child, .value = 'cols')
		if(0 < length(table_desc@cols)) {
			for(nms_col in names(table_desc@cols)) {
				col <- xml2::xml_add_child(.x = cols, .value = nms_col)
				xml2::xml_set_attr(x = col, attr = 'value', value = table_desc@cols[[nms_col]])
			}
		}
		sep <- xml2::xml_add_child(.x = child, .value = 'sep')
		bra <- xml2::xml_add_child(.x = child, .value = 'brackets')
		opn <- xml2::xml_add_child(.x = bra, .value = 'open')
		cls <- xml2::xml_add_child(.x = bra, .value = 'close')
		rme <- xml2::xml_add_child(.x = child, .value = 'rm_empty_cols')
		frm <- xml2::xml_add_child(.x = child, .value = 'format')
		kat <- xml2::xml_add_child(.x = child, .value = 'keep_attr')
		xml2::xml_set_attr(x = sep, attr = 'value', value = table_desc@sep)
		if(0 < length(table_desc@brackets)) {
			xml2::xml_set_attr(x = opn, attr = 'value', value = table_desc@brackets[1])
			xml2::xml_set_attr(x = cls, attr = 'value', value = table_desc@brackets[2])
		}
		xml2::xml_set_attr(x = rme, attr = 'value', value = table_desc@rm_empty_cols)
		xml2::xml_set_attr(x = frm, attr = 'value', value = table_desc@format)
		xml2::xml_set_attr(x = kat, attr = 'value', value = table_desc@keep_attr)
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
#' table_desc1 <- fhir_table_description(
#'     resource = 'Patient',
#'     cols     = c(
#'         id     = 'id',
#'         name   = 'name/family',
#'         gender = 'gender'
#'     ),
#'     sep = '||',
#'     brackets = c('[', ']'),
#'     rm_empty_cols = FALSE,
#'     format        = 'compact',
#'     keep_attr     = FALSE
#' )
#'
#' table_desc2 <- fhir_table_description(
#'     resource = 'Observation',
#'     cols     = c(
#'         'code/coding/system',
#'         'code/coding/code'
#'     )
#' )
#'
#' table_desc3 <- fhir_table_description(resource = 'Medication')
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
	xml_design <- xml2::xml_find_all(x = xml, xpath = '//Design')
	if (length(xml_design) < 1) {
		warning('The Argument xml does not contain a Design. \n')
		return(NULL)
	}
	if (1 < length(xml_design)) {
		warning('xml2design() does currently not support more than 1 Design per xml. Returning NULL. \n')
		return(NULL)
	}
	xml_design <- xml_design[[1]]
	xml_table_descriptions <- xml2::xml_find_all(x = xml_design, xpath = '*')
	if(length(xml_table_descriptions) < 1) {
		warning('Design does not contain any entries like resource, cols and so on. Returning NULL. \n')
		return(NULL)
	}
	resources_names <- sapply(xml_table_descriptions, xml2::xml_name)
	if(length(unique(resources_names)) < length(resources_names)) {
		warning(
			paste0(
				'Names of data.frame descriptions have to be unique. ',
				resources_names[duplicated(resources_names)],
				' are duplicates. Returning NULL. \n'
			)
		)
		return(NULL)
	}
	l <- lapply(
		seq_along(xml_table_descriptions),
		function(i) {
			xml_table_desc <- xml_table_descriptions[[i]]
			res <- xml2::xml_attr(x = xml2::xml_find_all(x = xml_table_desc, xpath = 'resource'), attr = 'value')
			if(length(res) < 1) {
				stop(
					paste0(
						'table description of resource ',
						resources_names[i],
						' needs at least a <Resource value=XPath_To_Resource> entry. Returning empty data.frame description. \n'
					)
				)
			}
			cols <- xml2::xml_find_all(x = xml_table_desc, xpath = 'cols')
			cols <- if(length(cols) < 1) { #no cols element
				fhir_columns()
			} else {
				columns_list <- xml2::xml_find_all(x = cols, xpath = '*') #extract cols
				if (length(columns_list) < 1) {#cols is empty
					fhir_columns()
				} else {
					col_names <- xml2::xml_name(x = columns_list)
					col_values <- xml2::xml_attr(x = columns_list, attr = 'value')
					fhir_columns(xpaths = col_values, colnames = col_names)
				}
			}

			sep <- xml2::xml_attr(x = xml2::xml_find_all(x = xml_table_desc, xpath = 'sep'), attr = 'value')
			if(length(sep) < 1 || all(is.na(sep))){sep <- ':::'}

			bro <- xml2::xml_attr(x = xml2::xml_find_all(x = xml_table_desc, xpath = 'brackets/open'), attr = 'value')
			if(length(bro) < 1 || all(is.na(bro))){bro <- NULL}

			brc <- xml2::xml_attr(x = xml2::xml_find_all(x = xml_table_desc, xpath = 'brackets/close'), attr = 'value')
			if(length(brc) < 1 || all(is.na(brc))){brc <- NULL}

			bra <- if(!is.null(bro)) {
				if(!is.null(brc)) {
					c(bro, brc)
				} else {
					c(bro, bro)
				}
			} else NULL

			rec <- as.logical(xml2::xml_attr(x = xml2::xml_find_all(x = xml_table_desc, xpath = 'rm_empty_cols'), attr = 'value'))
			if(length(rm_empty_cols) < 1 || all(is.na(rm_empty_cols))){rm_empty_cols <- TRUE}

			frm <- as.logical(xml2::xml_attr(x = xml2::xml_find_all(x = xml_table_desc, xpath = 'format'), attr = 'value'))
			if(length(rm_empty_cols) < 1 || all(is.na(rm_empty_cols))){rm_empty_cols <- TRUE}

			kat <- as.logical(xml2::xml_attr(x = xml2::xml_find_all(x = xml_table_desc, xpath = 'keep_attr'), attr = 'value'))
			if(length(rm_empty_cols) < 1 || all(is.na(rm_empty_cols))){rm_empty_cols <- TRUE}

			fhir_table_description(resource = resource, cols = columns)
			fhir_table_description(
				resource      = res,
				cols          = cols,
				sep           = sep,
				brackets      = bra,
				rm_empty_cols = rec,
				format        = frm,
				keep_attr     = kat
			)
		}
	)
	names(l) <- resources_names
	fhir_design(l)
}
