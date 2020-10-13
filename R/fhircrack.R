#' Concatenate paths
#' @description Concatenates two strings to path string correctly.
#'
#' @param path1 A string specifying the left hand part of the resulting path.
#' @param path2 A string specifying the right hand part of the resulting path.
#' @param os A string specifying the operating system you're operating on: windows or linux.
#'
#' @return A string containing the concatenated path.
#' @export
#'
#' @examples
#' paste_paths("data", "patients")
#' paste_paths("/data", "patients")
#' paste_paths("/data/", "patients")
#' paste_paths("/data", "/patients")
#' paste_paths("/data/", "/patients/")
#' paste_paths("data", "patients", "windows")

paste_paths <- function(path1 = "w",
						path2 = "d",
						os = "LiNuX") {
	os <- tolower(substr(os, 1, 1))

	if (os == "w") {
		return(paste0(sub("\\\\$" , "", path1), "\\", sub("^\\\\", "", path2)))
	}

	paste0(sub("/$" , "", path1), "/", sub("^/", "", path2))
}



#' Download Fhir search result
#' @description Downloads all FHIR bundles of a FHIR search request from a FHIR server.
#'
#' @param request A string containing the full FHIR search request.
#' @param username A string containing the username for basic authentication. Defaults to NULL, meaning no authentication.
#' @param password A string containing the password for basic authentication. Defaults to NULL, meaning no authentication.
#' @param max_bundles Maximal number of bundles to get. Defaults to Inf meaning all available bundles are downloaded.
#' @param verbose An Integer Scalar.  If 0, nothings is printed, if 1, only finishing message is printed, if > 1,
#' downloading progress will be printed. Defaults to 2.
#' @param max_attempts A numeric scalar. The maximal number of attempts to send a request, defaults to 5.
#' @param delay_between_attempts A numeric scalar specifying the delay in seconds between two attempts. Defaults to 10.
#' @param log_errors Takes values 0, 1 or 2. Controls the logging of errors. 1 and 2 will write a file to the current working directory.
#'
#' 0: no logging of errors,
#'
#' 1: tabulate http response and write to csv-file
#'
#' 2: write http response as to xml-file
#'
#' @return A list of bundles in xml format.
#' @export
#'
#' @examples
#' \donttest{bundles <- fhir_search("https://hapi.fhir.org/baseR4/Medication?", max_bundles=3)}

fhir_search <-
	function(request,
			 username = NULL,
			 password = NULL,
			 max_bundles = Inf,
			 verbose = 1,
			 max_attempts = 5,
			 delay_between_attempts = 10,
			 log_errors = 0) {
		bundles <- list()

		addr <- request

		if (0 < verbose) {
			message(
				paste0(
					"Starting download of ",
					if (max_bundles < Inf)
						max_bundles
					else
						"ALL!",
					" bundles of resource type ",
					gsub("(^.+/)(.+)(\\?).*$", "\\2", request, perl = TRUE),
					" from FHIR endpoint ",
					gsub("(^.+)(/.+\\?).*$", "\\1", request, perl = TRUE),
					".\n"
				)
			)

			if (9 < max_bundles)
				message("This may take a while...")
		}

		cnt <- 0

		repeat {
			cnt <- cnt + 1

			if (1 < verbose) {
				cat(paste0("bundle[", cnt, "]"))
			}

			bundle <-
				get_bundle(
					request = addr,
					username = username,
					password = password,
					verbose = verbose,
					max_attempts = max_attempts,
					delay_between_attempts = delay_between_attempts,
					log_errors = log_errors
				)

			if (is.null(bundle)) {
				if (0 < verbose) {
					message("Download interrupted.\n")
				}

				break
			}

			xml2::xml_ns_strip(bundle)

			bundles[[addr]] <- bundle

			links <- xml2::xml_find_all(bundle, "link")

			rels.nxt <-
				xml2::xml_text(xml2::xml_find_first(links, "./relation/@value")) == "next"

			if (cnt == max_bundles) {
				if (0 < verbose) {
					if (any(!is.na(rels.nxt) & rels.nxt)) {
						message(
							"\nDownload completed. Number of downloaded bundles was limited to ",
							cnt,
							" bundles, this is less than the total number of bundles available.\n"
						)
					}
					else {
						message("\nDownload completed. All available bundles were downloaded.\n")
					}
				}

				break
			}

			if (!any(!is.na(rels.nxt) & rels.nxt)) {
				if (0 < verbose) {
					message("\nDownload completed. All available bundles were downloaded.\n")
				}

				break
			}

			urls  <-
				xml2::xml_attr(xml2::xml_find_first(links, "./url"), "value")

			addr <- urls[rels.nxt][1]

			if (is.null(addr) ||
				is.na(addr) || length(addr) < 1 || addr == "") {
				if (0 < verbose) {
					message("\nDownload completed. All available bundles were downloaded.\n")
				}

				break
			}
		}

		bundles
	}



#' Save FHIR bundles as xml-files
#' @description Writes a list of FHIR bundles as numbered xml files into a directory.
#'
#' @param bundles A list of xml objects representing the FHIR bundles.
#' @param directory A string containing the path to the folder to store the data in.

#' @export
#'
#' @examples
#' #unserialize example bundle
#' bundles <- fhir_unserialize(medication_bundles)
#'
#' #save to temporary directory
#' fhir_save(bundles, directory = tempdir())


fhir_save <- function(bundles, directory = "result") {
	if (is_invalid_bundles_list(bundles)) {
		warning("Invalid bundle list format. No bundles have been saved")

		return(NULL)
	}

	w <- 1 + floor(log10(length(bundles)))

	if (!dir.exists(directory))

		dir.create(directory, recursive = TRUE)

	for (n in 1:length(bundles)) {
		xml2::write_xml(bundles[[n]], paste_paths(directory, paste0(
			stringr::str_pad(n, width = w, pad = "0"), ".xml"
		)))
	}
}



#' Load bundles from xml-files
#' @description Reads all bundles stored as xml files from a directory.
#'
#' @param directory A string containing the path to the folder were the files are stored.
#'
#' @return A list of bundles in xml format.
#' @export
#'
#' @examples
#' #unserialize example bundle
#' bundles <- fhir_unserialize(medication_bundles)
#'
#' #save to temporary directory
#' fhir_save(bundles, directory = tempdir())
#'
#' #load from temporary directory
#' loaded_bundles <- fhir_load(tempdir())

fhir_load <- function(directory) {
	xml.files <- dir(directory, "*.xml")

	lapply(lst(xml.files), function(x)
		xml2::read_xml(paste_paths(directory, x)))
}



#' Flatten list of FHIR bundles
#' @description Converts all FHIR bundles (the result of \code{\link{fhir_search}}) to a list of data frames.
#'
#' @param bundles A FHIR search result as returned by \code{\link{fhir_search}}.
#' @param design A named list of data frame descriptions.
#' Each data frame description will produce one data frame in the list of data frames returned by \code{fhir_crack}, where the data frame has the same name as the data frame description in \code{design}.
#'
#' Each data frame description is a list of 3 named elements:
#'
#' 1) \code{design$resource}: Mandatory. A string with an XPath expression locating the entries for this data frame in a FHIR bundle page. This is usually the path to a resource tpye
#'  such as \code{"//Patient"} or \code{"//Observation"}.
#'
#' 2) \code{design$cols}: Optional. Either a string containing an XPath expression referencing a certain level of attributes that should be extracted (
#'  \code{"./@value"} e.g. would extract all values on the root level) or a named list where the elements are XPath expressions indicating the specific
#'   position of attributes to extract and the names of the list elements are the column names of the resulting data frame. If \code{design$cols} is \code{NULL},
#'   all available attributes will be extracted.
#'
#' 3) \code{design$style}: Optional. This can be used instead of the function arguments \code{sep}, \code{brackets} and \code{remove_empty_columns}, but will be
#' overwritten if the corresponding function arguments are not \code{NULL}.
#'
#' A named list with the following optional elements:
#'
#'    a) \code{design$style$sep} : A string to separate pasted multiple entries.
#'
#'    b) \code{design$style$brackets}: A character vector of length two defining the brackets surrounding indices for multiple entries, e.g. \code{c( "<", ">")}.
#'       If \code{NULL}, no indices will be added to multiple entries.
#'
#'    c) \code{design$style$rm_empty_cols}: Logical scalar. Remove empty columns?
#'
#' For a more detailed explanation and comprehensive examples of \code{design}, please see the package vignette.
#'
#' @param sep A string to separate pasted multiple entries. NULL means \code{sep} is looked up in design, if it is \code{NULL} there too, \code{sep} will be set to \code{" "} as the default.
#'
#' @param remove_empty_columns Logical scalar. Remove empty columns? \code{NULL} means \code{remove_empty_columns} is looked up in \code{design}, if it is \code{NULL} there too, \code{remove_empty_columns}
#'  will be set to \code{TRUE} as the default.
#'
#' @param brackets A character vector of length two defining the brackets surrounding indices for multiple entries, e.g. \code{c( "<", ">")}.
#'  If \code{NULL}, no indices will be added to multiple entries. \code{NULL} means \code{brackets} is looked up in design, if it is \code{NULL} there too, no indices are added.
#'
#' @param verbose An Integer Scalar.  If 0, nothing is printed, if 1, only finishing message is printed, if > 1,
#' extraction progress will be printed. Defaults to 2.
#'
#' @param return_design Logical scalar. If \code{TRUE}, the complete design with automatically by fhir_crack
#' amended elements is returned as the last element of the returned list. Defaults to \code{FALSE}
#'
#' @param data.table Logical scalar. Should tables be returned in data.table format instead of data.frame?
#' defaults to FALSE.
#'
#'
#' @param add_indices Deprecated. This argument was used to control adding of indices for multiple entries. This is now
#' done via the brackets argument. If brackets is \code{NULL}, no indices are added, if brackets is not \code{NULL}, indices are added to multiple entries.
#'
#' @return A list of data frames (if \code{return_design = FALSE}) or a list of data frames and the
#' utilized \code{design}, if \code{return_design = TRUE}.
#'
#' @export
#' @import data.table
#' @examples
#' #unserialize example bundle
#' bundles <- fhir_unserialize(medication_bundles)
#'
#' #define attributes to extract
#' design <- list(
#'
#'  #define specifically which elements to extract
#' 	MedicationStatement = list(
#'
#' 		resource = ".//MedicationStatement",
#'
#' 		cols = list(
#' 				MS.ID              = "id",
#' 				STATUS.TEXT        = "text/status",
#' 				STATUS             = "status",
#' 				MEDICATION.SYSTEM  = "medicationCodeableConcept/coding/system",
#' 				MEDICATION.CODE    = "medicationCodeableConcept/coding/code",
#' 				MEDICATION.DISPLAY = "medicationCodeableConcept/coding/display",
#' 				DOSAGE             = "dosage/text",
#' 				PATIENT            = "subject/reference",
#' 				LAST.UPDATE        = "meta/lastUpdated"
#' 		),
#'
#' 		style = list(
#' 				sep = " ",
#' 				brackets = c("[", "]"),
#' 				rm_empty_cols= FALSE
#' 				)
#' 	),
#'
#'  #extract all values
#' 	Patients = list(
#'
#' 		resource = ".//Patient"
#' 	)
#' )
#'
#' #convert fhir to data frames
#' list_of_tables <- fhir_crack(bundles, design)
#'
#' #check results
#' head(list_of_tables$MedicationStatement)
#' head(list_of_tables$Patients)
#'
#'
#' @export

fhir_crack <- function(bundles,
			 design,
			 sep = NULL,
			 remove_empty_columns = NULL,
			 brackets = NULL,
			 verbose = 2,
			 return_design = FALSE,
			 data.table = FALSE,
			 add_indices) {

		#-----------------------# remove once add_indices is removed:
		if(!missing("add_indices")){

			warning("Argument add_indices is deprecated and will be removed eventually.\n In future versions indices will automatically be added when brackets are provided.")

			if(add_indices && is.null(brackets)) {brackets <- c("<", ">")}

			if(!add_indices && !is.null(brackets)) {brackets <- NULL}

		}

		#-----------------------#

		#check input validity
		design_validity <- is_valid_design(design)

		#IF general problems with design
		if (!design_validity[[1]] && is.null(design_validity[[2]])){

			return(NULL)
		}

		#If single invalid data.frame descriptions
		if (!design_validity[[1]] && !is.null(design_validity[[2]])){

			design[design_validity[[2]]] <- "invalid"

		}

		#If invalid bundle list
		if (is_invalid_bundles_list(bundles)){

			return(NULL)

		}

		#complete design
		design <- fix_design(design)

		#overwrite design with function arguments
		if(!is.null(sep)) {

			design <- lapply(design, function(x){
							x$style$sep <- sep
							x
							}
						)
		}

		if(!is.null(brackets)) {

			brackets <- fix_brackets(brackets)

			design <-lapply(design, function(x){
						x$style$brackets <- brackets
						x
						}
					)
		}

		if(!is.null(remove_empty_columns)) {

			design <- lapply(design, function(x){
							x$style$rm_empty_cols <- remove_empty_columns
							x
							}
						)
		}

		#Add attributes to design
		design <- add_attribute_to_design(design)

		#crack
		dfs <-
			bundles2dfs(
				bundles = bundles,
				design = design,
				data.table = data.table,
				# sep = sep,
				# remove_empty_columns = remove_empty_columns,
				# brackets = brackets,
				verbose = verbose
			)

		if (0 < verbose) {
			message("FHIR-Resources cracked. \n")
		}

		if(return_design){
			c(dfs, design=list(design))
		}else{
			dfs
			}
	}



#' Get capability statement
#' @description Get the capability statement of a FHIR server.
#'
#' @param url The URL of the FHIR server endpoint.
#' @param username A string containing the username for basic authentication. Defaults to NULL, meaning no authentication.
#' @param password A string containing the password for basic authentication. Defaults to NULL, meaning no authentication.
#' @param sep A string to separate pasted multiple entries
#' @param remove_empty_columns Logical scalar. Remove empty columns?
#' @param brackets A character vector of length two defining the brackets surrounding indices for multiple entries, e.g. \code{c( "<", ">")}.
#' If \code{NULL}, no indices will be added to multiple entries. \code{NULL} means \code{brackets} is looked up in design, if it is \code{NULL} there too, no indices are added.
#' @param verbose An integer Scalar.  If 0, nothings is printed, if 1, only finishing message is printed, if > 1,
#' downloading/extraction progress will be printed. Defaults to 2.
#' @param add_indices Deprecated. This argument was used to control adding of indices for multiple entries. This is now
#' done via the brackets argument. If brackets is \code{NULL}, no indices are added, if brackets is not \code{NULL}, indices are added to multiple entries.
#'
#' @return A list of data frames containing the information from the statement
#' @export
#'
#' @examples
#' \donttest{cap <- fhir_capability_statement("https://hapi.fhir.org/baseR4")}
#'

fhir_capability_statement <-function(url = "https://hapi.fhir.org/baseR4",
									 username = NULL,
									 password = NULL,
									 sep = " ",
									 remove_empty_columns = TRUE,
									 brackets = NULL,
									 verbose = 2,
									 add_indices) {

		caps <-
			fhir_search(request = paste_paths(url, "/metadata?"),
						username = username,
						password = password,
						verbose = verbose)

		design <- list(
			META      = list(resource = "/CapabilityStatement", cols = "./*/@*"),
			REST.META = list(resource = "/CapabilityStatement/rest", cols = "./*/@*"),
			REST      = list(resource = "/CapabilityStatement/rest/resource")
		)

		fhir_crack(
			bundles = caps,
			design = design,
			sep = sep,
			remove_empty_columns = remove_empty_columns,
			brackets = brackets,
			verbose = verbose,
			add_indices = add_indices
		)
	}

#' Serialize a FHIR Bundle list
#'
#' @description  Serializes a list of FHIR bundles to allow for saving in .rda or .RData format without losing integrity of pointers
#' @param bundles A list of xml objects representing FHIR bundles as returned by \code{\link{fhir_search}}
#' @return A list of serialized xml objects
#' @export
#' @examples
#' #example bundles are serialized, unserialize like this:
#' bundles <- fhir_unserialize(medication_bundles)
#'
#' #Serialize like this:
#' bundles_for_saving <- fhir_serialize(bundles)



fhir_serialize <- function(bundles) {
	if (is_invalid_bundles_list(bundles)) {
		return(NULL)
	}

	lapply(bundles, xml2::xml_serialize, connection = NULL)
}

#' Unserialize a FHIR Bundle list
#'
#' @description Unserializes a list of FHIR bundles that have been serialized to allow for saving in .rda or .RData format.
#' @param bundles A list of serialized xml objects representing FHIR bundles as returned by \code{\link{fhir_search}}
#' @return A list of unserialized xml objects
#' @export
#' @examples
#' bundles <- fhir_unserialize(medication_bundles)

fhir_unserialize <- function(bundles) {
	if (any(!sapply(bundles, is.raw))) {
		warning("The list you provided doesn't seem to contain serialized objects. Returing NULL")
		return(NULL)
	}

	lapply(bundles, xml2::xml_unserialize)
}


#' Find common columns
#'
#' This is a convenience function to find all column names in a data frame starting with the same string that can
#' then be used for \code{\link{fhir_melt}}.
#'
#' It is intended for use on data frames with column names that have been automatically produced by \code{\link{fhir_crack}}
#' and follow the form \code{level1.level2.level3} such as \code{name.given.value} or \code{code.coding.system.value}.
#' Note that this function will only work on column names following exactly this schema.
#'
#' The resulting character vector can be used for melting all columns belonging to the same attribute in an indexed data frame, see \code{?fhir_melt}.
#'
#' @param data_frame A data frame with automatically named columns as produced by \code{\link{fhir_crack}}.
#' @param column_names_prefix A string containing the common prefix of the desired columns.
#' @return A character vector with the names of all columns matching \code{column_names_prefix}.
#' @examples
#' #unserialize example bundles
#' bundles <- fhir_unserialize(medication_bundles)
#'
#' #crack Patient Resources
#' design <- list(
#'   Patients = list(".//Patient")
#' )
#'
#' dfs <- fhir_crack(bundles, design)
#'
#' #look at automatically generated names
#' names(dfs$Patients)
#'
#' #extract all column names beginning with the string "name"
#' fhir_common_columns(data_frame = dfs$Patients, column_names_prefix = "name")
#' @export
#'
fhir_common_columns <- function(data_frame, column_names_prefix) {
	pattern_column_names  <- paste0("^", column_names_prefix, "\\.*")

	column_names <- names(data_frame)

	hits <- grepl(pattern_column_names, column_names)

	if (!any(hits)) {
		stop("The column prefix you gave doesn't appear in any of the column names.")
	}

	column_names[hits]
}


#' Melt multiple entries
#'
#' This function divides multiple entries in an indexed data frame as produced by \code{\link{fhir_crack}}
#' with \code{add_indices = TRUE} into separate observations.
#'
#' Every row containing values that consist of multiple entries on the variables specified by the argument \code{columns}
#' will be turned into multiple rows, one for each entry. Values on other variables will be repeated in all the new rows.
#'
#' The new data frame will contain only the molten variables (if \code{all_cloumns = FALSE}) or all variables
#' (if \code{all_columns = TRUE}) as well as an additional variable \code{resource_identificator} that maps which rows came
#' from the same origin. The name of this column can be changed in the argument \code{id_name}.
#'
#' For a more detailed description on how to use this function please see the package vignette.
#'
#' @param indexed_data_frame A data frame with indexed multiple entries.
#' @param columns A character vector specifying the names of all columns that should be molten simultaneously.
#' It is advisable to only melt columns simultaneously that belong to the same (repeating) attribute!
#' @param brackets A character vector of length 2, defining the brackets used for the indices.
#' @param sep A string defining the separator that was used when pasting together multiple entries in \code{\link{fhir_crack}}.
#' @param id_name A string, the name of the column that will hold the identification of the origin of the new rows.
#' @param all_columns A logical scalar. Return all columns or only the ones specified in \code{columns}?
#'
#' @return A data frame where each entry from the variables in \code{columns} appears in a separate row.
#'
#' @examples
#' #generate example
#' bundle <- xml2::read_xml(
#' "<Bundle>
#'
#'     <Patient>
#'         <id value='id1'/>
#'         <address>
#'             <use value='home'/>
#'             <city value='Amsterdam'/>
#'             <type value='physical'/>
#'            <country value='Netherlands'/>
#'         </address>
#'         <birthDate value='1992-02-06'/>
#'     </Patient>
#'
#'     <Patient>
#'         <id value='id2'/>
#'         <address>
#'             <use value='home'/>
#'             <city value='Rome'/>
#'             <type value='physical'/>
#'             <country value='Italy'/>
#'         </address>
#'         <address>
#'             <use value='work'/>
#'             <city value='Stockholm'/>
#'             <type value='postal'/>
#'             <country value='Sweden'/>
#'         </address>
#'         <birthDate value='1980-05-23'/>
#'     </Patient>
#' </Bundle>"
#')
#'
#' #crack fhir resources
#' dfs <- fhir_crack(bundles = list(bundle), design = list(Patients = list(".//Patient")),
#'                   brackets = c("[","]"))
#'
#' #find all column names associated with attribute address
#' col_names <- fhir_common_columns(dfs$Patients, "address")
#'
#' #original data frame
#' dfs$Patients
#'
#' #only keep address columns
#' fhir_melt(indexed_data_frame = dfs$Patients, columns = col_names,
#'           brackets = c("[","]", sep = " "))
#'
#' #keep all columns
#' fhir_melt(indexed_data_frame = dfs$Patients, columns = col_names,
#'           brackets = c("[","]"), sep = " ", all_columns = TRUE)
#' @export

fhir_melt <-
	function(indexed_data_frame,
			 columns,
			 brackets = c("<", ">"),
			 sep = " -+- ",
			 id_name = "resource_identifier",
			 all_columns = FALSE) {

		if (!all(columns %in% names(indexed_data_frame))) {
			stop("Not all column names you gave match with the column names in the data frame.")
		}

		is_DT <- data.table::is.data.table(indexed_data_frame)

		if(!is_DT){data.table::setDT(indexed_data_frame)}

		#dbg
		#column_prefix <- "id"

		d <- data.table::rbindlist(
					lapply(seq_len(nrow(
						indexed_data_frame
					)),
					function(row.id) {
						#dbg
						#row.id <- 3

						e <-
							melt_row(
								row = indexed_data_frame[row.id,],
								columns = columns,
								brackets = brackets,
								sep = sep,
								all_columns = all_columns
							)

						if (0 < nrow(e))
							e[seq_len(nrow(e)), id_name := row.id]

						e
					}), fill = TRUE)

		if (!is.null(d) && 0 < nrow(d)) {
			data.table::setorder(d, id_name)

			if(!is_DT){
				setDF(d)
				return(d)
			}else{
				return(d)
			}

		}
	}

#' Remove indices from data frame
#'
#' Removes the indices produced by \code{\link{fhir_crack}} when \code{add_indices=TRUE}
#' @param indexed_data_frame A data frame with indices for multiple entries as produced by \code{\link{fhir_crack}}
#' @param brackets A string vector of length two defining the brackets that were used in \code{\link{fhir_crack}}
#' @param columns A string vector of column names, indicating from which columns indices should be removed. Defaults to all columns.
#'
#' @return A data frame without indices.
#' @export
#'
#' @examples
#'
#' bundle <- xml2::read_xml(
#'"<Bundle>
#'
#'         <Patient>
#'             <id value='id1'/>
#'             <address>
#'                 <use value='home'/>
#'                 <city value='Amsterdam'/>
#'                 <type value='physical'/>
#'                <country value='Netherlands'/>
#'             </address>
#'             <birthDate value='1992-02-06'/>
#'         </Patient>
#'
#'         <Patient>
#'             <id value='id2'/>
#'             <address>
#'                 <use value='home'/>
#'                 <city value='Rome'/>
#'                 <type value='physical'/>
#'                 <country value='Italy'/>
#'             </address>
#'             <address>
#'                 <use value='work'/>
#'                 <city value='Stockholm'/>
#'                 <type value='postal'/>
#'                 <country value='Sweden'/>
#'             </address>
#'             <birthDate value='1980-05-23'/>
#'         </Patient>
#' </Bundle>"
#')
#'
#'
#' dfs <- fhir_crack(bundles = list(bundle), design = list(Patients = list("/Bundle/Patient")),
#'                   verbose = 2)
#'
#' df_indices_removed <- fhir_rm_indices(dfs[[1]])


fhir_rm_indices <-
	function(indexed_data_frame,
			 brackets = c("<", ">"),
			 columns = names( indexed_data_frame )) {

		..columns <- NULL # due to NSE notes in R CMD check

		is_DT <- data.table::is.data.table(indexed_data_frame)

		if(!is_DT){data.table::setDT(indexed_data_frame)}


		brackets.escaped <- esc(brackets)

		pattern.ids <- paste0(brackets.escaped[1], "([0-9]*\\.*)+", brackets.escaped[2])


		result <- data.table::data.table(gsub( pattern.ids, "", as.matrix(indexed_data_frame[,..columns] )))

		if(!is_DT){
			data.table::setDF(result)
			return(result)
		}else{
			return(result)
		}

}


##### Documentation for medication_bundles data set ######

#' Exemplary FHIR bundles
#'
#' This data example can be used to explore some of the functions from the fhircrackr package
#' when direct access to a FHIR server is not possible.
#'
#' medication_bundles is a list of \emph{serialized} xml objects representing FHIR bundles as returned by fhir_search().
#'
#' It contains 3 bundles with MedicationStatement resources representing Medications with Snomed CT code
#' 429374003 and the respective Patient resources that are linked to these MedicationStatements.
#'
#' It corresponds to the example of downloading and flattening FHIR resources from the vignette of the package.
#'
#' @format List of length 3 containing \emph{serialized} "xml_document" objects, each representing one bundle from a
#' FHIR search request. \emph{They have to be unserialized before use, see Examples!}
#'
#' @examples
#' #unserialize xml objects before doing anything else with them!
#' medication_bundles <- fhir_unserialize(medication_bundles)
#'
#'
#' @source Generated by
#'
#' \code{
#'fhir_search("https://hapi.fhir.org/baseR4/MedicationStatement?
#' code=http://snomed.info/ct|429374003&
#' _include=MedicationStatement:subject", max.bundles = 3)
#' }
#'
#' [Downloaded 06-22-20]
#'

"medication_bundles"

##### Documentation for patient_bundles data set ######

#' Exemplary FHIR bundles
#'
#' This data example can be used to explore some of the functions from the fhircrackr package
#' when direct access to a FHIR server is not possible.
#'
#' patient_bundles is a list of \emph{serialized} xml objects representing FHIR bundles as returned by fhir_search().
#'
#' It contains 2 bundles with Patient resources.
#'
#' @format List of length 2 containing \emph{serialized} "xml_document" objects, each representing one bundle from a
#' FHIR search request. \emph{They have to be unserialized before use, see Examples!}
#'
#' @examples
#' #unserialize xml objects before doing anything else with them!
#' patient_bundles <- fhir_unserialize(patient_bundles)
#'
#'
#' @source Generated by:
#'
#' \code{fhir_search(request="http://fhir.hl7.de:8080/baseDstu3/Patient?", max_bundles=2)}
#'
#' [Downloaded 07-07-20]
#'

"patient_bundles"
