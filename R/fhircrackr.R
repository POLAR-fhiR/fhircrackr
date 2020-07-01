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

paste_paths <- function(path1="w", path2="d", os = "LiNuX") {

	os <- tolower(substr(os, 1, 1))

	if (os == "w") {

		return(paste0(sub( "\\\\$" , "", path1), "\\", sub( "^\\\\", "", path2)))
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
#'
#' @return A list of bundles in xml format.
#' @export
#'
#' @examples
#' bundles <- fhir_search("https://hapi.fhir.org/baseR4/Medication?", max_bundles=3)

fhir_search <- function(request, username = NULL, password = NULL, max_bundles = Inf, verbose = 1, max_attempts = 5, delay_between_attempts = 10) {

	bundles <- list()

	addr <- request

	cnt <- 0

	repeat {

		cnt <- cnt + 1

		if (1 < verbose) {cat(paste0("bundle[", cnt, "]"))}

		bundle <- get_bundle(request = addr, username = username, password = password, verbose = verbose, max_attempts = max_attempts, delay_between_attempts = delay_between_attempts)

		if (is.null(bundle)) {

			if (0 < verbose) {message("download interrupted.\n")}

			break
		}

		xml2::xml_ns_strip( bundle )

		bundles[[addr]] <- bundle

		links <- xml2::xml_find_all(bundle, "link")

		rels.nxt  <- xml2::xml_attr(xml2::xml_find_first(links, "./relation"), "value") == "next"

		if (cnt == max_bundles ) {

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

			if (0 < verbose) {message("\nDownload completed. All available bundles were downloaded.\n")}

			break
		}

		urls  <- xml2::xml_attr(xml2::xml_find_first(links, "./url"), "value")

		addr <- urls[rels.nxt][1]

		if(is.null(addr) || is.na(addr) || length(addr) < 1 || addr == "") {

			if (0 < verbose) {message("\nDownload completed. All available bundles were downloaded.\n")}

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
#' \dontrun{
#' bundles <- fhir_unserialize(medication_bundles)
#'
#' #save to folder named "result"
#' fhir_save(bundles, "result")
#'}

fhir_save <- function(bundles, directory = "result") {

	w <- 1 + floor(log10(length(bundles)))

	if (!dir.exists(directory))

		dir.create(directory, recursive = T)

	for (n in 1:length(bundles)) {

		xml2::write_xml(bundles[[n]], paste_paths(directory, paste0(stringr::str_pad(n, width = w, pad = "0"), ".xml")))
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
#' \dontrun{
#' #save to folder named "result"
#' fhir_save(bundles, "result")
#'
#' #read from folder "result"
#' read_bundles <- fhir_load("result")
#' }

fhir_load <- function(directory) {

	xml.files <- dir(directory, "*.xml")

	lapply(lst(xml.files), function(x) xml2::read_xml( paste_paths(directory, x)))
}



#' Flatten list of FHIR bundles
#' @description Converts all FHIR bundles (the result of \code{\link{fhir_search}}) to a list of data frames.
#'
#' @param bundles A FHIR search result as returned by \code{\link{fhir_search}}.
#' @param design A named list specifying which data frame should contain which entries of the bundle.
#' The names correspond to the names of the resulting data frames.
#'
#' Each element of design is a list of length 1 or 2, where the first element is a XPath expression to locate the entry in a
#' FHIR bundle page. There are 3 options for the second element of that list:
#'
#' - There is no second element: all attributes of the resource are extracted
#'
#' - The second element is a string containing a XPath expression to all the values that should be extracted. "./@value" e.g. would extract all
#'   values on the root level.
#'
#' - The second element is a named list where the elements are XPath expressions indicating the specific position of values to extract,
#'  where the names of the list elements are the column names of the resulting data frame.
#'
#' For a more detailed explanation see the package vignette.
#'
#' @param sep A string to separate pasted multiple entries.
#' @param remove_empty_columns Logical scalar. Remove empty columns?
#' @param add_indices A Logical scalar.
#' @param brackets A character vector of length two defining the Brackets surrounding the indices. e.g. c( "<", ">")
#' @param verbose An Integer Scalar.  If 0, nothings is printed, if 1, only finishing message is printed, if > 1,
#' extraction progress will be printed. Defaults to 2.
#' #' @return A list of data frames as specified by \code{design}.
#' @export
#'
#' @examples
#' #unserialize example bundle
#' bundles <- fhir_unserialize(medication_bundles)
#'
#' #define attributes to extract
#' df_design <- list(
#'
#'  #define specifically which elements to extract
#' 	MedicationStatement = list(
#'
#' 		".//MedicationStatement",
#'
#' 		list(
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
#'
#'  #extract all values
#' 	Patients = list(
#'
#' 		".//Patient"
#' 	)
#' )
#'
#' #convert fhir to data frames
#' list_of_tables <- fhir_crack(bundles, df_design)
#'
#' #check results
#' head(list_of_tables$MedicationStatement)
#' head(list_of_tables$Patients)
#'
#' @return A list of data frames as specified by \code{design}
#'
#' @export

fhir_crack <- function(bundles, design, sep = " -+- ", remove_empty_columns = F, add_indices = F, brackets = c( "<", ">"), verbose = 2) {

	if (is_invalid_design(design)) return(NULL)

	if (is_invalid_bundles_list(bundles)) return(NULL)

	dfs <- bundles2dfs(bundles = bundles, design = design, sep = sep, remove_empty_columns = remove_empty_columns, add_indices = add_indices, brackets = brackets, verbose = verbose)

	if(0 < verbose) {

		message( "FHIR-Resources cracked.")
	}

	dfs
}



#' Get capability statement
#' @description Get the capability statement of a FHIR server.
#'
#' @param url The URL of the FHIR server endpoint.
#' @param sep A string to separate pasted multiple entries
#' @param remove_empty_columns Logical scalar. Remove empty columns?
#' @param add_indices A logical scalar.
#' @param brackets A vector of strings defining the Brackets surrounding the indices. e.g. c( "<", ">")
#' @param verbose An integer Scalar.  If 0, nothings is printed, if 1, only finishing message is printed, if > 1,
#' downloading/extraction progress will be printed. Defaults to 2.
#'
#' @return A list of data frames containing the information from the statement
#' @export
#'
#' @examples
#'
#' cap <- fhir_capability_statement("https://hapi.fhir.org/baseR4")
#'

fhir_capability_statement <- function(url = "https://hapi.fhir.org/baseR4", sep = " ", remove_empty_columns = T, add_indices = T, brackets = c( "<", ">"), verbose = 2) {

	caps <- fhir_search(request = paste_paths(url, "/metadata?"), verbose = verbose)

	design <- list(
		META      = list("/CapabilityStatement", "./*/@*"),
		REST.META = list("/CapabilityStatement/rest", "./*/@*"),
		REST      = list("/CapabilityStatement/rest/resource")
	)

	fhir_crack(bundles = caps, design = design, sep = sep, remove_empty_columns = remove_empty_columns, add_indices = add_indices, brackets = brackets, verbose=verbose)
}

#' Serialize a FHIR Bundle list
#'
#' @description  Serializes a list of FHIR bundles to allow for saving in .rda or .RData format without losing integrity of pointers
#' @param bundles A list of xml objects representing FHIR bundles as returned by \code{\link{fhir_search}}
#' @return A list of serialized xml objects
#' @export
#' @examples
#' bundles <- fhir_search("https://hapi.fhir.org/baseR4/Medication?", max_bundles=3)
#' bundles_for_saving <- fhir_serialize(bundles)



fhir_serialize <- function(bundles) {

	lapply(bundles, xml2::xml_serialize, connection=NULL)
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

	if (!any(hits)) {stop("The column prefix you gave doesn't appear in any of the column names.")}

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
#' #crack fhir resources
#' dfs <- fhir_crack(bundles = list(bundle), design = list(Patients = list(".//Patient")),
#'                   add_indices = TRUE, brackets = c("[","]"))
#'
#' #find all column names associated with attribute address
#' col_names <- fhir_common_columns(dfs$Patients, "address")
#'
#' #original data frame
#' dfs$Patients
#'
#' #only keep address columns
#' fhir_melt(indexed_data_frame = dfs$Patients, columns = col_names, brackets = c("[","]"))
#'
#' #keep all columns
#' fhir_melt(indexed_data_frame = dfs$Patients, columns = col_names,
#'           brackets = c("[","]"), all_columns = TRUE)
#' @export

fhir_melt <- function(indexed_data_frame, columns, brackets = c( "<", ">" ), sep = " -+- ", id_name = "resource_identifier", all_columns = F) {

	if (! is_indexed_data_frame(indexed_data_frame)) {stop("The data frame is not indexed by fhir_crack.")}

	if (! all(columns %in% names(indexed_data_frame))) {stop("Not all column names you gave match with the column names in the data frame.")}

	#dbg
	#column_prefix <- "id"

	d <- Reduce(
		rbind,
		lapply(
			seq_len(nrow(indexed_data_frame)),
			function(row.id) {

				#dbg
				#row.id <- 1


				e <- melt_row(row = indexed_data_frame[ row.id, ], columns = columns, brackets = brackets, sep = sep, all_columns = all_columns)

				e[1:nrow(e), id_name] <- row.id

				e
			}
		)
	)

	d[order(d[[id_name]]), ]
}

#' Remove indices from data frame
#'
#' Removes the indices produced by \code{\link{fhir_crack}} when \code{add_indices=TRUE}
#' @param indexed_data_frame A data frame with indices for multiple entries as produced by \code{\link{fhir_crack}}
#' @param brackets A character of length two defining the brackets that were used in \code{\link{fhir_crack}}
#' @param sep A string defining the separator that was used when pasting together multiple entries in \code{\link{fhir_crack}}
#'
#' @return A data frame without indices.
#' @export
#'
#' @examples
#'
#'
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
#'                   add_indices = TRUE, verbose = 2)
#'
#' df_indices_removed <- fhir_rm_indices(dfs[[1]])


fhir_rm_indices <- function(indexed_data_frame, brackets = c("<", ">"), sep="-+-"){

	brackets.escaped <- esc(brackets)

	pattern.ids <- paste0(brackets.escaped[1], "([0-9]*\\.*)+", brackets.escaped[2])

	vec <- c(as.matrix(indexed_data_frame))

	splitted_entries <- stringr::str_split(vec, esc(sep))

	stripped_entries <- lapply(splitted_entries, sub, pattern = pattern.ids, replacement="")

	bound_entries <- sapply(stripped_entries, paste, collapse=sep)

	ret <- as.data.frame(matrix(bound_entries, nrow=nrow(indexed_data_frame), ncol=ncol(indexed_data_frame)))

	rownames(ret) <- rownames(indexed_data_frame)

	colnames(ret) <- colnames(indexed_data_frame)

	attr(ret,"indexed") <- NULL

	return(ret)
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
#' @source \url{https://hapi.fhir.org/baseR4/MedicationStatement?code=http://snomed.info/ct|429374003&_include=MedicationStatement:subject}
#' [Downloaded 06-22-20]
#'


"medication_bundles"
