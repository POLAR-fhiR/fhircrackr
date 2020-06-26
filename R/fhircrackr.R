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



#' Download fhir search result
#' @description Downloads all fhir bundles of a fhir search request from a fhir server.
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
#' - The second element is string containing a XPath expression to all the values that should be extracted. "./@value" e.g. would extract all
#'   values on the root level.
#' - The second element is a named list where the elements are XPath expressions indicating the specific position of values to extract, where the names of the
#' list elements are the column names of the resulting data frame.
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
#' cap <- fhir_cs("https://hapi.fhir.org/baseR4")
#'

fhir_cs <- function(url = "https://hapi.fhir.org/baseR4", sep = " -+- ", remove_empty_columns = T, add_indices = F, brackets = c( "<", ">"), verbose = 2) {

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

#' Serialize a FHIR Bundle list
#'
#' @description Unserializes a list of FHIR bundles that have been serialized to allow for saving in .rda or .RData format.
#' @param bundles A list of xml objects representing FHIR bundles as returned by \code{\link{fhir_search}}
#' @return A list of serialized xml objects
#' @export
#' @examples
#' bundles <- fhir_unserialize(medication_bundles)

fhir_unserialize <- function(bundles) {

	lapply(bundles, xml2::xml_unserialize)

}



#' Reconstructring Data Frames
#'
#' @param indexed_data_frame A Data Frame with indexed multiple entries in its columns.
#' @param brackets A character vector of Length 2, holding the Brackets.
#' @param sep A string, the separator.
#' @param column.prefix A String specifiying the column(s) to be melted.
#'
#' @return A data frame without indices
#' @export
#'
#' @examples
#' \dontrun{
#' fhir_melt( df )
#' }
#'
fhir_melt <- function( indexed_data_frame, column.prefix = "id", brackets = c( "<", ">" ), sep = " -+- " ) {

	#dbg
	#column.prefix <- "id"

	d <- Reduce(
		rbind,
		lapply(
			seq_len(nrow(indexed_data_frame)),
			function( row.id ) {

				#dbg
				#row.id <- 1

				e <- detree_row(row = indexed_data_frame[row.id,], column.prefix = column.prefix, brackets = brackets, sep = sep)

				e[[ column.prefix ]] <- row.id


				#e[[ column.prefix ]] <- row.id

				e
			}
		)
	)

	d[ order( d[[ column.prefix ]] ), ]
}

#' Remove indices from data frame
#' Removes the indices produced by \code{\link{fhir_crack}} when \code{add_indices=TRUE}
#' @param indexed.df A data.frame with indices as produced by \code{\link{fhir_crack}}
#' @param brackets A character of length two defining the brackets that were used in \code{\link{fhir_crack}}
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
#' dfs <- fhir_crack(bundles = list(bundle), design = list(Patients = list("/Bundle/Patient")), add_indices = TRUE, verbose = 2)
#'
#' df_indices_removed <- fhir_rm_indices(dfs[[1]])


fhir_rm_indices <- function(indexed.df, brackets = c( "<", ">" )){

	brackets.escaped <- esc(brackets)

	pattern.ids <- paste0( brackets.escaped[1], "([0-9]+\\.*)+", brackets.escaped[2] )

	vec<-c(as.matrix(indexed.df))

	res<-gsub(pattern.ids, "",vec)

	ret <- as.data.frame(matrix(res, nrow=nrow(indexed.df), ncol=ncol(indexed.df)))

	rownames(ret) <- rownames(indexed.df)
	colnames(ret) <- colnames(indexed.df)

	return(ret)
}


##### Documentation for medication_bundles data set ######

#' Exemplary FHIR bundles
#'
#' This data example can be used to explore some of the functions from the fhircrackr package
#' when direct access to a FHIR server is not possible.
#'
#' medication_bundles is a list of \emph{serialized} xml objects representing FHIR bundles as returned by fhir_search().
#' It contains 3 bundles with MedicationStatement resources representing Medications with Snomed CT code
#' 429374003 and the respective Patient resources that are linked to these MedicationStatements.
#'
#' It corresponds to Example 2 of downloading and flattening FHIR resources from the vignette of the package.
#'
#' @format List of length 3 containing \emph{serialized} "xml_document" objects, each representing one bundle from a
#' FHIR search request. \emph{They have to be unserialized before use, see Usage!}
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
