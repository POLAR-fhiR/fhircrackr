## This file contains all functions dealing with multiple entries/indices##
## Exported functions are on top, internal functions below ##



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
#' bundles <- fhir_unserialize(medication_bundles)
#'
#' #crack Patient Resources
#' design <- fhir_design(
#'   fhir_df_description(resource = "Patient"),
#'   names = "Patients"
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
#' @seealso [fhir_melt()], [fhir_melt_all()], [fhir_rm_indices()]
#'
fhir_common_columns <- function(data_frame, column_names_prefix) {

	if(!is.data.frame(data_frame)){
		stop("You need to supply a data.frame or data.table to the argument indexed_data_frame.",
			 "The object you supplied is of type ", class(data_frame), ".")}

	pattern_column_names  <- paste0("^", column_names_prefix, "($|\\.+)")

	column_names <- names(data_frame)

	hits <- grepl(pattern_column_names, column_names)

	if (!any(hits)) {
		stop("The column prefix you gave doesn't appear in any of the column names.")
	}

	column_names[hits]
}


#' Melt multiple entries
#'
#' This function divides multiple entries in an indexed data frame as produced by [fhir_crack()].
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
#' @param brackets A character vector of length 2, defining the brackets used for the indices.
#' @param sep A string defining the separator that was used when pasting together multiple entries in [fhir_crack()].
#' @param id_name A string, the name of the column that will hold the identification of the origin of the new rows.
#' @param all_columns A logical scalar. Return all columns or only the ones specified in `columns`?
#'
#' @return A data.frame/data.table where each entry from the variables in `columns` appears in a separate row.
#'
#' @examples
#' #generate example
#' bundle <- fhir_bundle_xml(xml2::read_xml(
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
#'))
#'
#' #crack fhir resources
#' dfs <- fhir_crack(bundles = list(bundle),
#'                   design = fhir_design(fhir_df_description(resource = "Patient"),
#'                                                            names = "Patients"),
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
#'           brackets = c("[","]"), sep = " ")
#'
#' #keep all columns
#' fhir_melt(indexed_data_frame = dfs$Patients, columns = col_names,
#'           brackets = c("[","]"), sep = " ", all_columns = TRUE)
#' @export
#' @seealso [fhir_common_columns()], [fhir_melt_all()], [fhir_rm_indices()]

fhir_melt <- function(
	indexed_data_frame,
	columns,
	brackets = c("<", ">"),
	sep = " ",
	id_name = "resource_identifier",
	all_columns = FALSE) {

	if(!is.data.frame(indexed_data_frame)){
		stop("You need to supply a data.frame or data.table to the argument indexed_data_frame.",
			 "The object you supplied is of type ", class(indexed_data_frame), ".")}

	if (!all(columns %in% names(indexed_data_frame))) {
		stop("Not all column names you gave match with the column names in the data frame.")
	}

	indexed_dt <- copy(indexed_data_frame) #copy to avoid side effects

	is_DT <- data.table::is.data.table(indexed_dt)

	if(!is_DT){data.table::setDT(indexed_dt)}

	brackets <- fix_brackets(brackets)

	#dbg
	#column_prefix <- "id"

	d <- data.table::rbindlist(
		lapply(seq_len(nrow(
			indexed_dt
		)),
		function(row.id) {
			#dbg
			#row.id <- 3

			e <-
				melt_row(
					row = indexed_dt[row.id,],
					columns = columns,
					brackets = brackets,
					sep = sep,
					all_columns = all_columns
				)

			if (0 < nrow(e))
				e[seq_len(nrow(e)), (id_name) := row.id]

			e
		}), fill = TRUE)

	if(nrow(d) == 0) {warning("The brackets you specified don't seem to appear in the indices of the provided data.frame. Returning NULL.")}

	if(!is.null(d) && 0 < nrow(d)) {
		data.table::setorderv(d, id_name)

		if(!is_DT){setDF(d)}
		return(d)

	}
}

#' Melt all multiple entries
#'
#' This function divides all multiple entries in a (list of)indexed data frame as produced by [fhir_crack()]
#' into separate observations.
#'
#' Every row containing values that consist of multiple entries will be turned into multiple rows, one for each entry.
#' Values on other variables will be repeated in all the new rows. This function will only work if the column names
#' reflect the path to the corresponding resource element with `.` as a separator, e.g. `name.given`.
#' These names are produced automatically by [fhir_design()] when the cols element of the
#' design is unnamed or omitted.
#'
#' If `rm_indices=FALSE` the original indices are kept for every entry. These are useful if you need to keep in mind the
#' original structure of the resource.
#'
#' For a more detailed description on how to use this function please see the package vignette.
#'
#' @param indexed_data_frame A data.frame/data.table with indexed multiple entries or an object of class
#' [fhir_df_list-class]/[fhir_dt_list-class]. Column names mustreflect the path to the corresponding resource
#' element with `.` as a separator, e.g. `name.given`.
#' @param brackets A character vector of length 2, defining the brackets used for the indices. If a
#' [fhir_df_list-class]/[fhir_dt_list-class] is provided for `indexed_data_frame`, this argument is ignored.
#' @param sep A string defining the separator that was used when pasting together multiple entries in [fhir_crack()].
#' If a [fhir_df_list-class]/[fhir_dt_list-class] is provided for `indexed_data_frame`, this argument is ignored.
#' @param rm_indices Logical of length one. Should indices be removed? If `FALSE` the indices from the input data are preserved
#' the way they are. They can be extracted with [fhir_extract_indices()], removed with [fhir_rm_indices()]
#' and restored with [fhir_restore_indices()]
#' @return A data frame where each multiple entry appears in a separate row.
#' @docType methods
#' @rdname fhir_melt_all-methods
#' @examples
#' #generate example
#' bundle <- fhir_bundle_xml(xml2::read_xml(
#' 	"<Bundle>
#'
#' 		<Patient>
#' 			<id value='id1'/>
#' 			<address>
#' 				<use value='home'/>
#' 				<city value='Amsterdam'/>
#' 				<type value='physical'/>
#' 				<country value='Netherlands'/>
#' 			</address>
#' 			<birthDate value='1992-02-06'/>
#' 		</Patient>
#'
#' 		<Patient>
#' 			<id value='id2'/>
#' 			<address>
#' 				<use value='home'/>
#' 				<city value='Rome'/>
#' 				<type value='physical'/>
#' 				<country value='Italy'/>
#' 			</address>
#' 			<address>
#' 				<use value='work'/>
#' 				<city value='Stockholm'/>
#' 				<type value='postal'/>
#' 				<country value='Sweden'/>
#' 			</address>
#' 			<birthDate value='1980-05-23'/>
#' 		</Patient>
#'
#' 		<Patient>
#' 			<id value='id3.1'/>
#' 			<id value='id3.2'/>
#' 			<address>
#' 				<use value='home'/>
#' 				<city value='Berlin'/>
#' 			</address>
#' 			<address>
#' 				<type value='postal'/>
#' 				<country value='France'/>
#' 			</address>
#' 			<address>
#' 				<use value='work'/>
#' 				<city value='London'/>
#' 				<type value='postal'/>
#' 				<country value='England'/>
#' 			</address>
#' 			<birthDate value='1974-12-25'/>
#' 		</Patient>
#' 		<Observation>
#' 		<id value = '1'/>
#'		<code>
#'			<coding>
#'			   <system value='http://loinc.org'/>
#'			   <code value='29463-7'/>
#'			   <display value='Body Weight'/>
#'			</coding>
#'			<coding>
#'			   <system value='http://snomed.info/sct'/>
#'		 	   <code value='27113001'/>
#'			   <display value='Body weight'/>
#'			</coding>
#'		</code>
#' 		</Observation>
#' 	</Bundle>"
#' ))
#'
#' #crack fhir resources
#' patients <- fhir_df_description(resource = "Patient")
#'
#' observations <- fhir_df_description(resource = "Observation")
#'
#' design <- fhir_design(patients, observations)
#'
#' dfs <- fhir_crack(bundles = list(bundle), design = design, sep="||", brackets = c("[","]"))
#'
#' #use on the list of tables:
#' #all tables are molten, brackets and sep are inferred from the attached design
#' fhir_melt_all(dfs)
#'
#' #use on single df
#' fhir_melt_all(dfs$patients, brackets = c("[","]"), sep="||")
#' @export
#' @seealso [fhir_melt()], [fhir_rm_indices()]

setGeneric(
	"fhir_melt_all",
	function(indexed_data_frame, sep, brackets, rm_indices=TRUE){
		standardGeneric("fhir_melt_all")
	},
	signature = "indexed_data_frame"
)

#' @rdname fhir_melt_all-methods
#' @aliases fhir_melt_all,data.frame-method
setMethod(
	"fhir_melt_all",
	signature = c(indexed_data_frame = "data.frame"),
	function(indexed_data_frame, sep, brackets, rm_indices=TRUE){melt_all(indexed_data_frame, sep, brackets, rm_indices)}
)

#' @rdname fhir_melt_all-methods
#' @aliases fhir_melt_all,fhir_table_list-method
setMethod(
	"fhir_melt_all",
	signature = c(indexed_data_frame = "fhir_table_list"),
	function(indexed_data_frame, rm_indices=TRUE){
		lapply(lst(names(indexed_data_frame)), function(name){
			melt_all(indexed_data_frame[[name]],
					 sep = indexed_data_frame@design[[name]]@style@sep,
					 brackets = indexed_data_frame@design[[name]]@style@brackets,
					 rm_indices = rm_indices)
			})
	}
)

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
#' patients <- fhir_df_description(resource = "Patient")
#'
#' dfs <- fhir_crack(bundles = list(bundle),
#'                   design = fhir_design(patients),
#'                   brackets = c("[", "]"))
#'
#' df_indices_removed <- fhir_rm_indices(dfs$patients, brackets=c("[", "]"))
#'
#' @seealso [fhir_melt()], [fhir_melt_all()]


fhir_rm_indices <- function(
	indexed_data_frame,
	brackets = c("<", ">"),
	columns = names( indexed_data_frame )) {

	if(!is.data.frame(indexed_data_frame)){
		stop("You need to supply a data.frame or data.table to the argument indexed_data_frame.",
			 "The object you supplied is of type ", class(indexed_data_frame), ".")}

	indexed_dt <- copy(indexed_data_frame) #copy to avoid side effects

	is_DT <- data.table::is.data.table(indexed_dt)

	if(!is_DT){data.table::setDT(indexed_dt)}

	brackets <- fix_brackets(brackets)

	brackets.escaped <- esc(brackets)

	pattern.ids <- paste0(brackets.escaped[1], "([0-9]+\\.*)*", brackets.escaped[2])

	if(!any(grepl(pattern.ids, indexed_dt))){
		warning("The brackets you specified don't seem to appear in the data.frame.")}

	result <- data.table::data.table(gsub( pattern.ids, "", as.matrix(indexed_dt[,columns, with=F])))

	indexed_dt[ , columns] <- result

	if(!is_DT) data.table::setDF(indexed_dt)

	indexed_dt
}


#' Extract indices from molten data.frame/data.table with multiple entries
#'
#' Extracts a character matrix with indices from a molten indexed data.frame/data.table as created
#' by [fhir_melt_all()] with `rm_indices=FALSE`. After extraction, the indices of the data.frame can be removed,
#' and the data.frame can be manipulated as desired. As long as dimensions as well as row and column
#' order are preserved during manipulation, the indices can then be restored again with
#' [fhir_restore_indices()].
#'
#' @param indexed_data_frame A data frame with indices for multiple entries as produced by [fhir_melt_all()].
#' Please make sure there is no more than one index present per cell, i.e. all multiple entries have been molten into multiple
#' rows as achieved by [fhir_melt_all()] with the `rm_indices=FALSE`.
#' All columns have to be of class character.
#' @param brackets A string vector of length two defining the brackets that were used in [fhir_crack()].
#' @return A character matrix with same dimensions as `indexed_data_frame` containing the indices. For use with
#' `fhir_restore_indices()`.
#'
#' @examples
#' #generate example bundle
#'bundle <- xml2::read_xml(
#'	"<Bundle>
#'
#'		<Patient>
#'			<id value='id1'/>
#'			<address>
#'				<use value='home'/>
#'				<city value='Amsterdam'/>
#'				<type value='physical'/>
#'				<country value='Netherlands'/>
#'			</address>
#'			<birthDate value='1992-02-06'/>
#'		</Patient>
#'
#'		<Patient>
#'			<id value='id2'/>
#'			<address>
#'				<use value='home'/>
#'				<city value='Rome'/>
#'				<type value='physical'/>
#'				<country value='Italy'/>
#'			</address>
#'			<address>
#'				<use value='work'/>
#'				<city value='Stockholm'/>
#'				<type value='postal'/>
#'				<country value='Sweden'/>
#'			</address>
#'			<birthDate value='1980-05-23'/>
#'		</Patient>
#'
#'		<Patient>
#'			<id value='id3'/>
#'			<address>
#'				<use value='home'/>
#'				<city value='Berlin'/>
#'			</address>
#'			<address>
#'				<type value='postal'/>
#'				<country value='France'/>
#'			</address>
#'			<address>
#'				<use value='work'/>
#'				<city value='London'/>
#'				<type value='postal'/>
#'				<country value='England'/>
#'			</address>
#'			<birthDate value='1974-12-25'/>
#'			<birthDate value='1978-11-13'/>
#'		</Patient>
#'
#'	</Bundle>"
#')
#'
#'#crack fhir resources
#'patients <- fhir_df_description(resource = "Patient",
#'                                style = fhir_style(brackets = c("[","]"),
#'				                                     sep="||")
#'				                  )
#'dfs <- fhir_crack(bundles = list(bundle),
#'                  design = fhir_design(patients))
#'
#'#Melt all multiple entries
#'d <- fhir_melt_all(dfs$patients, brackets = c("[", "]"), sep="||", rm_indices = FALSE)
#'
#'#Extract indices
#'fhir_extract_indices(d, brackets = c("[", "]"))
#' @export
#' @seealso [fhir_melt()], [fhir_melt_all()], [fhir_rm_indices()]


fhir_extract_indices <- function(indexed_data_frame, brackets){

	if(!is.data.frame(indexed_data_frame)){
		stop("You need to supply a data.frame or data.table to the argument indexed_data_frame.",
			 "The object you supplied is of type ", class(indexed_data_frame), ".")}

	if(any(!sapply(indexed_data_frame, is.character))){
		warning("The indexed_data_frame contains columns that are not of type character. ",
				"Indices will only be extracted properly if all columns are of type character. ",
				"Please convert all columns to character before using fhir_extract_indices.")
	}

	brackets.escaped <- esc(brackets)

	pattern.ids <- paste0(brackets.escaped[1], "([0-9]+\\.*)*", brackets.escaped[2])

	if(!any(grepl(pattern.ids, indexed_data_frame))){stop("There don't seem to be any indices ",
														  "with the specified brackets.")}

	result <- apply(indexed_data_frame, 2, stringr::str_extract_all, pattern=pattern.ids, simplify = T)

	if(is.list(result)){
		warning("There seems to be more than one index per cell in indexed_data_frame.",
				" The result can therefore not be represented in a single matrix. ",
				"Please make sure you used fhir_melt_all() on your data.frame to ensure every ",
				"cell only contains a single entry and associated index.")
	}else{
		result <- matrix(result, ncol=ncol(indexed_data_frame))
		colnames(result) <- names(indexed_data_frame)
	}

	result
}

#' Restore indices to molten data.frame/data.table with multiple entries
#'
#' Takes a molten data.frame/data.table containing no indices as produced by [fhir_melt_all()] and
#' restores its indices from a matrix of indices as produces by [fhir_extract_indices()]. Dimensions as
#' well as row and column ordering of index matrix and data frame have to be identical!
#'
#' @param d A data.frame/data.table as produced by [fhir_melt_all()] without indices.
#' @param index_matrix A character matrix with the same dimensions as `d`, as produced by [fhir_extract_indices()]
#' @return `d` but with the indices from `index_matrix`.
#'
#' @examples
#' #generate example bundle
#'bundle <- xml2::read_xml(
#'	"<Bundle>
#'
#'		<Patient>
#'			<id value='id1'/>
#'			<address>
#'				<use value='home'/>
#'				<city value='Amsterdam'/>
#'				<type value='physical'/>
#'				<country value='Netherlands'/>
#'			</address>
#'			<birthDate value='1992-02-06'/>
#'		</Patient>
#'
#'		<Patient>
#'			<id value='id2'/>
#'			<address>
#'				<use value='home'/>
#'				<city value='Rome'/>
#'				<type value='physical'/>
#'				<country value='Italy'/>
#'			</address>
#'			<address>
#'				<use value='work'/>
#'				<city value='Stockholm'/>
#'				<type value='postal'/>
#'				<country value='Sweden'/>
#'			</address>
#'			<birthDate value='1980-05-23'/>
#'		</Patient>
#'
#'		<Patient>
#'			<id value='id3'/>
#'			<address>
#'				<use value='home'/>
#'				<city value='Berlin'/>
#'			</address>
#'			<address>
#'				<type value='postal'/>
#'				<country value='France'/>
#'			</address>
#'			<address>
#'				<use value='work'/>
#'				<city value='London'/>
#'				<type value='postal'/>
#'				<country value='England'/>
#'			</address>
#'			<birthDate value='1974-12-25'/>
#'			<birthDate value='1978-11-13'/>
#'		</Patient>
#'
#'	</Bundle>"
#')
#'
#'#crack fhir resources
#'patients <- fhir_df_description(resource = "Patient",
#'                                style = fhir_style(brackets = c("[","]"),
#'				                                     sep="||")
#'				                  )
#'dfs <- fhir_crack(bundles = list(bundle),
#'                  design = fhir_design(patients))
#'
#'#Melt multiple entries
#'d <- fhir_melt_all(dfs$patients, brackets = c("[", "]"), sep="||", rm_indices = FALSE)
#'
#'#Extract indices
#'indices <- fhir_extract_indices(d, brackets = c("[", "]"))
#'
#'#remove indices
#'d_removed <- fhir_rm_indices(d, brackets = c("[", "]"))
#'
#'#restore indices
#'d_restored <- fhir_restore_indices(d_removed, indices)
#'
#'#compare
#'identical(d, d_restored)
#'@export
#'
#' @seealso [fhir_melt()], [fhir_melt_all()], [fhir_rm_indices()]


fhir_restore_indices <- function(d, index_matrix){

	if(!is.data.frame(d)){
		stop("You need to supply a data.frame or data.table to the argument indexed_data_frame.",
			 "The object you supplied is of type ", class(d), ".")}

	if(any(dim(d)!=dim(index_matrix))){
		stop("Dimensions of d and index_matrix have to match.")
	}

	names <- colnames(index_matrix)
	if(any(!names %in% names(d))){
		stop("The variable names of d and colnames of index_matrix have to match.")
	}

	is_DT <- data.table::is.data.table(d)

	indices <- as.data.frame(index_matrix)

	#save NA positions because paste0 will paste NA to NANA
	na.pos <- which(is.na(d))


	result <- data.table::data.table()

	for(name in names){
		result[,eval(name) := paste0(indices[[name]], d[[name]])]
	}

	#fix NA values
	matr_result <- as.matrix(result)
	matr_result[na.pos] <- NA

	if(is_DT){data.table::as.data.table(matr_result)}else{
		as.data.frame(matr_result)
	}
}



########################################################################################
########################################################################################


#' Turn a row with multiple entries into a data frame
#'
#' @param row One row of an indexed data frame
#' @param columns A character vector specifying the names of all columns that should be molten simultaneously.
#' It is advisable to only melt columns simultaneously that belong to the same (repeating) attribute!
#' @param brackets A character vector of length 2, defining the brackets used for the indices.
#' @param sep A string, the separator.
#' @param all_columns A logical scalar. Return all columns or only the ones specified in `columns`?
#' @return A data frame with nrow > 1
#' @noRd


melt_row <- function(
	row,
	columns,
	brackets = c("<", ">"),
	sep = " ",
	all_columns = FALSE) {

	row <- as.data.frame(row)

	col.names.mutable  <- columns

	col.names.constant <- setdiff(names(row), col.names.mutable)

	row.mutable  <- row[col.names.mutable]

	row.constant <- row[col.names.constant]

	#dbg
	#row <- d3.3$Entries[ 1, ]

	brackets.escaped <- esc(brackets)

	pattern.ids <- paste0(brackets.escaped[1], "([0-9]+\\.*)+", brackets.escaped[2])

	ids <- stringr::str_extract_all(row.mutable, pattern.ids)

	names(ids) <- col.names.mutable

	pattern.items <- paste0(brackets.escaped[1], "([0-9]+\\.*)+", brackets.escaped[2])

	items <- stringr::str_split(row.mutable, pattern.items)

	items <-
		lapply(items, function(i) {
			if (!all(is.na(i)) && i[1] == "") {
				i[2:length(i)]
			} else {
				i
			}
		})

	names(items) <- col.names.mutable

	d <-
		if (all_columns) {
			row[0, , FALSE]
		} else {
			row[0, col.names.mutable, FALSE]
		}

	for (i in names(ids)) {
		#dbg
		#i<-names( ids )[1]

		id <- ids[[i]]

		if (!all(is.na(id))) {
			it <- items[[i]]

			new.rows        <-
				gsub(paste0(brackets.escaped[1], "([0-9]+)\\.*.*"),
					 "\\1",
					 id)
			new.ids         <-
				gsub(
					paste0(
						"(",
						brackets.escaped[1],
						")([0-9]+)\\.*(.*",
						brackets.escaped[2],
						")"
					),
					"\\1\\3",
					id
				)
			unique.new.rows <- unique(new.rows)

			set <- paste0(new.ids, it)

			f <- sapply(unique.new.rows,
						function(unr) {
							#dbg
							#unr <- unique.new.rows[1]

							fltr <- unr == new.rows

							paste0(set[fltr], collapse = "")
						})

			for (n in unique.new.rows) {
				d[as.numeric(n), i] <- gsub(paste0(esc(sep), "$"), "", f[names(f)==n], perl = TRUE)
			}
		}
	}

	if (0 < length(col.names.constant) && all_columns) {
		if (0 < nrow(d))
			d[, col.names.constant] <-
				dplyr::select(row, col.names.constant)
		else
			d[1, col.names.constant] <-
				dplyr::select(row, col.names.constant)
	}

	data.table::data.table(d)
}

#' This functions melts a row regarding a set
#' of columns completely, i.e. until there is no separator left in the
#' specified columns. It preserves the original ids
#' @noRd
melt_row_completely <- function(
	row,
	columns,
	brackets = c("<", ">"),
	sep = " ",
	all_columns = FALSE){

	setDT(row)
	#extract original ids
	ids <- suppressWarnings(fhir_extract_indices(row, brackets = brackets))
	if(is.matrix(ids)){ids <- as.data.frame(ids)}

	res <- copy(row)

	#melt
	while(any(grepl(esc(sep), res[,columns, with=F]))){
		res <- melt_row(res, brackets = brackets, sep=sep,
								   columns = columns, all_columns = T)
	}

	#reassign original ids
	brackets.escaped <- esc(brackets)
	pattern.ids <- paste0(brackets.escaped[1], "([0-9]+\\.*)*", brackets.escaped[2])

	for(n in columns){
		id <- as.matrix(ids[[n]])

		count <- 0
		for(j in which(!is.na(res[[n]]))){
			count <- count + 1
			pos <- stringr::str_locate(res[[n]][j], pattern.ids)
			cell <- res[j, n, with=F]
			stringr::str_sub(cell, start=pos[1,1], end = pos[1,2]) <- id[1,count]
			res[j, (n):=cell][]
		}

	}
	return(res)
}

#'This function does the same as fhir_melt but returns a data table
#'that is molten completely with regard to the specified columns.
#'It preserves the original ids
#'@noRd

fhir_melt_dt_preserveID <- function(
	indexed_data_frame,
	columns,
	brackets = c("<", ">"),
	sep = " ",
	all_columns = FALSE) {

	if (!all(columns %in% names(indexed_data_frame))) {
		stop("Not all column names you gave match with the column names in the data frame.")
	}

	indexed_dt <- copy(indexed_data_frame) #copy to avoid side effects

	brackets <- fix_brackets(brackets)

	rowlist <- lapply(seq_len(nrow(
		indexed_dt
	)),
	function(row.id) {

		e <-
			melt_row_completely(
				row = indexed_dt[row.id,],
				columns = columns,
				brackets = brackets,
				sep = sep,
				all_columns = all_columns
			)

		e
	})

	d<- data.table::rbindlist(rowlist, fill = TRUE)

	if(nrow(d) == 0) {warning("The brackets you specified don't seem to appear in the indices of the provided data.frame. Returning NULL.")}

	if(!is.null(d) && 0 < nrow(d)) {

		return(d)

	}
}


####This function is wrapped in the generic fhir_melt_all
####
melt_all <- function(indexed_data_frame, sep, brackets, rm_indices = TRUE){

	if(!is.data.frame(indexed_data_frame)){
		stop("You need to supply a data.frame or data.table to the argument indexed_data_frame.",
			 "The object you supplied is of type ", class(indexed_data_frame), ".")}

	is_DT <- data.table::is.data.table(indexed_data_frame)

	#sort columns to make sure columns belonging to the same element are next to each other
	d <- data.table::data.table(indexed_data_frame)
	oldOrder <- copy(names(d))
	data.table::setcolorder(d, sort(names(d)))

	#columns which have multiple values
	targetCols <- grepl(esc(sep), d)
	if(!any(targetCols)){
		warning("There don't seem to be any multiple values in the data. Did you specify the sep argument correctly? ",
				"Returning unaltered data.")
		return(indexed_data_frame)
	}

	#dissect colnames
	names <- stringr::str_split(names(d), esc("."), simplify = T)[targetCols,,drop=F]

	#find the columns which represent new elements
	newElement <- matrix(!apply(names,2, duplicated, incomparables = ""), nrow=nrow(names))
	targetElements <- names[newElement[,1],1]

	#loop through elements
	for (i in 1:length(targetElements)){
		cols <- fhir_common_columns(d, targetElements[i])

		d <- fhir_melt_dt_preserveID(d, columns = cols, brackets=brackets, sep=sep, all_columns = T)

	}

	#remove indices
	if(rm_indices){d <- fhir_rm_indices(d, brackets = brackets)}

	#set old order
	data.table::setcolorder(d, oldOrder)

	#return appropriate type
	if(!is_DT){data.table::setDF(d)}
	d
}
