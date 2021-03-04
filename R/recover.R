#To stop devtools::check( from warning about no visible global function definition)
globalVariables(".")


#'This function does the same as melt_row but returns a data table with
#'original indices
#'
#'@noRd

melt_row_preserveID <-
	function(row,
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

		pattern.ids <- paste0(brackets.escaped[1], "([0-9]+\\.*)*", brackets.escaped[2])

		ids <- stringr::str_extract_all(row.mutable, pattern.ids)
		names(ids) <- col.names.mutable

		items <- stringr::str_split(row.mutable, pattern.ids)

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
					d[n, i] <- gsub(paste0(esc(sep), "$"), "", f[n], perl = TRUE)
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
#' of columns completely, i.e. until there is no seperator left in the
#' specified columns. It preserves the original ids
#' @noRd
melt_row_completely <- function(row,
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
		res <- melt_row_preserveID(res, brackets = brackets, sep=sep,
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

fhir_melt_dt_preserveID <-
	function(indexed_data_frame,
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


#'Create single FHIR resource from data.frame/data.table
#'
#'This function takes a data.frame with 1 or more rows containing information for a single
#'FHIR resource and builds the FHIR resource as a xml-object.
#'When there are multiple entries to some of the elements of the resource, the input data
#'should be rows from a molten data.frame as produced by [fhir_melt_all()] with
#'`rm_indices=FALSE`. When there are no multiple entries, the data still must contain
#'indices as produced by [fhir_crack()] when brackets are provided in the design.
#'
#'The column names of the input data must reflect the path to the corresponding resource element
#'with {.} as a seperator, e.g. `code.coding.system`. These names are produced automatically
#'by [fhir_crack()] when the `cols` element of the design is omitted.
#'
#' @param resourceType A string naming the resource Type of the FHIR resource, e.g. "Patient".
#' @param data A data.frame or data.table containing one ore more rows with information on the
#' FHIR resource. Ideally, this should have been produced by [fhir_crack()] with the `cols`
#' element of the design omitted in combination with [fhir_melt_all()] with
#' `rm_indices=FALSE`. If you wish to manipulate you data before remodeling the FHIR resource
#' you can store the indices using [fhir_extract_indices()], remove them temporarily
#' with [fhir_rm_indices()] and restore them again with [fhir_restore_indices()]
#' before the use of `fhir_create_resource`.
#' @param brackets A character vector of length two specifying the brackets that surround the indices,
#' should match the brackets used in [fhir_crack()].
#'
#' @return An xml object containing a single FHIR resource of Type `resourceType`. This can
#' be saved using [xml2::write_xml()].
#'
#' @examples
#' #generate example
#' bundle <- xml2::read_xml(
#' 	"<Bundle>
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
#' 	</Bundle>"
#' )
#'
#' #crack bundle
#' design <- list(Patient = list(
#'                              resource = "//Patient",
#'                              style = list(brackets = c("[", "]"),
#'                                           sep="||")))
#'
#' dfs <- fhir_crack(list(bundle), design)
#'
#'#melt multiple entries
#' d <- fhir_melt_all(dfs$Patient, sep="||", brackets = c("[", "]"),
#'                    rm_indices = FALSE)
#'
#'#create resource
#' resource <- fhir_create_resource(resourceType = "Patient",
#'                                  data = d, brackets = c("[", "]"))
#'
#'#save/inspect resource
#'library(xml2)
#'tmp <- tempfile(fileext = ".xml")
#'write_xml(resource, tmp)
#'readLines(tmp)
#'
#' @export
fhir_create_resource <- function(resourceType, data, brackets){

	if(!is.data.frame(data)){stop("Input data must be of class data.frame or data.table.")}
	rows<- copy(data)
	data.table::setDT(rows)

	#remove entirely empty columns
	na.cols <- rows[, .(which(apply(is.na(.SD),2,all)))]
	if(nrow(na.cols)>0){rows[, unlist(na.cols):=NULL]}

	colnames <- names(rows)

	indices <- fhir_extract_indices(rows, brackets)
	indices <- data.frame(indices)

	nodes <- strsplit(colnames, esc("."))
	names(nodes) <- colnames

	values <- fhir_rm_indices(rows, brackets = brackets)

	#find out how often every element appears
	for(col in colnames){
		ind <- stringr::str_remove_all(indices[[col]], paste0(esc(brackets[1]), "|", esc(brackets[2])))
		freq <- t(data.frame(strsplit(ind, esc("."))))
		freq <- matrix(as.numeric(freq), ncol = length(nodes[[col]]), dimnames = list(NULL, nodes[[col]]))
		nodes[[col]] <- freq

	}

	#create resource
	myres <- xml2::read_xml("<resource> </resource>")
	xml2::xml_add_child(myres, xml2::xml_new_root(resourceType))

	#define nodes
	depth <- sapply(nodes, ncol)
	max_depth <- max(depth)
	nodes <- lapply(nodes, data.table::as.data.table)
	lapply(nodes, function(x){x[,eval(resourceType):=1]}) #add root node
	lapply(nodes, function(x){data.table::setcolorder(x, c(ncol(x),1:(ncol(x)-1)))})#put root node in front


	#loop through resource depth
	for(j in 1: max_depth){

		#only look at current and previous level of current j
		nodes_reduced <- lapply(nodes, function(x){
			n <- copy(x)
			if(ncol(n) > j){
				res <- n[,max(n[,eval(j+1),with=F], na.rm=T), by=eval(names(n)[j])]
				res <- stats::na.omit(res)
				names(res)[1] <- paste(names(x)[1:j], collapse="/")
				names(res)[2] <- names(x)[j+1]
				res
			}
		})

		nodes_reduced <- nodes_reduced[!sapply(nodes_reduced, is.null)]

		#create list containing information for every node to produce
		item <- sapply(nodes_reduced, function(x){paste(c(colnames(x)[1], colnames(x)[2]), collapse="/")})
		item_list <- list()
		for(i in unique(item)){
			res <- Reduce(rbind, nodes_reduced[item==i])
			res <- unique(res)
			item_list <- append(item_list, list(res))
		}

		#create appropriate number of nodes
		for(i in 1:length(item_list)){
			item <- item_list[[i]]
			parent_node <- xml2::xml_find_all(myres, names(item)[1])
			for(l in 1:nrow(item)){
				frq <- as.numeric(item[eval(l),])
				for(k in 1:frq[2]){


					#if created node is in colnames of rows: fill with appropriate value
					current_node <- paste(names(item), collapse="/")
					current_node <- stringr::str_remove(current_node, paste0(resourceType, "/"))
					current_node <- stringr::str_replace_all(current_node, "/", ".")

					if(current_node %in% names(rows)){#node is terminal and needs a value
						#find correct value
						values <- rows[[current_node]]

						if(j==1){#pattern is different on first level
							pattern <- paste0(esc(brackets)[1], k, esc(brackets)[2])
						}else{
							pattern <- paste0(esc(brackets)[1],
											  "([[:digit:]]\\.){", j-2, "}",
											  paste(c(frq[1],k), collapse="."),
											  esc(brackets)[2])
						}

						value <- unique(values[grepl(pattern, values)])
						value <- stringr::str_remove(value, pattern)

						#add and fill node
						xml2::xml_add_child(parent_node[frq[1]], xml2::xml_new_root(names(item)[2], value=value))
					}else{#node is a parent and doesnt need value
						xml2::xml_add_child(parent_node[frq[1]], xml2::xml_new_root(names(item)[2]))

					}

				}
			}
		}

	 }

	myres

}

#' Create transaction/batch bundle with FHIR resources to post on a FHIR server
#'
#'This function takes a data.frame/data.table containing information for several
#'FHIR resources of the same resource type and builds a transaction bundle of the
#'FHIR resources as a xml-object.
#'
#'When there are multiple entries to some of the elements of the resource, the input data
#'should be rows from a molten data.frame as produced by [fhir_melt_all()] with
#'`rm_indices=FALSE`. When there are no multiple entries, the data still must contain
#'indices as produced by [fhir_crack()] when brackets are provided in the design.
#'
#'The column names of the input data must reflect the path to the corresponding resource element
#'with {.} as a separator, e.g. `code.coding.system`. These names are produced automatically
#'by [fhir_crack()] when the `cols` element of the design is omitted.
#'
#' @param resourceType A string naming the resource Type of the FHIR resources, e.g. "Patient".

#' @param data A data.frame or data.table containing information on the
#' FHIR resources. Ideally, this should have been produced by [fhir_crack()] with the `cols`
#' element of the design omitted in combination with [fhir_melt_all()] with
#' `rm_indices=FALSE`. If you wish to manipulate you data before remodeling the FHIR resource
#' you can store the indices using [fhir_extract_indices()], remove them temporarily
#' with [fhir_rm_indices()] and restore them again with [fhir_restore_indices()]
#' before the use of `fhir_create_bundle`.
#'
#' @param brackets A character vector of length two specifying the brackets that surround the indices,
#' should match the brackets used in [fhir_crack()].
#'
#' @param bundleType A string defining the bundle type, should be either `"transaction"` or `"batch"`.
#' See <https://www.hl7.org/fhir/http.html#transaction> for an explanation of the two.
#'
#' @param requestMethod A string defining the request Method that will be applied to every individual resource.
#' Should be either `"PUT"`, resulting in an *update*, <https://www.hl7.org/fhir/http.html#update>
#' or `"POST"`, resulting in a *create*, <https://www.hl7.org/fhir/http.html#create>.
#'
#' @return An xml object containing a transaction/batch bundle. This can be saved using [xml2::write_xml()]
#' and be posted to the FHIR server outside of R or inside of R using [httr::POST()].
#'
#' @examples
#' #unserialize example bundles
#' bundles <- fhir_unserialize(patient_bundles)
#'
#' #crack example bundles
#' design <- list(Patient = list(
#'                              resource = "//Patient",
#'                              style = list(brackets = c("[", "]"),
#'                                           sep="||")))
#'
#' dfs <- fhir_crack(bundles, design)
#'
#'#melt multiple entries
#' d <- fhir_melt_all(dfs$Patient, sep="||", brackets = c("[", "]"),
#'                    rm_indices = FALSE)
#'
#'#create bundle
#'result <- fhir_create_bundle(resourceType = "Patient", data = d, brackets = c("[", "]"))
#'
#'#save bundle
#'library(xml2)
#'tmp <- tempfile(fileext = ".xml")
#'write_xml(result, tmp)
#'
#'\donttest{
#'#post bundle
#'library(httr)
#'POST(url = "http://fhir.hl7.de:8080/baseDstu3", body = upload_file(tmp))
#'}
#'@export

fhir_create_bundle <- function(resourceType, data, brackets, bundleType = "transaction", requestMethod = "PUT"){

	d <- data.table::copy(data)
	setDT(d)

	#remove any narrative elements https://www.hl7.org/fhir/narrative.html
	if(any(grepl("^text", names(d)))){
		d[,fhir_common_columns(d, "text"):=NULL]
	}


	#create transaction bundle
	bundle <- xml2::xml_new_root("Bundle")
	xml2::xml_add_child(bundle, xml2::xml_new_root("type", value= bundleType))


	resources <- unique(d$id)
	setkey(d, "id")

	for(i in 1:length(resources)){
		rows <- d[resources[i]]
		#create  resource
		xml2::xml_add_child(bundle, fhir_create_resource(resourceType = resourceType, data = rows, brackets = brackets))
		#create entry and request nodes
		xml2::xml_add_parent(xml2::xml_child(bundle, i+1), xml2::xml_new_root("entry"))
		xml2::xml_add_child(xml2::xml_child(bundle, i+1), xml2::xml_new_root("request"))
		xml2::xml_add_child(xml2::xml_find_all(bundle, "entry/request")[[i]], xml2::xml_new_root("method", value=requestMethod))

		if(requestMethod=="PUT"){
			xml2::xml_add_child(xml2::xml_find_all(bundle, "entry/request")[[i]],
								xml2::xml_new_root("url",
												   value=paste(resourceType,
												   			xml2::xml_attr(xml2::xml_find_all(bundle, "entry/resource/*/id")[i], "value"),
												   			sep="/")))
		}

	}

	if(requestMethod=="POST"){
		ids <- xml2::xml_find_all(bundle, "entry/resource/*/id")
		xml2::xml_remove(ids)
	}

	bundle
}

