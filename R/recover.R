
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


#create resource
create_resource <- function(resourceType, data, brackets){

	rows<- copy(data)

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
				res <- na.omit(res)
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

#fill
# fill_resource <- function(resource, rows, brackets){
#
# 	resourceType <- xml2::xml_name(xml2::xml_child(resource, 1))
#
# 	elements <- unique(stringr::str_split(names(rows), esc("."), simplify = T)[,1])
#
# 	brackets.escaped <- esc(brackets)
# 	pattern.ids <- paste0(brackets.escaped[1], "([0-9]+\\.*)*", brackets.escaped[2])
#
#     #loop trough resource elements
# 	for(element in elements){
# 		cols <- fhir_common_columns(rows, element)
# 		rows_sub <- rows[,cols, with=F]
# 		rows_sub <- unique(rows_sub)
#
# 		depth <- stringr::str_count(names(rows_sub),esc("."))+1
#
# 		#loop through all levels of the element
# 		for(j in unique(depth)){
#
# 			#current level
# 			sub <- rows_sub[,depth==j, with=F]
# 			sub <- unique(sub)
#
# 			paths <- gsub(esc("."), esc("/"),names(sub))
# 			paths <- paste(resourceType, paths, sep="/")
#
#
# 			for(i in 1:ncol(sub)){
# 				nodes <- xml2::xml_find_all(resource, paths[i])
# 				values <- na.omit(sub[[i]])
# 				#determine position of every value in parent node list
# 				if(j==1){#position doesnt matter on first level, there is always just one parent (the resource)
# 					pos <- 1:length(values)
# 				}else{
# 					levelpos <- stringr::str_length(brackets)[1] + (j-1) +(j-2)
# 					pos <- as.numeric(stringr::str_sub(values, levelpos, levelpos))
# 				}
# 				values <- stringr::str_remove_all(values, pattern.ids)
#
# 				xml2::xml_attr(nodes, "value")  <- values[pos]
#
# 			}
#
# 		}
#
# 	}
#
#
# }
#

#
create_bundle <- function(resourceType, df, brackets = brackets){

	d <- data.table::copy(df)
	setDT(d)

	#remove any narrative elements https://www.hl7.org/fhir/narrative.html
	if(any(grepl("^text", names(d)))){
		d[,fhir_common_columns(d, "text"):=NULL]
	}


	#create transaction bundle
	bundle <- xml2::xml_new_root("Bundle")
	xml2::xml_add_child(bundle, xml2::xml_new_root("type", value= "transaction"))


	resources <- unique(d$id)
	setkey(d, "id")

	for(i in 1:length(resources)){
		rows <- d[resources[i]]
		#create  resource
		xml2::xml_add_child(bundle, create_resource(resourceType = resourceType, data = rows, brackets = brackets))
		#create entry and request nodes
		xml2::xml_add_parent(xml2::xml_child(bundle, i+1), xml2::xml_new_root("entry"))
		xml2::xml_add_child(xml2::xml_child(bundle, i+1), xml2::xml_new_root("request"))
		xml2::xml_add_child(xml2::xml_find_all(bundle, "entry/request")[[i]], xml2::xml_new_root("method", value="PUT"))
		xml2::xml_add_child(xml2::xml_find_all(bundle, "entry/request")[[i]],
							xml2::xml_new_root("url",
											   value=paste(resourceType,
											   			xml2::xml_attr(xml2::xml_find_all(bundle, "entry/resource/*/id")[i], "value"),
											   			sep="/")))
	}

	bundle
}

