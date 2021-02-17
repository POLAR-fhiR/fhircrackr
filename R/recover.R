
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


#create empty resource
create_resource <- function(resourceType, rows, brackets){

	colnames <- names(rows)

	indices <- fhir_extract_indices(rows, brackets)
	indices <- data.frame(indices)

	nodes <- strsplit(colnames, esc("."))
	names(nodes) <- colnames

	for(col in colnames){
		ind <- stringr::str_remove_all(indices[[col]], paste0(esc(brackets[1]), "|", esc(brackets[2])))
		freq <- t(data.frame(strsplit(ind, esc("."))))
		freq <- matrix(as.numeric(freq), ncol = length(nodes[[col]]), dimnames = list(NULL, nodes[[col]]))
		#freq <- apply(freq,2, max, na.rm=T)

		nodes[[col]] <- freq

	}

	#create resource
	myres <- xml2::read_xml("<resource> </resource>")
	xml2::xml_add_child(myres, xml2::xml_new_root(resourceType))

	#define nodes
	#nodes <- strsplit(colnames, esc("."))
	depth <- sapply(nodes, ncol)
	max_depth <- max(depth)

	#create one instance of every node
	# current <- sapply(nodes, function(x){colnames(x)[1]})
	# parent <- rep(resourceType, length(current))
	# freq <- sapply(nodes, function(x){x[1]})



	nodes <- lapply(nodes, data.table::as.data.table)
	lapply(nodes, function(x){x[,eval(resourceType):=1]})
	lapply(nodes, function(x){data.table::setcolorder(x, c(resourceType, names(x)[1:(ncol(x)-1)]))})

	#
	# nodes_reduced <- lapply(nodes, function(x){
	# 							res <-copy(x)
	# 		        			res <- res[,max(res[,1], na.rm=T)]
	# 	        				res <- as.data.table(res)
	# 	        				names(res) <- names(x)[1]
	# 	        				res
	# 	        				 })


	for(j in 1: max_depth){

		nodes_reduced <- lapply(nodes, function(x){
			n <- copy(x)
			if(ncol(n) > j){
				res <- n[,max(n[,eval(j+1),with=F], na.rm=T), by=eval(names(n)[j])]
				res <- na.omit(res)
				names(res) <- names(x)[j:(j+1)]
				res
			}
		})

		nodes_reduced <- nodes_reduced[!sapply(nodes_reduced, is.null)]

		item <- sapply(nodes_reduced, function(x){paste(c(colnames(x)[1], colnames(x)[2]), collapse="/")})

		item_list <- list()
		for(i in unique(item)){
			res <- Reduce(rbind, nodes_reduced[item==i])
			res <- unique(res)
			item_list <- append(item_list, list(res))
		}

		# item==item_unique[2]
		# freq <- data.table::as.data.table(sapply(nodes_reduced, function(x)x[,j, with=F]))
		# names(freq) <- item
#
# 		#summarise columns belonging to the same item
# 		duplicates <- unique(names(freq)[duplicated(names(freq))])
#
# 		for(dup in duplicates){
# 			cols_reduced <- apply(freq[,names(freq) %in% dup, with=F],1,function(x)unique(na.omit(x)))
# 			freq[,eval(dup):=cols_reduced]
# 		}
#
# 		freq <- freq[,!duplicated(names(freq)), with=F]

		#create nodes
		# for(i in 1:ncol(freq)){
		# 	parent_node <- xml2::xml_find_all(myres, parents_reduced[i])
		#
		# 	for(r in 1:nrow(freq)){
		# 		for(k in 1:as.numeric(freq[eval(r),eval(i), with=F])){
		# 		 xml2::xml_add_child(parent_node, xml2::xml_new_root(names(freq)[i]))
		# 		 }
		#
		# 	}
		#
		#
		# }

		for(i in 1:length(item_list)){
			item <- item_list[[i]]
			parent_node <- xml2::xml_find_all(myres, names(item)[1])
			for(j in 1:nrow(item)){
				frq <- as.numeric(item[eval(j),])
				for(k in 1:frq[2]){
					xml2::xml_add_child(parent_node[frq[1]], xml2::xml_new_root(names(item)[2]))
				}
			}
		}
		#remove current first column for every element of nodes
#
#
# 		parent <- parent[!sapply(nodes_reduced, is.null)]
#
# 		parent <- paste(parent, sapply(nodes_reduced, function(x){names(x)[j]}), sep="/")
#
# 		#nodes <- lapply(nodes, function(x){x[,-1, drop=F]})
# 		#nodes <- nodes[!sapply(nodes, function(x)ncol(x)==0)]
#
# 		parent_red <- parent[!is.na(current)]
# 		freq_red <- freq[!is.na(current)]
# 		current_red <- current[!is.na(current)]
#
# 		parent_red <- parent_red[!duplicated(current_red)]
# 		freq_red <- freq_red[!duplicated(current_red)]
# 		current_red <- current_red[!duplicated(current_red)]
#
#
# 		for (i in 1:length(current_red)){
# 			parent_node <- xml2::xml_find_all(myres, parent_red[i])
# 			for(k in 1:freq_red[i]){
# 				xml2::xml_add_child(parent_node, xml2::xml_new_root(current_red[i]))
# 			}
#
# 		}
#
# 		parent <- paste(parent,current, sep="/")
# 		current <- sapply(nodes, function(x){names(x)[j+1]})
# 		freq <- sapply(nodes, function(x){x[j+1]})
#
# 	}
#
# 	if(any(single_nodes_dt$freqency > 1)){
#
# 		multiple <- single_nodes_dt[freqency>1]
#
#
#
#
# 		#create appropriate number of siblings for every node with multiple entries:
# 		#reduce to nodes with multiple entries
# 		nodes_df <- as.data.frame(stringr::str_split(colnames, esc("."), simplify = T))
# 		setDT(nodes_df)
# 		nodes_df[,depth:=apply(nodes_df,1,function(x){sum(x!="")})]
# 		nodes_df[,frequency:=frequency]
#
# 		multiple <- nodes_df[frequency > 1,]
# 		multiple[,max_freq:=max(frequency), by=.(V1)]
#
# 		#indices <- fhir_extract_indices(multiple, brackets = brackets  )
#
#
# 		elements <- unlist(c(unique(multiple[,1])))
#
# 		for(element in elements){
# 			multiple_sub <- multiple[multiple$V1==element,]
# 			# bool <- matrix(apply(multiple_sub[,1:(ncol(multiple_sub)-3)], 2, duplicated, incomparables=""),
# 			# 			   ncol=ncol(multiple_sub)-3)
# 			mult <- multiple_sub[, grepl("V", names(multiple_sub)), with=F]
# 			mult <- mult[,apply(mult,2, function(x){length(unique(x))==1}), with=F]
#
# 			multiple_paths <- paste(mult[1,], collapse = esc("/"))
# 			if(stringr::str_sub(multiple_paths, -1) =="/"){ multiple_paths  <- stringr::str_sub(multiple_paths , 1,-2)}
# #
# # 			#paste all elements up to the first FALSE
# # 			for(k in 1:nrow(multiple_sub)){
# # 				multiple_paths <- c(multiple_paths,
# # 									paste(multiple_sub[k,1:(min(which(bool[k,]==F))-1)], collapse=esc("/")))
# # 			}
# #
# # 			multiple_dt <- data.table(multiple_paths, max_freq=multiple_sub$max_freq)
# # 			#	multiple_dt <- multiple_dt[apply(bool, 1, function(x)sum(x)>0) | multiple$depth==1,]
# # 			multiple_dt <- multiple_dt[!duplicated(multiple_paths),]#remove duplicates
#
# 			#create siblings
# 			for(l in 1:(multiple_sub$max_freq[1]-1)){
# 				node <- xml2::xml_child(myres, paste(c(resourceType,multiple_paths), collapse = esc("/")))
# 				xml2::xml_add_sibling(node, node)
#
# 			}
#


#		}

	# 	#find node which needs to be duplicated
	# 	bool <- matrix(apply(multiple[,1:(ncol(multiple)-3)], 2, duplicated, incomparables=""),ncol=ncol(multiple)-3)
	#
	# 	multiple_paths <- c()
	#
	# 	#paste all elements up to the first FALSE
	# 	for(k in 1:nrow(multiple)){
	# 	#	if(sum(bool[k,])>0 | multiple[k,]$depth==1){ #gets rid of parent node for depth > 1
	# 			multiple_paths <- c(multiple_paths,
	# 								paste(multiple[k,1:(min(which(bool[k,]==F))-1)], collapse=esc("/")))
	# 	#	}
	#
	# 	}
	#
	# 	multiple_dt <- data.table(multiple_paths, max_freq=multiple$max_freq)
	# #	multiple_dt <- multiple_dt[apply(bool, 1, function(x)sum(x)>0) | multiple$depth==1,]
	# 	multiple_dt <- multiple_dt[!duplicated(multiple_paths),]#remove duplicates
	#
	# 	#create siblings
	# 	for(l in 1:nrow(multiple_dt)){
	# 		node <- xml2::xml_child(myres, paste(c(resourceType,multiple_dt$multiple_paths[l]), collapse = esc("/")))
	#
	# 		for(m in 1:(multiple_dt$max_freq[l]-1)){
	# 			xml2::xml_add_sibling(node, node)
	# 		}
	# 	}
	 }

	myres

}

#fill
fill_resource <- function(resource, rows){

	resourceType <- xml2::xml_name(xml2::xml_child(resource, 1))

	elements <- unique(stringr::str_split(names(rows), esc("."), simplify = T)[,1])



	for(element in elements){
		cols <- fhir_common_columns(rows, element)
		rows_sub <- rows[,cols, with=F]
		rows_sub <- unique(rows_sub)

		depth <- stringr::str_count(names(rows_sub),esc("."))+1

		for(j in unique(depth)){
			sub <- rows_sub[,depth==j, with=F]
			sub <- unique(sub)

			paths <- gsub(esc("."), esc("/"),names(sub))
			paths <- paste(resourceType, paths, sep="/")

			for(i in 1:ncol(sub)){
				nodes <- xml2::xml_find_all(resource, paths[i])

				xml2::xml_attr(nodes, "value")  <- unlist(c(sub[,i, with=F]))
				xml2::xml_remove(nodes[is.na(unlist(c(sub[,i, with=F])))])

			}

		}

		# paths <- gsub(esc("."), esc("/"),names(rows_sub))
		# paths <- paste(resourceType, paths, sep="/")
		#
		# for(i in 1:ncol(rows_sub)){
		# 	nodes <- xml2::xml_find_all(resource, paths[i])
		#
		# 	xml2::xml_attr(nodes, "value")  <- unlist(c(rows_sub[,i, with=F]))
		# 	xml2::xml_remove(nodes[is.na(unlist(c(rows_sub[,i, with=F])))])
		#
		# }
	}


}


#
create_bundle <- function(resourceType, df){

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
		#create and fill resource
		xml2::xml_add_child(bundle, create_resource(resourceType = resourceType, rows = rows))
		fill_resource(xml2::xml_child(bundle, i+1), rows = rows)
		#create entry and request nodes
		xml2::xml_add_parent(xml2::xml_child(bundle, i+1), xml2::xml_new_root("entry"))
		xml2::xml_add_child(xml2::xml_child(bundle, i+1), xml2::xml_new_root("request"))
		xml2::xml_add_child(xml2::xml_find_all(bundle, "entry/request")[[i]], xml2::xml_new_root("method", value="PUT"))
		xml2::xml_add_child(xml2::xml_find_all(bundle, "entry/request")[[i]],
							xml2::xml_new_root("url",
											   value=paste(resourceType,
											   			xml2::xml_attr(xml2::xml_child(bundle, "entry/resource/*/id"), "value"),
											   			sep="/")))
	}

	bundle
}

