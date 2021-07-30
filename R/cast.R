library(fhircrackr)
library(data.table)
library(dplyr)

lst <- function(v){names(v)<-v;v}
esc <- function(s) {gsub("([\\.|\\^|\\$|\\*|\\+|\\?|\\(|\\)|\\[|\\{|\\\\\\|\\|])", "\\\\\\1", s)}
esc_xml <- function(s) {
	gsub("\"", "&quot;",
		 gsub("'", "&apos;",
		 	 gsub("<", "&lt;",
		 	 	 gsub(">", "&gt;",
		 	 	 	 gsub("&", "&amp;", s)
		 	 	 )
		 	 )
		 )
	)
}

desc_xml <- function(s) {
	gsub("&quot;", "\"",
		 gsub("&apos;", "'",
		 	 gsub("&lt;", "<",
		 	 	 gsub("&gt;", ">",
		 	 	 	 gsub("&amp;", "&", s)
		 	 	 )
		 	 )
		 )
	)
}
inc_tab <- function(tab, add = "....") paste0(tab, add)
dec_tab <- function(tab, sub = "....") substr(tab, 1, nchar(tab) - nchar(sub))

vlist <- function(.value, ...) {
	list <- list(...)
	attr(list, "value") <- .value
	list
}

frame_string <- function(text = "\nHello !!!\n\n\nIs\nthere\n\nA N Y O N E\n\nout\nthere\n???\n ", pos = c("left", "center", "right")[1], edge = " ", hori = "-", vert = "|") {
	edge <- rep_len(strsplit(edge, "")[[1]], 4)[1 : 4]
	side <- c("right", "both", "left")[match(pos, c("left", "center", "right"))]
	r <- ""
	s <- strsplit(text, "\n")[[1]]
	h <- length(s)
	w <- max(sapply(s, nchar))
	hbt <- paste0(edge[1], paste0(rep_len(hori, w + 2), collapse = ""), edge[2], "\n")
	hbb <- paste0(edge[3], paste0(rep_len(hori, w + 2), collapse = ""), edge[4], "\n")
	r <- hbt
	for(s_ in s) {
		r <- paste0(r, vert, " ", stringr::str_pad(string = s_, width = w, side = side, " "), " ", vert, "\n")
	}
	r <- paste0(r, hbb)
	r
}

fhir_cast <- function(indexed_df = df.patients, sep = desc.patients@style@sep, brackets = desc.patients@style@brackets, keep_1st_index = F, shift_index = 0, use_brackets = F, verbose = 1) {
	if(!inherits(indexed_df, "data.table")) setDT(indexed_df)

	df_names <- names(indexed_df)
	s <- strsplit(df_names, "\\.")
	names(s) <- df_names
	sep_ <- esc(sep)
	bra_ <- esc(brackets[1])
	ket_ <- esc(brackets[2])
	regexpr_ids <- paste0(bra_, "([0-9]+(\\.[0-9]+)*)", ket_, "(.*$)")
	if(0 < verbose) cat("Pass 1\n")
	map <- sapply(
		names(indexed_df),
		function(name) {
			#name <- names(indexed_df)[[6]]
			if(0 < verbose) cat(name, "\n")
			entries <- strsplit(indexed_df[[name]], sep_)
			ids <- lapply(entries, function(entry)gsub(regexpr_ids, "\\1", entry))
			name_vec <- strsplit(name, "\\.")[[1]]
			u <- unlist(
				lapply(
					ids[sapply(ids, function(i) all(!is.na(i)))],
					function(id){
						#id <- ids[sapply(ids, function(i) all(!is.na(i)))][[2]]
						if(1 < length(name_vec)) {
							sapply(
								id,
								function(i) {
									#i <- id[[2]]
									i_ <- strsplit(i, "\\.")[[1]]
									i_ <- as.numeric(i_)
									ones_ <- 1 == i_
									i_ <- i_ + shift_index
									if(!keep_1st_index) {
										i_[ones_] <- ""
									}
									if(use_brackets) {
										bras_ <- rep_len("[", length(i_))
										kets_ <- rep_len("]", length(i_))
										if(!keep_1st_index) {
											bras_[ones_] <- ""
											kets_[ones_] <- ""
										}
										paste0(paste0(name_vec, bras_, i_, kets_), collapse = ".")
									} else {
										paste0(paste0(name_vec, i_), collapse = ".")
									}
								},
								simplify = F
							)
						} else {
							if(1 < id || keep_1st_index) {
								i <- as.numeric(id) + shift_index
								a <- if(use_brackets) {
									paste0(name_vec, "[", i, "]")
								} else {
									paste0(name_vec, i)
								}
								names(a) <- id
								a
							} else {
								i <- as.numeric(id) + shift_index
								a <- rep_len(name_vec, length(id))
								names(a) <- id
								a
							}
						}
					}
				),
				use.names = T
			)
			sort(u[unique(names(u))])
		},
		simplify = F
	)
	df_new_names <- unlist(map, use.names = F)
	d <- data.table(matrix(data = rep_len(character(), nrow(indexed_df) * length(df_new_names)), nrow = nrow(indexed_df), ncol = length(df_new_names))) %>% setnames(df_new_names)
	if(0 < verbose) cat("Pass 2\n")
	for(name in names(map)) {
		#name <- names(map)[[1]]
		if(0 < verbose) cat(paste0(name, ":\n"))
		for(id in names(map[[name]])) {
			#id <- names(map[[name]])[[1]]
			sname <- map[[name]][[id]]
			if(0 < verbose) cat("  ", sname, "\n")
			id_str <- paste0(bra_, id, ket_)
			row_with_id <- grep(id_str, indexed_df[[name]], perl = T)
			entries <- strsplit(indexed_df[[name]][row_with_id], sep_)
			values <- gsub(
				id_str,
				"",
				sapply(
					entries,
					function(entry) {
						entry[grep(id_str, entry, perl = T)]
					},
					simplify = F
				)
			)
			d[row_with_id, (sname) := values]
		}
	}
	#setcolorder(x = d, neworder = sort(names(d)))

	d
}


build_tree <- function(row =  df.patients_cast[4,], root = "Bundle", keep_nas = F) {
	tree <- function(col_names, tre, value = 1) {
		len <- length(col_names)
		if(is.null(tre)) tre <- list()
		if(len == 0) {
			setattr(tre, "value", value)
		} else {
			tr <- tree(col_names = col_names[-1], tre = tre[[col_names[1]]], value = value)
			tre[[col_names[1]]] <- tr
		}
		tre
	}
	tre <- list()
	row <- sapply(row, function(x)x)
	if(!keep_nas) row <- row[!is.na(row)]
	names(row) <- paste0(root, ".", names(row))
	for(col_name in names(row)) {
		value <- row[[col_name]]
		col_names_split <- strsplit(col_name, "\\.")[[1]]
		if(length(col_names_split) == 1) {
			tr <- list()
			setattr(tr, "value", value)
		} else {
			tr <- tree(col_names = col_names_split[-1], tre = tre[[col_names_split[[1]]]], value = value)
		}
		tre[[col_names_split[1]]] <- tr
	}
	tre
}

build_tree_bundles <- function(df = df.patients_cast, resource_name = "Patient", bundle_size = 50) {
	bundles <- list()
	b <- 0
	i <- 1
	while(i <= nrow(df)) {
		end_ <- min(c(i - 1 + bundle_size, nrow(df)))
		bundle <- list()
		j <- i
		while(i <= end_) {
			row <- df[i,]
			bundle <- c(bundle, rm_ids_from_tree(build_tree(row = row, root = resource_name, keep_nas = F)))
			i <- i + 1
		}
		bundle <- list(Bundle = bundle)
		bundles[[paste0("Bundle", b)]] <- bundle
		b <- b + 1
		cat(paste0("Bundle ", b, " a ", i - j, " ", resource_name, "s  \u03A3 ", resource_name, "s = ", i - 1, "\n"))
	}
	bundles
}

tree2string <- function(tre = tree.patients_cast, str = "", tab = "", add = "  ") {
	for(i in seq_along(tre)) {
		n <- names(tre)[i]
		#n<-names(tre)[[1]]
		tr <- tre[[i]]
		str <- paste0(str, tab, n)
		a <- attr(tr, "value")
		if(!is.null(a)) {
			str <- paste0(str, " : ", a)
		}
		str <- tree2string(tre = tr, str = paste0(str, "\n"), tab = inc_tab(tab, add), add = add)
	}
	str
}

tree2xml <- function(tre = tree.patients_cast, escaped = T, str = "", tab = "", add = "  ") {
	for(i in seq_along(tre)) {
		s <- ""
		#i<-1
		n <- names(tre)[i]
		#n<-names(tre)[[1]]
		tr <- tre[[i]]

		s <- paste0(tab, "<", n)
		a <- attr(tr, "value")
		if(!is.null(a)) {
			s <- paste0(s, " value=\"", if(escaped) esc_xml(a) else a, "\"")
		}
		s <- if(length(tr) == 0) paste0(s, "/>") else paste0(s, ">")
		s <- tree2xml(tre = tr, escaped = escaped, str = paste0(s, "\n"), tab = inc_tab(tab, add), add = add)
		if(0 < length(tr)) s <- paste0(s, tab, "</", n, ">\n")
		str <- paste0(str, s)
	}
	str
}


xml2_tree2string <- function(tre = t2) {
	s <- toString(xml2::as_xml_document(tre))
	s <- gsub("^[^(\\\n)]+\\\n", "", s)
	s <- gsub("<\\/[^(\\\n)]+", "", s)
	s <- gsub(" +\\\n", "", s)
	s <- gsub("\"", "", s)
	s <- gsub("value=", ": ", gsub("(<)|(</)", "", gsub("(>)|(/>)", "", s)))
	cat(s)
}

tree2json <- function(tree, tab = "", add = "  ") {
	get_arrays <- function(tree) {
		s <- ""
		names_of_siblings <- names(tree)
		names_of_siblings_without_ids <- gsub("(\\[[0-9]+]$)|([0-9]+$)", "", names_of_siblings)
		arrays_indices <- sapply(
			simplify = F,
			unique(names_of_siblings_without_ids),
			function(n) {
				#n <- unique(names_of_siblings_without_ids)[[1]]
				which(names_of_siblings_without_ids == n)
			}
		)
		names_of_arrays <- names(arrays_indices)
		arrays_indices
	}
	dict <- function(tree, tab = "", add = "    ") {
		s <- "{ \n"
		arrays <- get_arrays(tree)
		len <- length(arrays)
		for(i in seq_len(len)) {
			name_of_array <- names(arrays)[[i]]
			s <- paste0(s, inc_tab(tab, add), "\"", name_of_array, "\": ")
			array <- arrays[[name_of_array]]
			s <- paste0(s, arr(tree[array], tab, add))
			if(i != len) s <- paste0(s, ",\n")
		}
		s <- paste0(s, "\n", tab, "}")
	}
	arr <- function(tree, tab = "", add = "    ") {
		s <- "[ "
		len <- length(tree)
		for(i in seq_len(len)) {
			tre <- tree[[i]]
			s <- paste0(s, tree2json(tre, inc_tab(tab, add), add))
			if(i != len) s <- paste0(s, ", ")
		}
		s <- paste0(s, " ]")
	}
	s <- ""
	a <- attr(tree, "value")
	if(!is.null(a)) {
		s <- paste0(s, "\"", a, "\"")
		return(s)
	}
	if(length(names(tree)) == 0) {
		s <- paste0(s, arr(tree, tab, add))
	} else {
		s <- paste0(s, dict(tree, tab, add))
	}
}

print_tree <- function(tre, tab = "") {
	cat(tree2string(tre = tre, str = "", tab = tab))
}

rm_ids_from_tree <- function(tre = tree.patients_cast) {
	if(!is.null(names(tre))) {
		for(n in names(tre)) {
			#n <- names(tre)[[1]]
			tre[[n]] <- rm_ids_from_tree(tre = tre[[n]])
		}
		names(tre) <- gsub("(\\[[0-9]+])|([0-9]+)", "", names(tre))
	}
	tre
}

build_xml_bundles <- function(cast_table = cast_table_obs, resource_name="Observation", bundle_size = 500) {
	max_ <- nrow(cast_table)
	i <- 1
	b <- 0
	bundles <- list()
	while(i <= max_) {
		s <- ""
		end_ <- min(c(max_, i + bundle_size - 1))
		j <- i
		while(i <= end_) {
			s <- paste0(s, tree2xml(rm_ids_from_tree(build_tree(row = cast_table[i,], resource_name)), tab = "  "))
			#s_ <- paste0(s_, xml2::as_xml_document(rm_ids_from_tree(build_tree(row = cast_table[i,], resource_name))))
			i <- i + 1
		}
		s <- paste0("<Bundle>\n", s, "</Bundle>")
		b <- b + 1
		#cat(s)
		bundles[[paste0("Bundle", b)]] <- xml2::read_xml(s)
		cat(paste0("Bundle ", b, " a ", i - j, " ", resource_name, "s  \u03A3 ", resource_name, "s = ", i - 1, "\n"))
	}
	bundles
}
