
#' Print xml-like tree from cast table
#' This function takes a table as created by [fhir_cast()] and prints the tree structure implicit in the
#' column names of the tables. It is useful to get an overview over the implied structure when planning to create
#' FHIR bundles from this table using [fhir_build_bundles()].
#'
#' By default, only the first 5 rows are converted to tree structure to prevent the output from getting too long. This
#' can be changed by setting the argument `nrow` to the desired number.
#'
#' @param cast_table A data.frame or data.table as produced by [fhir_cast()]
#' @param resource A character vector of length one or [fhir_resource_type-class] object
#' indicating which resource type the table is build from.
#' @param nrow A numeric of length 1 indicating how many rows to convert to tree structures.
#' @param rm_indices Remove indices from elements before printing? Defaults to `TRUE`
#'
#' @examples
#' #' #unserialize example
#' bundles <- fhir_unserialize(bundles = example_bundles1)
#'
#' #crack fhir resources
#' table_desc <- fhir_table_description(resource = "Patient",
#'                                      style = fhir_style(brackets = c("[","]"),
#'                                                         sep = " "))
#' df <- fhir_crack(bundles = bundles, design = table_desc)
#'
#' #cast
#' cast_df <- fhir_cast(df, brackets=c("[","]"), sep=" ", verbose=0)
#'
#' #show tree
#' fhir_show_tree(cast_df, resource="Patient")
#' @export
#' @seealso [fhir_cast()], [fhir_build_bundles()]
#'
fhir_show_tree <- function(cast_table, resource, nrow = 5, rm_indices=TRUE){
	i <- 1
	bundle <- list()
	while(i <= min(nrow(cast_table), nrow)) {
		row <- cast_table[i,]

		if(rm_indices){
			bundle <- c(bundle, rm_ids_from_tree(build_tree(row = row, root = resource, keep_nas = F)))
		}else{
			bundle <- c(bundle, build_tree(row = row, root = resource, keep_nas = F))
		}
		i <- i + 1
	}
	print_tree(bundle)
}

#' Build a list of FHIR bundles
#'
#' This function takes a table as produced by [fhir_cast()] and builds a [fhir_bundle_list-class] object from it. It is primarily used
#' to create transaction/batch bundles to POST back to a FHIR server. The column names of the table must represent the XPath expression of the
#' respective element with indices for repeating items. A table like this is produced when FHIR resources have been cracked with [fhir_crack()] without
#' assigning explicit column names in the [fhir_design-class]/[fhir_table_description-class] and this table has in turn been cast to wide format with
#' [fhir_cast()].
#'
#' The typical use case would look like this:
#' 1) Download resources from a server whith [fhir_search()]
#' 2) Crack and cast them whith [fhir_crack()] and [fhir_cast()]
#' 3) Do something to values (e.g. some kind of anonymization)
#' 4) Translate the data back into FHIR resources whith [fhir_build_bundles()]
#' 5) Post the resources to a server
#'
#' A FHIR bundle that can be POSTed to a server is usually of type `transaction` or `batch`. Each entry of these bundles consists of the resource itself
#' as well as an instruction for the server of what to to with the resource. A very simple example looks like this:
#'
#' ```
#' <Bundle>
#'    <type value="transaction"/>
#'	  <entry>
#'	     <resource>
#'	        <Patient>
#'	           <id value="id1"/>
#'		       <address>
#'		          <city value="Amsterdam"/>
#'		          <country value="Netherlands"/>
#'		       </address>
#'		       <name>
#'		          <given value="Marie"/>
#'		       </name>
#'	        </Patient>
#'	    </resource>
#'	    <request>
#'		   <method value="POST"/>
#'		   <url value="Patient"/>
#'	    </request>
#'	 </entry>
#'	 <entry>
#'	     <resource>
#'	        <Patient>
#'	           <id value="id2"/>
#'		       <address>
#'		          <city value="Paris"/>
#'		          <country value="France"/>
#'		       </address>
#'		       <name>
#'		          <given value="Anne"/>
#'		       </name>
#'	        </Patient>
#'	    </resource>
#'	    <request>
#'		   <method value="POST"/>
#'		   <url value="Patient"/>
#'	    </request>
#'	 </entry>
#' </Bundle>
#' ```
#' In this example the bundle contains two Patient resources that are sent to server with a POST. For more information the structure of transaction/batch bundles,
#' please see the FHIR documentation at https://www.hl7.org/fhir/http.html and https://www.hl7.org/fhir/bundle.html.
#'
#' In the cast table, each row corresponds to one resource that is created. To add the information for the `request` element of the bundle,
#' this table has to be augmented with two columns named `request.method` and `request.url`, which contain the respective HTTP verb and URL for the resource.
#' If these columns are not added to the table, [fhir_build_bundles()] still builds bundles from it, but those bundles will not be POSTeable to a server. See examples.
#'
#'
#' @param cast_table A cast table as produced by [fhir_cast()], possibly modified (see details).
#' @param resource_type A character vector of length one or [fhir_resource_type-class] object
#' indicating which resource type the table is build from.
#' @param bundle_type A character vector of length one defining the bundle type. Will usually be
#' either `"transaction"` (the default) or `"batch"`.
#' @param bundle_size Numeric of length one defining how many resources to put in each bundle
#' @return A [fhir_bundle_list-class] object.
#' @export
#' @examples
#' #unserialize example
#' bundles <- fhir_unserialize(bundles = example_bundles1)
#'
#' #crack fhir resources
#' Pat <- fhir_table_description(resource = "Patient",
#'                                      style = fhir_style(brackets = c("[","]"),
#'                                                         sep = " "))
#'
#' df <- fhir_crack(bundles = bundles, design = Pat)
#'
#' #cast
#' cast_df <- fhir_cast(df, brackets=c("[","]"), sep=" ", verbose=0)
#'
#' #add request info to table
#' request <- data.frame(request.method = c("POST", "PUT"),
#'                       request.url = c("Patient", "Patient/111"))
#'
#' request_df <- cbind(cast_df, request)
#'
#' #build bundles
#' bundles <- fhir_build_bundles(request_df, "Patient", bundle_type = "transaction", bundle_size=2)
#'
#' #print to console
#' cat(toString(bundles[[1]]))
#' @export
#' @seealso [fhir_cast()], [fhir_crack()]

fhir_build_bundles <- function(
	cast_table,
	resource_type,
	bundle_type = "transaction",
	bundle_size = 500) {

	names(cast_table)[!grepl("^request", names(cast_table))] <- paste0("resource.", resource_type, ".", names(cast_table)[!grepl("^request", names(cast_table))])

	max_ <- nrow(cast_table)
	i <- 1
	b <- 0
	bundles <- list()
	while(i <= max_) {
		s <- ""
		end_ <- min(c(max_, i + bundle_size - 1))
		j <- i
		while(i <= end_) {
			s <- paste0(s, tree2xml(rm_ids_from_tree(build_tree(row = cast_table[i,], "entry")), tab = "  "))
			#s_ <- paste0(s_, xml2::as_xml_document(rm_ids_from_tree(build_tree(row = cast_table[i,], resource_name))))
			i <- i + 1
		}
		s <- paste0("<Bundle>\n","   <type value='",bundle_type, "'/>\n", s, "</Bundle>")
		b <- b + 1
		#cat(s)
		bundles[[paste0("Bundle", b)]] <- xml2::read_xml(s)
		cat(paste0("Bundle ", b, " a ", i - j, " ", resource_type, "s  \u03A3 ", resource_type, "s = ", i - 1, "\n"))
	}
	fhir_bundle_list(bundles)
}


#######################################################################################################################
#######################################################################################################################

#' Build tree for xml creation
#' Creates a tree (list of lists) ready to be converted to an xml by `xml2::as_xml_document()`
#'
#' @param row One row of casted data.frame where the column names reflect indexed XPath expressions
#' @param root The root node to build the tree under
#' @param keep_nas Keep `NA` in the tree? If `FALSE` (the default), `NA` are removed.
#'
#' @examples
#' #unserialize example
#' bundles <- fhir_unserialize(bundles = example_bundles1)
#'
#' #crack fhir resources
#' table_desc <- fhir_table_description(resource = "Patient",
#'                                      style = fhir_style(brackets = c("[","]"),
#'                                                         sep = " "))
#' df <- fhir_crack(bundles = bundles, design = table_desc)
#'
#' #cast
#' cast_df <- fhir_cast(df, brackets=c("[","]"), sep=" ", verbose=0)
#'
#' #build tree
#' tree <- build_tree(cast_df[1,], root="Patient")
#'
#' print_tree(tree)
#' tree <- rm_ids_from_tree(tree)
#' cat(toString(xml2::as_xml_document(tree)))
#'
#' @noRd
build_tree <- function(row, root = "Bundle", keep_nas = F) {

	new_tree <- function(col_names, tree, value = 1) {
		len <- length(col_names)
		if(is.null(tree)) {tree <- list()}
		if(len == 0) {
			setattr(tree, "value", value)
		} else {
			tr <- new_tree(col_names = col_names[-1], tree = tree[[col_names[1]]], value = value)
			tree[[col_names[1]]] <- tr
		}
		tree
	}

	tree <- list()
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
			tr <- new_tree(col_names = col_names_split[-1], tree = tree[[col_names_split[[1]]]], value = value)
		}
		tree[[col_names_split[1]]] <- tr
	}
	tree
}

#' Remove ids from tree
#' Removes the ids leftover from the casted table
#' @param tree A tree as produced by [build_tree()]
#'
#' @examples
#' #unserialize example
#' bundles <- fhir_unserialize(bundles = example_bundles1)
#'
#' #crack fhir resources
#' table_desc <- fhir_table_description(resource = "Patient",
#'                                      style = fhir_style(brackets = c("[","]"),
#'                                                         sep = " "))
#' df <- fhir_crack(bundles = bundles, design = table_desc)
#'
#' #cast
#' cast_df <- fhir_cast(df, brackets=c("[","]"), sep=" ", verbose=0)
#'
#' #build tree
#' tree <- build_tree(cast_df[1,], root="Patient")
#'
#' tree <- rm_ids_from_tree(tree)
#' cat(tree2text(tree))
#' @noRd
rm_ids_from_tree <- function(tree) {
	if(!is.null(names(tree))) {
		for(n in names(tree)) {
			#n <- names(tree)[[1]]
			tree[[n]] <- rm_ids_from_tree(tree = tree[[n]])
		}
		names(tree) <- gsub("(\\[[0-9]+])|([0-9]+)", "", names(tree))
	}
	tree
}

#' Create text version of tree
#'
#' @param tree A tree as produced by [build_tree()]
#' @param tab A string that is put at the beginning of each line
#' @param add The string used for indentation of each line
#'
#' @examples
#' #unserialize example
#' bundles <- fhir_unserialize(bundles = example_bundles1)
#'
#' #crack fhir resources
#' table_desc <- fhir_table_description(resource = "Patient",
#'                                      style = fhir_style(brackets = c("[","]"),
#'                                                         sep = " "))
#' df <- fhir_crack(bundles = bundles, design = table_desc)
#'
#' #cast
#' cast_df <- fhir_cast(df, brackets=c("[","]"), sep=" ", verbose=0)
#'
#' #build tree
#' tree <- build_tree(cast_df[1,], root="Patient")
#'
#' tree <- rm_ids_from_tree(tree)
#' cat(tree2text(tree))
#' @noRd

tree2text <- function(tree, tab = "", add = "  ") {
	str = ""
	for(i in seq_along(tree)) {
		#s <- ""
		n <- names(tree)[i]
		tr <- tree[[i]]
		s <- paste0(tab, n)
		a <- attr(tr, "value")
		if(!is.null(a)) {
			s <- paste0(s, " : ", a)
		}
		str <- paste0(str, s, "\n", tree2text(tree = tr, tab = inc_tab(tab, add), add = add))
	}
	str
}

#' Short form for cat(tree2string())
#' @noRd
print_tree <- function(tree, sign = ":") {
	cat(tree2string(tree = tree, sign=sign))
}

#' Create string for printing of tree
#'
#' @param tree A tree as produced by [build_tree()]
#' @param sign A string that is put between each element and its value
#'
#' @examples
#' #unserialize example
#' bundles <- fhir_unserialize(bundles = example_bundles1)
#'
#' #crack fhir resources
#' table_desc <- fhir_table_description(resource = "Patient",
#'                                      style = fhir_style(brackets = c("[","]"),
#'                                                         sep = " "))
#' df <- fhir_crack(bundles = bundles, design = table_desc)
#'
#' #cast
#' cast_df <- fhir_cast(df, brackets=c("[","]"), sep=" ", verbose=0)
#'
#' #build tree
#' tree <- build_tree(cast_df[1,], root="Patient")
#'
#' tree <- rm_ids_from_tree(tree)
#' cat(tree2string(tree, sign="\u2500"))
#' @noRd
tree2string <- function(tree, sign = c("\u2500", ":")[1]) {
	tree2string_ <- function(tree, pre, sign) {
		if(is.null(tree)) return(NULL)
		rows <- list()
		len <- length(tree)
		for(i in seq_len(len)) {
			#i <- 1
			n <- names(tree)[i]
			tr <- tree[[i]]
			s <- paste0(pre, (if(i == len) "\u2514" else "\u251C"), "\u2500", (if(length(tr) == 0) "\u2500" else "\u2510"), " ", n)
			a <- attr(tr, "value")
			if(!is.null(a)) {
				s <- paste0(s, " ", sign, " ", a)
			}
			rows[[i]] <- paste0(
				s,
				"\n",
				tree2string_(
					tree = tr,
					pre = if(i < len) paste0(pre, "\u2502", " ") else paste0(pre, "  "),
					sign = sign
				)
			)
		}

		paste0(rows, collapse = "")
	}
	tree2string_(tree = tree, pre = "", sign = sign)
}




#' Create xml version of tree
#'
#' @param tree A tree as produced by [build_tree()]
#' @param escaped Escape special xml characters? Defaults to `TRUE`
#' @param tab A string that is put at the beginning of each line
#' @param add The string used for indentation of each line
#'
#' @return
#' A string representing an xml
#'
#' @examples
#' #unserialize example
#' bundles <- fhir_unserialize(bundles = example_bundles1)
#'
#' #crack fhir resources
#' table_desc <- fhir_table_description(resource = "Patient",
#'                                      style = fhir_style(brackets = c("[","]"),
#'                                                         sep = " "))
#' df <- fhir_crack(bundles = bundles, design = table_desc)
#'
#' #cast
#' cast_df <- fhir_cast(df, brackets=c("[","]"), sep=" ", verbose=0)
#'
#' #build tree
#' tree <- build_tree(cast_df[1,], root="Patient")
#'
#' tree <- rm_ids_from_tree(tree)
#' cat(tree2xml(tree))
#' @noRd

tree2xml <- function(tree, escaped = T, tab = "", add = "  ") {
	str = ""
	for(i in seq_along(tree)) {
		s <- ""
		#i<-1
		n <- names(tree)[i]
		#n<-names(tree)[[1]]
		tr <- tree[[i]]

		s <- paste0(tab, "<", n)
		a <- attr(tr, "value")
		if(!is.null(a)) {
			s <- paste0(s, " value=\"", if(escaped) esc_xml(a) else a, "\"")
		}
		s <- if(length(tr) == 0) paste0(s, "/>") else paste0(s, ">")
		s = paste0(s, "\n", tree2xml(tree = tr, escaped = escaped, tab = inc_tab(tab, add), add = add))
		if(0 < length(tr)) s <- paste0(s, tab, "</", n, ">\n")
		str <- paste0(str, s)
	}
	str
}

