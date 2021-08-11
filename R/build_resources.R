
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

#' Build a single FHIR resource
#'
#' This function takes a single row from a table as produced by [fhir_cast()] and builds a [fhir_resource_xml-class] object from it. The column names of the table
#' must represent the XPath expression of the respective element with indices for repeating items. A table like this is produced when FHIR resources have
#' been cracked with [fhir_crack()] without assigning explicit column names in the [fhir_design-class]/[fhir_table_description-class] and this table has in turn
#' been cast to wide format with [fhir_cast()].
#'
#' @param cast_row Single row from a cast table as produced by [fhir_cast()]
#' @param resource_type A character vector of length one or [fhir_resource_type-class] object
#' indicating which resource type the table is build from.
#' @return A [fhir_resource_xml-class] object.
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
#' #build bundles
#' resource <- fhir_build_resource(cast_df[1,], "Patient")
#'
#' #print to console
#' resource
#' @export
#' @seealso [fhir_cast()], [fhir_crack()], [fhir_build_bundles()]

fhir_build_resource <- function(
	cast_row,
	resource_type) {

	s <- tree2xml(rm_ids_from_tree(build_tree(cast_row, root = resource_type)))
	fhir_resource_xml(xml2::read_xml(s))
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
#' @param verbose An integer vector of length one. If 0, nothing is printed, if > 0 progress message is printed. Defaults to 1.
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
#' @seealso [fhir_cast()], [fhir_crack()], [fhir_build_resource()]

fhir_build_bundles <- function(
	cast_table,
	resource_type,
	bundle_type = "transaction",
	bundle_size = 500,
	verbose = 1) {

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
		if(verbose > 0) {cat(paste0("Bundle ", b, " a ", i - j, " ", resource_type, "s  \u03A3 ", resource_type, "s = ", i - 1, "\n"))}
	}
	fhir_bundle_list(bundles)
}

#' POST to a FHIR server
#'
#' This function is a convenience wrapper around [httr::POST()].
#'
#' [fhir_post()] accepts three classes for the body:
#'  1) A [fhir_bundle_list-class] representing a list of transaction or batch bundles as created by [fhir_build_bundles()].
#'  This is used to POST or PUT several resources to the server. When `body` is a [fhir_bundle_list-class], the `url` has to be the base
#'  url of the server, e.g. http://hapi.fhir.org/baseR4.
#'
#'  2) A [fhir_resource-class] as created by [fhir_build_resource()]. This is used when just a single resource should be POSTed to the server.
#'  In this case `url` must contain the base url plus the resource type, e.g. http://hapi.fhir.org/baseR4/Patient.
#'
#'  3) A [fhir_body-class] as created by [fhir_body()]. This is the most flexible approach, because within the [fhir_body-class] object you can represent
#'  any kind of `content` as a string and set the `type` accordingly. See examples.
#'
#'  For examples of how to create the different body types see the respective help pages. For an example of the entire workflow around creating
#'  and POSTing resources, see the package vignette on recreating resources.
#'
#' @param url An object of class [fhir_url-class] or a character vector of length one containing the url to POST to.
#' @param body An object of class [fhir_bundle_list-class], [fhir_resource-class] or [fhir_body-class]. See details for how to generate them.
#' @param username A character vector of length one containing the username for basic authentication.
#' @param password A character vector of length one containing the password for basic authentication.
#' @param token A character vector of length one or object of class [httr::Token-class], for bearer token authentication (e.g. OAuth2). See [fhir_authenticate()]
#' for how to create this.
#' @param verbose An integer vector of length one. If 0, nothing is printed, if > 0 success message is printed. Defaults to 1.
#' @param log_errors Either `NULL` or a character vector of length one indicating the name of a file in which to save http errors.
#' `NULL` means no error logging. When a file name is provided, the errors are saved in the specified file. Defaults to `NULL`.
#' Regardless of the value of `log_errors` the most recent http error message within the current R session is saved internally and can
#' be accessed with [fhir_recent_http_error()].
#'
#' @export
#' @examples
#' \donttest{
#' ### 1. POST transaction bundles
#' #unserialize example bundles
#' bundles <- fhir_unserialize(transaction_bundle_example)
#'
#' #have a look at the bundle
#' cat(toString(bundles[[1]]))
#'
#' #post
#' fhir_post(url = "http://hapi.fhir.org/baseR4", body = bundles)
#'
#'
#' ### 2. POST single resouce
#' #unserialize example resource
#' resource <- fhir_unserialize(example_resource1)
#'
#' #have a look at the resource
#' resource
#'
#' #post
#' url <- fhir_url(url = "http://hapi.fhir.org/baseR4", resource = "Patient")
#' fhir_post(url = url, body = resource)
#'
#'
#' ### 3. POST arbitrary body
#' #define body
#' body <- fhir_body(content = "<Patient> <gender value='female'/> </Patient>", type = "xml")
#'
#' #post
#' url <- fhir_url(url = "http://hapi.fhir.org/baseR4", resource = "Patient")
#' fhir_post(url = url, body = body)
#' }

fhir_post <- function(
	url,
	body,
	username = NULL,
	password = NULL,
	token = NULL,
	verbose = 1,
	log_errors = NULL){

	auth <- if(!is.null(username) && !is.null(password)) {
		httr::authenticate(user = username, password = password)
	}

	#prepare token authorization
	if(!is.null(token)) {
		if(!is.null(username) || is.null(password)) {
			warning(
				"You provided username and password as well as a token for authentication.\n",
				"Ignoring username and password, trying to authorize with token."
			)
			username <- NULL
			password <- NULL
		}
		if(is(token, "Token")) {
			token <- token$credentials$access_token
		}
		if(1 < length(token)) {stop("token must be of length one.")}
		bearerToken <- paste0("Bearer ", token)
	} else {
		bearerToken <- NULL
	}

	if(is(body, "fhir_bundle_list")) {
		i<-0
		invisible(lapply(body,
				function(bundle){
					i<<-i+1
					response <- httr::POST(
						url = url,
						config = httr::add_headers(
							Accept = "application/fhir+xml",
							Authorization = token
						),
						httr::content_type(type = "xml"),
						auth,
						body = toString(bundle)
					)

					#check for http errors
					check_response(response = response, log_errors = log_errors, append = TRUE)

					if(response$status_code==200 && verbose>0) {
						cat(paste0("Bundle ", i, " sucessfully POSTed\n"))
					}
				}
			))

	}else if(is(body, "fhir_resource_xml")) {

		response <- httr::POST(
			url = url,
			config = httr::add_headers(
				Accept = "application/fhir+xml",
				Authorization = token
			),
			httr::content_type(type = "xml"),
			auth,
			body = toString(body)
		)

		#check for http errors
		check_response(response = response, log_errors = log_errors)

		if(response$status_code==201 && verbose>0) {
			cat("Resource sucessfully POSTed")
		}

	}else if(is(body, "fhir_body")) {

		response <- httr::POST(
			url = url,
			config = httr::add_headers(
				Accept = "application/fhir+xml",
				Authorization = token
			),
			httr::content_type(type = body@type),
			auth,
			body = body@content
		)

		#check for http errors
		check_response(response = response, log_errors = log_errors)
		if(response$status_code %in% c(200,201,202) && verbose>0) {
			cat("Body sucessfully POSTed")
		}

	} else {
		stop("body must be of type fhir_bundle_xml, fhir_resource_xml or fhir_body")
	}


}

#' PUT to a FHIR server
#'
#' This function is a convenience wrapper around [httr::PUT()].
#'
#' [fhir_put()] accepts two classes for the body:
#'
#'  1) A [fhir_resource-class] as created by [fhir_build_resource()]. This is used when just a single resource should be PUT to the server.
#'  In this case `url` must contain the base url plus the resource type and the resource id,
#'  e.g. http://hapi.fhir.org/baseR4/Patient/1a2b3c.
#'
#'  2) A [fhir_body-class] as created by [fhir_body()]. This is the most flexible approach, because within the [fhir_body-class] object you can represent
#'  any kind of `content` as a string and set the `type` accordingly. See examples.
#'
#'  For examples of how to create the different body types see the respective help pages. For an example of the entire workflow around creating
#'  and PUTing resources, see the package vignette on recreating resources.
#'
#' @param url An object of class [fhir_url-class] or a character vector of length one containing the url to PUT to.
#' @param body An object of class [fhir_resource-class] or [fhir_body-class]. See details for how to generate them.
#' @param username A character vector of length one containing the username for basic authentication.
#' @param password A character vector of length one containing the password for basic authentication.
#' @param token A character vector of length one or object of class [httr::Token-class], for bearer token authentication (e.g. OAuth2). See [fhir_authenticate()]
#' for how to create this.
#' @param verbose An integer vector of length one. If 0, nothing is printed, if > 0 success message is printed. Defaults to 1.
#' @param log_errors Either `NULL` or a character vector of length one indicating the name of a file in which to save http errors.
#' `NULL` means no error logging. When a file name is provided, the errors are saved in the specified file. Defaults to `NULL`.
#' Regardless of the value of `log_errors` the most recent http error message within the current R session is saved internally and can
#' be accessed with [fhir_recent_http_error()].
#'
#' @export
#' @examples
#' \donttest{
#' ### 1. PUT fhir__resource object
#' #unserialize example resource
#' resource <- fhir_unserialize(example_resource2)
#'
#' #have a look at the resource
#' resource
#'
#' #put
#' fhir_put(url = "http://hapi.fhir.org/baseR4/Patient/1a2b3c", body = resource)
#'
#' ### 2. PUT fhir_body object
#' #define body
#' body <- fhir_body(content = "<Patient> <id value='x1y2'/> <gender value='female'/> </Patient>",
#'                   type = "xml")
#'
#' #put
#' fhir_put(url = "http://hapi.fhir.org/baseR4/Patient/x1y2", body = body)
#' }

fhir_put <- function(
	url,
	body,
	username = NULL,
	password = NULL,
	token = NULL,
	verbose = 1,
	log_errors = NULL){

	auth <- if(!is.null(username) && !is.null(password)) {
		httr::authenticate(user = username, password = password)
	}

	#prepare token authorization
	if(!is.null(token)) {
		if(!is.null(username) || is.null(password)) {
			warning(
				"You provided username and password as well as a token for authentication.\n",
				"Ignoring username and password, trying to authorize with token."
			)
			username <- NULL
			password <- NULL
		}
		if(is(token, "Token")) {
			token <- token$credentials$access_token
		}
		if(1 < length(token)) {stop("token must be of length one.")}
		bearerToken <- paste0("Bearer ", token)
	} else {
		bearerToken <- NULL
	}

	if(is(body, "fhir_resource_xml")) {

		response <- httr::PUT(
			url = url,
			config = httr::add_headers(
				Accept = "application/fhir+xml",
				Authorization = token
			),
			httr::content_type(type = "xml"),
			auth,
			body = toString(body)
		)

		#check for http errors
		check_response(response = response, log_errors = log_errors)

		if(response$status_code==201 && verbose>0) {
			cat("Resource sucessfully PUT")
		}

	}else if (is(body, "fhir_body")){

		response <- httr::PUT(
			url = url,
			config = httr::add_headers(
				Accept = "application/fhir+xml",
				Authorization = token
			),
			httr::content_type(type = body@type),
			auth,
			body = body@content
		)

		#check for http errors
		check_response(response = response, log_errors = log_errors)
		if(response$status_code %in% c(200,201,202) && verbose>0){
			cat("Body sucessfully PUT")
		}

	}else{
		stop("body must be of type fhir_bundle_xml or fhir_body")
	}


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

