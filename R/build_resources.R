## This file contains all functions for building and uploading FHIR resources##
## Exported functions are on top, internal functions below ##


#' Build a single FHIR resource
#'
#' This function takes a single row from a wide table as produced by [fhir_crack()] and builds a [fhir_resource_xml-class] object from it. The column names of the table
#' must represent the XPath expression of the respective element with indices for repeating items. A table like this is produced when FHIR resources have
#' been cracked with [fhir_crack()] without assigning explicit column names in the [fhir_design-class]/[fhir_table_description-class] and with `format` set to `"wide"`.
#'
#' @param row Single row from a wide table as produced by [fhir_crack()] with `format="wide"`
#' @param brackets A character vector of length one. The brackets used for cracking.
#' @param resource_type A character vector of length one or [fhir_resource_type-class] object
#' indicating which resource type the table is build from.
#' @return A [fhir_resource_xml-class] object.
#' @export
#' @examples
#' #unserialize example
#' bundles <- fhir_unserialize(bundles = example_bundles1)
#'
#' #crack fhir resources
#' Pat <- fhir_table_description(
#'     resource = "Patient",
#'     brackets = c("[", "]"),
#'     sep      = " ",
#'     format   = "wide"
#' )
#'
#' df <- fhir_crack(bundles = bundles, design = Pat)
#'
#'
#' #build resource
#' resource <- fhir_build_resource(
#'                 row           = df[1,],
#'                 brackets      = c('[', ']'),
#'                 resource_type = "Patient"
#'             )
#'
#' #print to console
#' resource
#' @export
#' @seealso [fhir_cast()], [fhir_crack()], [fhir_build_bundle()], [fhir_post()], [fhir_put()]

fhir_build_resource <- function(row, brackets, resource_type) {
	tree <- fhir_tree.new(table     = row,
						  brackets  = brackets,
						  root      = resource_type)
	tree <- fhir_tree.rm_ids(tree)
	xml <- xml2::read_xml(x = fhir_tree.as_xml(tree = tree))
	fhir_resource_xml(resource = xml)
}



#' Build a FHIR bundle
#'
#' This function takes a table as produced by [fhir_crack()] with `format="wide"` and builds a [fhir_bundle_xml-class] object from it. It is primarily used
#' to create transaction/batch bundles to POST back to a FHIR server. The column names of the table must represent the XPath expression of the
#' respective element with indices for repeating items. A table like this is produced when FHIR resources have been cracked with [fhir_crack()] without
#' assigning explicit column names in the [fhir_design-class]/[fhir_table_description-class] and the format has been set to `"wide"`.
#'
#' The typical use case would look like this:
#' 1) Download resources from a server whith [fhir_search()]
#' 2) Crack to wide format them with [fhir_crack()]
#' 3) Do something to values (e.g. some kind of anonymization)
#' 4) Translate the data back into FHIR resources with [fhir_build_bundle()]
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
#' In the table, each row corresponds to one resource that is created. To add the information for the `request` element of the bundle,
#' this table has to be augmented with two columns named `request.method` and `request.url`, which contain the respective HTTP verb and URL for the resource.
#' If these columns are not added to the table, [fhir_build_bundle()] still builds bundles from it, but those bundles will not be POSTable to a server. See examples.
#'
#'
#' @param table A wide table as produced by [fhir_crack()], possibly modified (see details) or a named list
#' of wide tables, if different resource types have to be included in the same bundle. In this case the names of
#' the list elements must correspond to the resource type represented in the table!
#' @param brackets A character vector of length one. The brackets used for cracking.
#' @param resource_type A character vector of length one or [fhir_resource_type-class] object
#' indicating which resource type is represented in the table, if a single table is provided. This argument is
#' ignored when `table` is a named list of tables.
#' @param bundle_type A character vector of length one defining the bundle type. Will usually be
#' either `"transaction"` (the default) or `"batch"`.
#' @param verbose An integer vector of length one. If 0, nothing is printed, if > 0 progress message is printed. Defaults to 1.
#' @return A [fhir_bundle_xml-class] object.
#' @export
#' @examples
#' #unserialize example
#' bundles <- fhir_unserialize(bundles = example_bundles1)
#'
#' #crack fhir resources
#' table_desc_pat <- fhir_table_description(
#'     resource = "Patient",
#'     brackets = c("[", "]"),
#'     sep      = " ",
#'     format   = "wide"
#' )
#'
#' df <- fhir_crack(bundles = bundles, design = table_desc_pat)
#'
#' #add request info to table
#' request <- data.frame(
#'     request.method = c("POST", "PUT"),
#'     request.url    = c("Patient", "Patient/id3")
#' )
#'
#' request_df <- cbind(df, request)
#'
#' #build bundle
#' bundle <- fhir_build_bundle(table          = request_df,
#'                             brackets       = table_desc_pat@brackets,
#'                             resource_type  = "Patient",
#'                             bundle_type    = "transaction")
#'
#' #print to console
#' cat(toString(bundle))
#' @rdname fhir_build_bundle-methods
#' @docType methods
#'
#' @export
#' @seealso [fhir_crack()],[fhir_cast()], [fhir_build_resource()], [fhir_post()]


setGeneric(
	name = "fhir_build_bundle",
	def = function(table,
				   brackets,
				   resource_type,
				   bundle_type   = "transaction",
				   verbose       = 1) {
		standardGeneric("fhir_build_bundle")
	}
)

#' @rdname fhir_build_bundle-methods
#' @aliases fhir_build_bundle,data.frame-method
setMethod(
	f = "fhir_build_bundle",
	signature = c("table" = "data.frame"),
	definition =  function(table,
						   brackets,
						   resource_type,
						   bundle_type   = "transaction",
						   verbose       = 1) {
		#build entries
		s <-
			build_entries(table = table,
						  brackets = brackets,
						  resource_type = resource_type)

		#Wrap in Bundle
		s <-
			paste0("<Bundle>\n",
				   "   <type value='",
				   bundle_type,
				   "'/>\n",
				   s,
				   "</Bundle>")

		#turn into fhir_bundle_xml
		max_ <- nrow(table)
		bundle <- xml2::read_xml(s)
		if (0 < verbose) {
			message("Created a ",
					bundle_type,
					" Bundle with ",
					max_,
					" resource",
					pluralS(max_),
					".")
		}

		fhir_bundle_xml(bundle)
	}
)

#' @rdname fhir_build_bundle-methods
#' @aliases fhir_build_bundle,list-method
setMethod(
	f          = "fhir_build_bundle",
	signature  = c("table" = "list"),
	definition =  function(table,
						   brackets,
						   bundle_type = "transaction",
						   verbose     = 1) {
		#preliminary checks
		if (!all(sapply(table, is.data.frame))) {
			stop(
				"All elements of the list provided to cast_table must be data.frames/data.tables."
			)
		}

		if (length(names(table)) != length(table)) {
			stop(
				"You have to provide a **named** list, where the names correspond to the resource type represented in the table."
			)
		}

		#build entries
		s <- ""
		lapply(
			X = seq_len(length(table)),
			FUN = function(i) {
				#i <- 1
				single_table <- table[[i]]
				resource_type <- fhir_resource_type(names(table)[i])

				#build entries
				s <<-
					paste0(
						s,
						build_entries(
							single_table,
							brackets = brackets,
							resource_type = resource_type
						)
					)
			}
		)

		#wrap in Bundle
		s <-
			paste0("<Bundle>\n",
				   "   <type value='",
				   bundle_type,
				   "'/>\n",
				   s,
				   "</Bundle>")

		#turn into fhir_bundle_xml
		bundle <- xml2::read_xml(s)

		if (0 < verbose) {
			message("Created a  ",
					bundle_type,
					" Bundle with ",
					Reduce(sum, lapply(table, nrow)),
					" resources.")
		}

		fhir_bundle_xml(bundle)
	}
)

#' POST to a FHIR server
#'
#' This function is a convenience wrapper around [httr::POST()].
#'
#' @details
#' [fhir_post()] accepts four classes for the body:
#'
#'  1) A [fhir_resource-class] as created by [fhir_build_resource()]. This is used when just a single resource should be POSTed to the server.
#'  In this case `url` must contain the base url plus the resource type, e.g. http://hapi.fhir.org/baseR4/Patient.
#'
#'  2) A [fhir_bundle_xml-class] representing a transaction or batch bundle as created by [fhir_build_bundle()].
#'
#'  3) A [fhir_body-class] as created by [fhir_body()]. This is the most flexible approach, because within the [fhir_body-class] object you can represent
#'  any kind of `content` as a string and set the `type` accordingly. See examples.
#'
#'  For examples of how to create the different body types see the respective help pages. For an example of the entire workflow around creating
#'  and POSTing resources, see the package vignette on recreating resources.
#'
#' @param url An object of class [fhir_url-class] or a character vector of length one containing the url to POST to.
#' @param body An object of class  [fhir_resource-class], [fhir_bundle_xml-class] or [fhir_body-class].
#' See details for how to generate them.
#' @param username A character vector of length one containing the username for basic authentication.
#' @param password A character vector of length one containing the password for basic authentication.
#' @param token A character vector of length one or object of class [httr::Token-class], for bearer token authentication (e.g. OAuth2). See [fhir_authenticate()]
#' for how to create this.
#' @param verbose An integer vector of length one. If 0, nothing is printed, if > 0 success message is printed. Defaults to 1.
#' @param log_errors Either `NULL` or a character vector of length one indicating the name of a file in which to save http errors.
#' `NULL` means no error logging. When a file name is provided, the errors are saved in the specified file. Defaults to `NULL`.
#' Regardless of the value of `log_errors` the most recent http error message within the current R session is saved internally and can
#' be accessed with [fhir_recent_http_error()].
#' @include fhir_resource.R fhir_bundle_list.R fhir_body.R
#' @export
#' @rdname fhir_post-methods
#' @docType methods
#'
#' @examples
#' \dontrun{
#' ### 1. POST transaction bundle
#' #unserialize example bundles
#' bundle <- fhir_unserialize(transaction_bundle_example)
#'
#' #have a look at the bundle
#' cat(toString(bundle))
#'
#' #post
#' fhir_post(url = "http://hapi.fhir.org/baseR4", body = bundle)
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

setGeneric(
	name = "fhir_post",
	def = function(url,
				   body,
				   username   = NULL,
				   password   = NULL,
				   token      = NULL,
				   verbose    = 1,
				   log_errors = NULL) {
		standardGeneric("fhir_post")
	}
)

#' @rdname fhir_post-methods
#' @aliases fhir_post,fhir_resource-method
setMethod(
	f = "fhir_post",
	signature = c(body = "fhir_resource"),
	definition = function(url,
						  body,
						  username   = NULL,
						  password   = NULL,
						  token      = NULL,
						  verbose    = 1,
						  log_errors = NULL) {
		auth <-
			auth_helper(username = username,
						password = password,
						token = token)

		response <- httr::POST(
			url    = url,
			config = httr::add_headers(Accept        = "application/fhir+xml",
									   Authorization = auth$token),
			httr::content_type(type = "xml"),
			auth$basicAuth,
			body = toString(body)
		)

		#check for http errors
		check_response(response = response, log_errors = log_errors)

		if (response$status_code == 201 && 0 < verbose) {
			message("Resource sucessfully created")
		}
	}
)

#' @rdname fhir_post-methods
#' @aliases fhir_post,fhir_bundle_xml-method
setMethod(
	f          = "fhir_post",
	signature  = c(body = "fhir_bundle_xml"),
	definition = function(url,
						  body,
						  username   = NULL,
						  password   = NULL,
						  token      = NULL,
						  verbose    = 1,
						  log_errors = NULL) {
		auth <-
			auth_helper(username = username,
						password = password,
						token = token)

		response <- httr::POST(
			url = url,
			config = httr::add_headers(Accept = "application/fhir+xml",
									   Authorization = auth$token),
			httr::content_type(type = "xml"),
			auth$basicAuth,
			body = toString(body)
		)

		#check for http errors
		check_response(response = response,
					   log_errors = log_errors,
					   append = TRUE)

		if (response$status_code == 200 && 0 < verbose) {
			message("Bundle sucessfully POSTed")
		}
	}
)

#' @rdname fhir_post-methods
#' @aliases fhir_post,fhir_body-method
setMethod(
	f          = "fhir_post",
	signature  = c(body = "fhir_body"),
	definition = function(url,
						  body,
						  username   = NULL,
						  password   = NULL,
						  token      = NULL,
						  verbose    = 1,
						  log_errors = NULL) {
		auth <-
			auth_helper(username = username,
						password = password,
						token = token)

		response <- httr::POST(
			url    = url,
			config = httr::add_headers(Accept        = "application/fhir+xml",
									   Authorization = auth$token),
			httr::content_type(type = body@type),
			auth$basicAuth,
			body = body@content
		)

		#check for http errors
		check_response(response = response, log_errors = log_errors)
		if (response$status_code %in% c(200, 201, 202) && 0 < verbose) {
			message("Body sucessfully POSTed")
		}
	}
)


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
#' \dontrun{
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

fhir_put <- function(url,
					 body,
					 username   = NULL,
					 password   = NULL,
					 token      = NULL,
					 verbose    = 1,
					 log_errors = NULL) {
	auth <-
		auth_helper(username = username,
					password = password,
					token = token)

	if (is(body, "fhir_resource_xml")) {
		response <- httr::PUT(
			url    = url,
			config = httr::add_headers(Accept        = "application/fhir+xml",
									   Authorization = auth$token),
			httr::content_type(type = "xml"),
			auth$basicAuth,
			body = toString(body)
		)

		#check for http errors
		check_response(response = response, log_errors = log_errors)

		if (response$status_code == 201 && 0 < verbose) {
			message("Resource sucessfully created")
		}

		if (response$status_code == 200 && 0 < verbose) {
			message("Resource sucessfully updated")
		}
	} else if (is(body, "fhir_body")) {
		response <- httr::PUT(
			url    = url,
			config = httr::add_headers(Accept        = "application/fhir+xml",
									   Authorization = auth$token),
			httr::content_type(type = body@type),
			auth$basicAuth,
			body = body@content
		)

		#check for http errors
		check_response(response = response, log_errors = log_errors)

		if (response$status_code %in% c(200, 201, 202) && 0 < verbose) {
			message("Body sucessfully PUT")
		}
	} else {
		stop("body must be of type fhir_bundle_xml or fhir_body")
	}
}

##############################################################################################################################
##############################################################################################################################
#' @param table A wide table as produced by [fhir_crack()], possibly modified (see details) or a named list
#' of wide tables, if different resource types have to be included in the same bundle. In this case the names of
#' the list elements must correspond to the resource type represented in the table!
#' @param brackets A character vector of length one. The brackets used for cracking.
#' @param resource_type A character vector of length one or [fhir_resource_type-class] object
#' indicating which resource type is represented in the table, if a single table is provided. This argument is
#' ignored when `table` is a named list of tables.

#' @return A string representing xml entries
#' @noRd
#' @examples
#' #unserialize example
#' bundles <- fhir_unserialize(bundles = example_bundles1)
#'
#' #crack fhir resources
#' table_desc_pat <- fhir_table_description(
#'     resource = "Patient",
#'     brackets = c("[", "]"),
#'     sep      = " ",
#'     format   = "wide"
#' )
#'
#' df <- fhir_crack(bundles = bundles, design = table_desc_pat)
#'
#' #add request info to table
#' request <- data.frame(
#'     request.method = c("POST", "PUT"),
#'     request.url    = c("Patient", "Patient/id3")
#' )
#'
#' request_df <- cbind(df, request)
#'
#' #build bundle
#' entries <- build_entries(table         = request_df,
#'                          brackets      = table_desc_pat@brackets,
#'                          resource_type = "Patient")
#'
#' #print to console
#' cat(toString(entries))
build_entries <- function(table,
						  brackets,
						  resource_type) {
	#add resource type to column names, increment indices accordingly
	resource_names <- names(table)[!grepl("^request", names(table))]
	resource_names_new <- paste0(
		paste0(
			brackets[1],
			"1.1.",
			separate_indices(
				resource_names,
				bra = esc(brackets[1]),
				ket = esc(brackets[2])
			),
			brackets[2]
		),
		paste0(
			"resource.",
			resource_type,
			".",
			separate_names(
				resource_names,
				bra = esc(brackets[1]),
				ket = esc(brackets[2])
			)
		)

	)
	names(table)[!grepl("^request", names(table))] <-
		resource_names_new

	#loop trough rows and build entries
	max_ <- nrow(table)
	s <- ""
	for (row in seq_len(max_)) {
		#row <- 1
		s <- paste0(s,
					fhir_tree.as_xml(fhir_tree.rm_ids(
						fhir_tree.new(
							table    = table[row,],
							brackets = brackets,
							root     = "entry"
						)
					),
					tabs = "  "))
	}
	s
}
