#' A S4 class to represent columns in a [fhir_table_description-class]
#'
#' An object of class `fhir_columns` is part of a [fhir_table_description-class]
#' in a [fhir_design-class] and holds information on the elements
#' that should be extracted from the FHIR resources, as well as the column names of the resulting data.frame.
#' The elements to be extracted are indicated by XPath xpaths.
#'
#' @slot names The column names
#' @include fhir_xpath_expression.R
#' @export
#'
setClass(
	Class    = "fhir_columns",
	contains = "fhir_xpath_expression",
	slots    = c(names = "character")
)

setValidity(
	Class  = "fhir_columns",
	method = function(object) {

		messages <- c()
		if(length(object) == 0) {return(TRUE)}
		if(length(names(object)) == 0) {
			messages <- c(messages, "fhir_columns has to be a *named* character.")
		}

		if(any(duplicated(object))){
			messages <- c(messages, paste("There are duplicates in the columns defined. Please remove the following duplicates: ",
										  paste(object[duplicated(object)], collapse = ", ")))
		}

		with_attribute <- object[grepl("/@[[:alpha:]]*$",object)]

		if(0 < length(with_attribute)){
			messages <- c(messages,
						  paste0("Please don't search for specific attributes like '/@value' as you did in ",
						  	   with_attribute[1], ".\n fhir_crack() will extract all available attributes for you."))
		}
		if(0 < length(messages)) {messages} else {TRUE}
	}
)

#' Create [fhir_columns-class] object
#'
#' An object of class `fhir_columns` is part of a `fhir_table_description` in a `fhir_design` and holds information on the elements
#' that should be extracted from the FHIR resources, as well as the column names of the resulting data.frame.
#' The elements to be extracted are indicated by XPath xpaths. If no column names are provided,
#' they are generated automatically and reflect the elements position in the resource.
#'
#' @param xpaths A (named) character vector or (named) list containing xpath xpaths,
#' or a [fhir_xpath_expression-class] object.
#' @param colnames The names of the columns to create. If no colnames are provided and the list or vector
#' in `xpaths` has names, those names are taken as the colnames. If no colnames are provided and
#' `xpaths` is unnamed too, the colnames are generated automatically from the xpath xpaths. See examples.
#'
#' @docType methods
#' @rdname fhir_columns-methods
#' @examples
#'  #provide colnames explicitly
#'  fhir_columns(xpaths = c("name/given", "code/coding/code"),
#'               colnames = c("name", "code"))
#'
#'  #colnames are taken from xpaths argument
#'  fhir_columns(xpaths = c(name = "name/given", code = "code/coding/code"))
#'
#'  #colnames are taken from xpaths argument
#'  fhir_columns(xpaths = list(name = "name/given", code = "code/coding/code"))
#'
#'  #colnames are generated automatically
#'  fhir_columns(xpaths = c("name/given", "code/coding/code"))
#' @export
setGeneric(
	name = "fhir_columns",
	def  = function(xpaths, colnames) {
		standardGeneric("fhir_columns")
	}
)

#' @rdname fhir_columns-methods
#' @aliases fhir_columns,missing,missing-method
setMethod(
	f          = "fhir_columns",
	signature  = c(
		xpaths   = "missing",
		colnames = "missing"),
	definition = function() {
		new(Class = "fhir_columns")
	}
)

#' @rdname fhir_columns-methods
#' @aliases fhir_columns,NULL,missing-method
#'
setMethod(
	f          = "fhir_columns",
	signature  = c(
		xpaths   = "NULL",
		colnames = "missing"),
	definition = function(xpaths) {
		new(Class = "fhir_columns")
	}
)

#' @rdname fhir_columns-methods
#' @aliases fhir_columns,character,character-method
setMethod(
	f          = "fhir_columns",
	signature  = c(
		xpaths   = "character",
		colnames = "character"
	),
	definition = function(xpaths, colnames){
		new(
			Class = "fhir_columns",
			fhir_xpath_expression(expression = xpaths),
			names = colnames
		)
	}
)

#' @rdname fhir_columns-methods
#' @aliases fhir_columns,character,missing-method
setMethod(
	f = "fhir_columns",
	signature = c(xpaths = "character", colnames = "missing"),
	definition = function(xpaths) {
		if(is.null(names(xpaths))) {
			new(Class = "fhir_columns", fhir_xpath_expression(expression = xpaths), names = as.character(gsub("/", ".", xpaths)))
		} else {
			new(Class = "fhir_columns", fhir_xpath_expression(expression = xpaths), names = names(xpaths))
		}
	}
)

#' @rdname fhir_columns-methods
#' @aliases fhir_columns,list,missing-method
setMethod(
	f = "fhir_columns",
	signature = list(xpaths = "list", colnames = "missing"),
	definition = function(xpaths) {

		if(any(!sapply(xpaths, is.character))) {
			stop("xpaths can only contain elements of type character")
		}

		if(any(sapply(xpaths, function(x){length(x)>1}))) {
			stop("xpaths can only contain character vectors of length one.")
		}

		if(is.null(names(xpaths))) {
			new(Class = "fhir_columns", fhir_xpath_expression(expression = unlist(xpaths)), names = gsub("/", ".", unlist(xpaths)))
		} else {
			new(Class = "fhir_columns", fhir_xpath_expression(expression = unlist(xpaths)), names = names(xpaths))
		}

	}
)

setMethod(
	f = "show",
	signature = "fhir_columns",
	definition = function(object) {

		if(length(object) == 0) {
			cat("An empty fhir_columns object\n");
			return()
		}

		pairs <- paste(names(object), object, sep = "=")
		colwidth1 <- max(c(nchar(names(object)), nchar('column_name'))) + 1
		colwidth2 <- max(c(nchar(object), nchar('xpath_expression'))) + 1
		horiz_bar <- paste0(
			paste0(rep("-", colwidth1), collapse = ""), ' ',
			paste0(rep("-", colwidth2), collapse = ""), '\n'
		)
		header <- paste0(
			horiz_bar,
			stringr::str_pad(string = "column name", width = colwidth1 , side = "right"),
			"| xpath expression", "\n",
			horiz_bar
		)

		cat(
			paste0(
				header,
				paste(
					paste0(stringr::str_pad(string = names(object), width = colwidth1, side = "right"), "| ", object),
					collapse = "\n"
				), '\n',
				horiz_bar
			)
		)
	}
)
