
#' A S4 class to represent columns in a [fhir_df_description-class]
#'
#' An object of class [fhir_columns-class] is part of a [fhir_df_description-class]
#' in a [fhir_design-class] and holds information on the elements
#' that should be extracted from the FHIR resources, as well as the column names of the resulting data.frame.
#' The elements to be extracted are indicated by XPath expressions.
#'
#' @slot names The column names
#' @include fhir_xpath_expression.R
#'
setClass(
	"fhir_columns",
	 contains = "fhir_xpath_expression",
	 slots = c(names = "character")
)

setValidity(
	"fhir_columns",
	function(object) {
		messages <- c()
		if(length(names(object))==0) {
			messages <- c(messages, "fhir_columns has to be a *named* character.")
		}
		if(0 < length(messages)) {messages} else {TRUE}
	}
)

#' Create [fhir_columns-class] object
#'
#' An object of class [fhir_columns-class] is part of a fhir_design and holds information on the elements
#' that should be extracted from the FHIR resources, as well as the column names of the resulting data.frame.
#' The elements to be extracted are indicated by XPath expressions. If no column names are provided,
#' they are generated automatically and reflect the elements position in the resource.
#'
#' @param expressions A (named) character vector or (named) list containing xpath expressions,
#' or a [fhir_xpath_expressions-class] object.
#' @param colnames The names of the columns to create. If no colnames are provided and the list or vector
#' in `expressions` has names, those names are taken as the colnames. If no colnames are provided and
#' `expressions` is unnamed too, the colnames are generated automatically from the xpath expressions. See examples.
#'
#'
#' @examples
#'  #provide colnames explicitly
#'  fhir_columns(expressions = c("name/given", "code/coding/code"),
#'               colnames = c("name", "code"))
#'
#'  #colnames are taken from expressions argument
#'  fhir_columns(expressions = c(name = "name/given", code = "code/coding/code"))
#'
#'  #colnames are taken from expressions argument
#'  fhir_columns(expressions = list(name = "name/given", code = "code/coding/code"))
#'
#'  #colnames are generated automatically
#'  fhir_columns(expressions = c("name/given", "code/coding/code"))
#'



setGeneric(
	"fhir_columns",
	function(expressions, colnames){
		standardGeneric("fhir_columns")
	}
)

setMethod(
	"fhir_columns",
	signature = c(expressions = "missing", colnames = "missing"),
	function(){
		new("fhir_columns")
	}
)

setMethod(
	"fhir_columns",
	signature = c(expressions = "NULL", colnames = "missing"),
	function(expressions){
		new("fhir_columns")
	}
)

setMethod(
	"fhir_columns",
	signature = c(expressions = "character", colnames = "character"),
	function(expressions, colnames){
		new("fhir_columns", fhir_xpath_expression(expressions), names = colnames)
	}
)

setMethod(
	"fhir_columns",
	signature = c(expressions = "character", colnames = "missing"),
	function(expressions){
		if(is.null(names(expressions))){
			new("fhir_columns", fhir_xpath_expression(expressions), names = as.character(sub("/", ".", expressions)))
		}else{
			new("fhir_columns", fhir_xpath_expression(expressions), names = names(expressions))
		}
	}
)

setMethod(
	"fhir_columns",
	signature = list(expressions = "list", colnames = "missing"),
	function(expressions){
		if(any(!sapply(expressions, is.character))){
			stop("expressions can only contain elements of type character")
		}
		if(any(sapply(expressions, function(x){length(x)>1}))){
			stop("expressions can only contain character vectors of length 1.")
		}
		if(is.null(names(expressions))){
			new("fhir_columns", fhir_xpath_expression(unlist(expressions)), names = sub("/", ".", unlist(expressions)))
		}else{
			new("fhir_columns", fhir_xpath_expression(unlist(expressions)), names = names(expressions))
		}
	}
)

setMethod(
	"show",
	"fhir_columns",
	function(object){
		if(length(object)==0){cat("An empty fhir_columns object"); return()}

		pairs <- paste(names(object), object, sep = "=")
		colwidth1 <- max(c(stringr::str_length(names(object)),11)) + 1
		colwidth2 <- max(stringr::str_length(object)) + 1
		header <- paste(
			stringr::str_pad("column name", colwidth1 - 1, side="right"),
			"| xpath expression", "\n",
			paste(rep("-", colwidth1 + colwidth2), collapse=""),
			"\n",
			collapse = ""
		)
		cat(
			paste0(
				"A fhir_columns object:\n\n",
				header,
				paste(
					paste0(stringr::str_pad(names(object), colwidth1, side="right"), "| ", object),
					collapse = "\n"
				)
			)
		)

	}
)

