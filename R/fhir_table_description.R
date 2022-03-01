#' A S4 class describing the form of a table produced by [fhir_crack()]
#'
#' A `fhir_table_description` is part of a `fhir_design` and holds the information [fhir_crack()] needs to flatten (aka crack)
#' FHIR resources from a FHIR bundle and is created with [fhir_table_description()].
#' There should be one `fhir_table_description` per resource type as [fhir_crack()] will create one data.frame/data.table
#' per resource type. See Details.
#'
#' @details
#' A `fhir_table_description` consists of
#' the following elements:
#'
#' - The resource element: Defines the resource type (e.g. `Patient` or `Observation`). See [fhir_resource_type()].
#' - The cols element: Contains the column names and XPath expressions defining the columns to extract.
#' If this element is empty, [fhir_crack()] will extract all available elements of the resource and name the
#' columns automatically. See [fhir_columns()].
#' - The sep element: A character of length one containing the separator string used for separating multiple entries in cells.
#' - The brackets element: A character of length one or two used for the indices of multiple entries. The first one is the opening bracket
#' and the second one the closing bracket. Vectors of length one will be recycled.
#' Defaults to `character(0)`, i.e. no brackets, meaning that multiple entries won't be indexed.
#' - The rm_empty_cols element: A logical of length one indicating whether empty columns should be removed in the resulting table or not. Defaults to `FALSE`.
#' - The format element: A character of length one indicating whether the resulting table should be cracked to a `wide` or `compact` format.
#' `wide` means multiple entries will be distributed over several columns with indexed names. `compact` means multiple entries will be pasted into one cell/column separated by `sep` .
#' Defaults to `compact`.
#' - The keep_attr element: A logical of length one indicating whether the attribute name of the respective element (`@value` in most cases)
#' should be attached to the name of the variable in the resulting table. Defaults to `FALSE`.
#'
#' A full `fhir_table_description` looks for example like this:
#' ```
#' fhir_resource_type: Patient
#'
#' fhir_columns:
#' column name | xpath expression
#' ------------------------
#' name        | name/family
#' gender      | gender
#' id          | id
#'
#'sep:           ':::'
#'brackets:      '[', ']'
#'rm_empty_cols: FALSE
#'format:        'compact'
#'keep_attr:     FALSE
#'```
#'
#' @slot resource An object of class [fhir_resource_type-class] defining the resource type that
#' should be extracted.
#' @slot cols An object of class [fhir_columns-class] describing which columns should be created and how.
#' If this is an empty [fhir_columns-class] object, the call to [fhir_crack()] will extract all available
#' elements and put them in automatically named columns.
#' @slot sep A character of length one containing the separator string used for separating multiple entries in cells when `format = "compact"`.
#' ignored when `format = "wide"`.
#' @slot brackets A character of length one or two used for the indices of multiple entries. The first one is the opening bracket
#' and the second one the closing bracket. Vectors of length one will be recycled.
#' Defaults to `character(0)`, i.e. no brackets, meaning that multiple entries won't be indexed.
#' @slot rm_empty_cols A logical of length one indicating whether empty columns should be removed from the resulting table or not. Defaults to FALSE.
#' @slot format A character of length one indicating whether the resulting table should be cracked to a `wide` or `compact` format.
#' `wide` means multiple entries will be distributed over several columns with indexed names. `compact` means multiple entries will be pasted into one cell/column separated by `sep` .
#' Defaults to `compact`.
#' @slot keep_attr A logical of length one indicating whether the attribute name of the respective element (`@value` in most cases)
#' should be attached to the name of the variable in the resulting table. Defaults to `FALSE`
#'
#' @include fhir_resource_type.R fhir_columns.R
#' @seealso [fhir_resource_type()],[fhir_columns()], [fhir_design()], [fhir_crack()]
#' @export
setClass(
	"fhir_table_description",
	slots = c(
		resource      = "fhir_resource_type",
		cols          = "fhir_columns",
		sep           = "character",
		brackets      = "character",
		rm_empty_cols = "logical",
		format        = "character",
		keep_attr     = "logical"
	)
)

setValidity(
	Class  = "fhir_table_description",
	method = function(object) {

		messages <- c()
		if(1 < length(object@sep)) {messages <- c(messages, "sep must be character of length one.")}
		if(!length(object@brackets) %in% c(0, 2)) {messages <- c(messages, "brackets must be character of length two or empty.")}
		if("" %in% object@brackets) {messages <- c(messages, "You cannot use \"\" for brackets.")}
		if(1 < length(object@rm_empty_cols)) {messages <- c(messages, "rm_empty_cols must be logical of length one.")}
		if(!object@format %in% c("wide", "compact")) {messages <- c(messages, "format must be either 'compact' or 'wide'.")}
		if(1 < length(object@keep_attr)) {messages <- c(messages, "keep_attr must be logical of length one.")}

		if(0 < length(messages)) {messages} else {TRUE}
	}
)

#' Create [fhir_table_description-class] object
#'
#' A `fhir_table_description` is part of a `fhir_design` and holds the information [fhir_crack()] needs to flatten (aka crack)
#' FHIR resources from a FHIR bundle. There should be one `fhir_table_description` per resource type as
#' [fhir_crack()] will create one data.frame/data.table per resource type. See Details.
#'
#' @details
#' A `fhir_table_description` consists of
#' the following elements:
#'
#' - The resource element: Defines the resource type (e.g. `Patient` or `Observation`). See [fhir_resource_type()].
#' - The cols element: Contains the column names and XPath expressions defining the columns to extract.
#' If this element is empty, [fhir_crack()] will extract all available elements of the resource and name the
#' columns automatically. See [fhir_columns()].
#' - The sep element: A character of length one containing the separator string used for separating multiple entries in cells.
#' - The brackets element: A character of length one or two used for the indices of multiple entries. The first one is the opening bracket
#' and the second one the closing bracket. Vectors of length one will be recycled.
#' Defaults to `character(0)`, i.e. no brackets, meaning that multiple entries won't be indexed.
#' - The rm_empty_cols element: A logical of length one indicating whether empty columns should be removed in the resulting table or not. Defaults to `FALSE`.
#' - The format element: A character of length one indicating whether the resulting table should be cracked to a `wide` or `compact` format.
#' `wide` means multiple entries will be distributed over several columns with indexed names. `compact` means multiple entries will be pasted into one cell/column separated by `sep` .
#' Defaults to `compact`.
#' - The keep_attr element: A logical of length one indicating whether the attribute name of the respective element (`@value` in most cases)
#' should be attached to the name of the variable in the resulting table. Defaults to `FALSE`.
#'
#'
#' A full `fhir_table_description` looks for example like this:
#' ```
#' fhir_resource_type: Patient
#'
#' fhir_columns:
#' column name | xpath expression
#' ------------------------
#' name        | name/family
#' gender      | gender
#' id          | id
#'
#'sep:           ':::'
#'brackets:      '[', ']'
#'rm_empty_cols: FALSE
#'format:        'compact'
#'keep_attr:     FALSE
#'```
#'
#' @param resource A character vector of length one or [fhir_resource_type-class] object
#' indicating which resource type should be extracted.
#' @param cols Optional. A [fhir_columns-class] object or something that can be coerced to one,
#' like a (named) character vector, a (named) list containing xpath expressions,
#' or a [fhir_xpath_expression-class] object. See [fhir_columns()] and the examples.
#' If this argument is omitted, an empty [fhir_columns-class] object will be supplied.
#' This means that in the call to [fhir_crack()], all available elements are extracted in put
#' in automatically named columns.
#' @param sep  A character of length one containing the separator string used for separating multiple entries in cells when `format = "compact"`.
#' ignored when `format = "wide"`. Defaults to `":::"`.
#' @param brackets A character of length one or two used for the indices of multiple entries. The first one is the opening bracket
#' and the second one the closing bracket. Vectors of length one will be recycled.
#' Defaults to `character(0)`, i.e. no brackets, meaning that multiple entries won't be indexed.
#' @param rm_empty_cols A logical of length one indicating whether empty columns should be removed from the resulting table or not. Defaults to `FALSE`.
#' @param format  A character of length one indicating whether the resulting table should be cracked to a `wide` or `compact` format.
#' `wide` means multiple entries will be distributed over several columns with indexed names. `compact` means multiple entries will be pasted into one cell/column separated by `sep` .
#' Defaults to `compact`.
#' @param keep_attr A logical of length one indicating whether the attribute name of the respective element (`@value` in most cases)
#' should be attached to the name of the variable in the resulting table. Defaults to `FALSE`.
#' @param style Deprecated since fhircrackr 2.0.0. Can at the moment still be used for backwards compatibility but will throw an warning.
#' @return An object of class [fhir_table_description-class].
#'
#' @examples
#' # a minimal table description
#' fhir_table_description(
#'     resource = "Patient"
#' )
#'
#' # named list for cols
#' fhir_table_description(
#'     resource = "Patient",
#'     cols     = list(
#'         id     = "id",
#'         name   = "name/family",
#'         gender = "gender"
#'     )
#' )
#'
#' #unnamed character for cols, colnames are generated automatically
#' fhir_table_description(
#'     resource = "Patient",
#'     cols     = c(
#'         "id",
#'         "name/family",
#'         "gender"
#'     )
#' )
#'
#' # named character for cols, and overwritten default for other arguments
#' fhir_table_description(
#'     resource = "Patient",
#'     cols = c(
#'         id            = "id",
#'         name          = "name/family",
#'         gender        = "gender"
#'     ),
#'     brackets      = c("[", "]"),
#'     rm_empty_cols = TRUE,
#'     format        = "wide"
#' )
#'
#' # no column arguments is given -> would create a column for all available elements
#' fhir_table_description(
#'     resource = "Patient",
#'     sep           = " <~> ",
#'     brackets      = c("<<<", ">>>"),
#'     rm_empty_cols = FALSE,
#'     format        = "wide"
#' )
#'
#' @export
fhir_table_description <- function(
	resource,
	cols          = fhir_columns(),
	sep           = ':::',
	brackets      = character(),
	rm_empty_cols = FALSE,
	format        = 'compact',
	keep_attr     = FALSE,
	style = NULL
	) {

	if(!is.null(style)){
		warning("Have to overwrite fhir_table_description arguments because deprecated fhir_style is used.\n")
		sep <- style@sep
		brackets <- style@brackets
		rm_empty_cols <- style@rm_empty_cols
	}

	resource <- fhir_resource_type(string = resource)

	brackets <- fix_brackets(brackets = brackets)

	if(class(cols) != 'fhir_columns') {
		cols <- fhir_columns(xpaths = cols)
	}

	new(
		Class         = 'fhir_table_description',
		resource      = resource,
		cols          = cols,
		sep           = sep,
		brackets      = brackets,
		rm_empty_cols = rm_empty_cols,
		format        = format,
		keep_attr     = keep_attr
	)
}

setMethod(
	f = 'show',
	signature = 'fhir_table_description',
	function(object) {
		cat('A fhir_table_description with the following elements: \n\n')
		cat(paste0('resource: ', as.character(object@resource), '\n\n'))
		cat('cols: \n');
		show(object@cols)
		cat(paste0('\nsep:           \'', object@sep, '\'\n'))
		if(length(object@brackets) < 1) {
			cat('brackets:      no brackets\n')
		} else {
			cat(paste0('brackets:      \'', object@brackets[1], '\', \'', object@brackets[2], '\'\n'))
		}
		cat(paste0('rm_empty_cols: ', object@rm_empty_cols, '\n'))
		cat(paste0('format:        \'', object@format, '\'\n'))
		cat(paste0('keep_attr:     ', object@keep_attr, '\n'))
	}
)
