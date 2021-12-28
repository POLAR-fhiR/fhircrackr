#' A S4 class describing the form of data.frame produced by [fhir_crack()]
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
#' - The brackets element: A characters of length one or two used for separating multiple entries in cells. The first one is the opening bracket
#' and the second one the closing bracket. If only one is given, both are assumed opening and closing brackets are equal.
#' Defaults to character(0). If, as in this case, no brackets are given, then no indexing will applied on multiple entries.
#' - The rm_empty_cols element: A logical of length one indicating whether empty columns should be removed of the resulting table or not. Defaults to FALSE.
#' - The format element: A character of length one indicating whether the resulting table should be cracked 'wide' or 'compact'.
#' Cracking 'wide' means multiple entries will be distributed over several columns with indexed names.
#' Otherwise multiple entries will be pasted separated by 'sep' into one cell/column. Defaults to 'compact'.
#' - The keep_attr element: A logical of length one indicating whether the names of the table columns end with or without the attributes' name,
#' e.g. name.given oder name.given.value. This effects only, if no 'cols' argument is given. That means the column names are created by
#' the xpath of their certain tags.
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
#'
#' @slot resource An object of class [fhir_resource_type-class] defining the resource type that
#' should be extracted.
#' @slot cols An object of class [fhir_columns-class] describing which columns should be created and how.
#' If this is an empty [fhir_columns-class] object, the call to [fhir_crack()] will extract all available
#' elements and put them in automatically named columns.
#' @slot sep An character of length one containing the separator string used for separating multiple entries in cells.
#' @slot brackets A characters of length one or two used for separating multiple entries in cells. The first one is the opening bracket
#' and the second one the closing bracket. If only one is given, both are assumed opening and closing brackets are equal.
#' Defaults to NULL. NULL means no indexing for multiple entries.
#' @slot rm_empty_cols A logical of length one indicating whether empty columns should be removed of the resulting table or not. Defaults to FALSE.
#' @slot format A character of length one indicating whether the resulting table should be cracked 'wide' or 'compact'.
#' Cracking 'wide' means multiple entries will be distributed over several columns with indexed names.
#' Otherwise multiple entries will be pasted separated by 'sep' into one cell/column. Defaults to 'compact'.
#' @slot keep_attr A logical of length one indicating whether the names of the table columns end with or without the attributes' name,
#' e.g. name.given oder name.given.value. This effects only, if no 'cols' argument is given. That means the column names are created by
#' the xpath of their certain tags.
#' @include fhir_resource_type.R fhir_columns.R
#' @seealso [fhir_resource_type()],[fhir_columns()], [fhir_design()], [fhir_crack()]
#' @export
setClass(
	Class = "fhir_table_description",
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
#' - The resource element: Defines the resource type (e.g. `Patient` or `Observation`). See `?fhir_resource`.
#' - The cols element: Contains the column names and XPath expressions defining the columns to extract.
#' If this element is empty, [fhir_crack()] will extract all available elements of the resource and name the
#' columns automatically. See `?fhir_columns`.
#' - The sep element: A character of length one containing the separator string used for separating multiple entries in cells.
#' - The brackets element: A characters of length one or two used for separating multiple entries in cells. The first one is the opening bracket
#' and the second one the closing bracket. If only one is given, both are assumed opening and closing brackets are equal.
#' - The rm_empty_cols element: A logical of length one indicating whether empty columns should be removed of the resulting table or not.
#' - The format element: A character of length one indicating whether the resulting table should be cracked 'wide' or 'compact'.
#' Cracking 'wide' means multiple entries will be distributed over several columns with indexed names.
#' Otherwise multiple entries will be pasted separated by 'sep' into one cell/column. Defaults to 'compact'.
#' - The keep_attr element: A logical of length one indicating whether the names of the table columns end with or without the attributes' name,
#' e.g. name.given oder name.given.value. This effects only, if no 'cols' argument is given. That means the column names are created by
#' the xpath of their certain tags.
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
#' separator            : ':::'
#' bracket open         : '['
#' bracket close        : ']'
#' remove empty columns : TRUE
#' ```
#' @param resource A character vector of length one or [fhir_resource_type-class] object
#' indicating which resource type should be extracted.
#' @param cols Optional. A [fhir_columns-class] object or something that can be coerced to one,
#' like a (named) character vector, a (named) list containing xpath expressions,
#' or a [fhir_xpath_expression-class] object. See [fhir_columns()] and the examples.
#' If this argument is omitted, an empty [fhir_columns-class] object will be supplied.
#' This means that in the call to [fhir_crack()], all available elements are extracted in put
#' in automatically named columns.
#'
#' @return An object of class [fhir_table_description-class].
#'
#' @examples
#' # a minimal table description
#' fhir_table_description(
#'     resource = 'Patient'
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
#' # named character for cols, and redundantly given defaults for all arguments
#' fhir_table_description(
#'     resource = 'Patient',
#'     cols = c(
#'         id            = 'id',
#'         name          = 'name/family',
#'         gender        = 'gender'
#'     ),
#'     sep           = ':::',
#'     brackets      = c('<|', '|>'),
#'     rm_empty_cols = FALSE,
#'     format        = 'compact',
#'     format        = 'compact'
#' )
#'
#' # no column arguments is given,
#' # so we may use 'keep_attr' and set it to TRUE for later recreating bundles
#' fhir_table_description(
#'     resource = 'Patient',
#'     sep           = ' <~> ',
#'     brackets      = c('<<<', '>>>'),
#'     rm_empty_cols = FALSE,
#'     format        = 'wide',
#'     keep_attr     = TRUE
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
	keep_attr     = FALSE) {

	resource <- fhir_resource_type(string = resource)
	if(0 < length(cols) && keep_attr) {
		warning(paste0(
			'Do you really want to add attributes to your given column names? ',
			'If not, then leave/set keep_attr to its default value FALSE'
		))
	}
	brackets <- fix_brackets(brackets)
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
		cat(paste0('fhir_resource_type: ', as.character(object@resource), '\n\n'))
		cat('fhir_columns: \n');
		show(object@cols)
		cat(paste0('\n\nsep:           \'', object@sep, '\'\n'))
		if(length(object@brackets) < 1) {
			cat('brackets:      no brackets\n')
		} else {
			cat(paste0('brackets:      \'', object@brackets[1], '\', \'', object@brackets[2], '\'\n'))
		}
		cat(paste0('rm_empty_cols: ', object@rm_empty_cols, '\n'))
		cat(paste0('format:        \'', object@format, '\'\n'))
		cat(paste0('keep_attr:     ', object@keep_attr, '\n\n'))
	}
)
