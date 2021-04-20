#' A S4 class describing the form of data.frame produced by [fhir_crack()]
#'
#' A fhir_df_description is an element of a [fhir_design-class] and describes how resources
#' of one particular resource type should be transformed into a data.frame/data.table.
#'
#' When the bundle to be cracked contains more than one resource type, each of those
#' types will be cracked into its own data.frame and needs its own fhir_df_description.
#'
#' @slot resource An object of class [fhir_resource_type-class] defining the resource type that
#' should be extracted
#' @slot cols An object of class [fhir_columns-class] describing which columns should be created and how.
#' If this is an empty [fhir_columns-class] object, the call to [fhir_crack()] will extract all available
#' elements and put them in automatically named columns.
#' @slot style An object of class [fhir_style-class] describing how to deal with multiple entries and emtpy columns.
#' @include fhir_resource_type.R fhir_style.R fhir_columns.R
setClass(
	"fhir_df_description",
	slots = c(resource = "fhir_resource_type",
			  cols = "fhir_columns",
			  style = "fhir_style"
			  )
)

#' Create [fhir_df_description-class] object
#'
#' @param resource A character vector of length one or [fhir_resource_type-class] object,
#' indicating which resource type should be extracted.
#' @param cols A [fhir_columns-class] object or something that can be coerced to one,
#' like a(named) character vector, a (named) list containing xpath expressions,
#' or a [fhir_xpath_expressions-class] object. See [fhir_columns()] and the examples.
#' If this argument is omitted, an empty [fhir_columns-class] object will be supplied.
#' This means that in the call to [fhir_crack()], all available elements are extracted in put
#' in automatically named columns.
#' @param style Optional A [fhir_style-class] object, as created by [fhir_style()].
#' If this argument is omitted, default values will be assumed, see [fhir_style()].
#'
#' @return An object of class [fhir_df_description-class].
#'
#' @examples
#' #named character for cols
#' fhir_df_description(resource = "Patient",
#'                     cols = c(name = "name/family",
#'                              gender = "gender",
#'                              id = "id"),
#'                     style = fhir_style(sep = "||",
#'                                        brackets = c("[", "]"),
#'                                        rm_empty_cols = FALSE
#'                     )
#' )
#'
#' #' #If style is ommitted, default values are assumed
#' fhir_df_description(resource = "Patient",
#'                     cols = c(name = "name/family",
#'                              gender = "gender",
#'                              id = "id")
#' )
#'
#' #named list for cols
#' fhir_df_description(resource = "Patient",
#'                     cols = list(name = "name/family",
#'                              gender = "gender",
#'                              id = "id")
#' )
#'
#' #unnamed character for cols, colnames are generated automatically
#' fhir_df_description(resource = "Patient",
#'                     cols = c("name/family",
#'                                 "gender",
#'                                 "id")
#' )
#'


fhir_df_description <- function(resource,
								cols = fhir_columns(),
								style = fhir_style()){

	resource <- fhir_resource_type(resource)
	if(class(cols)!="fhir_columns"){cols <- fhir_columns(cols)}

	new("fhir_df_description", resource = resource, cols = cols, style = style)

}


setMethod(
	"show",
	"fhir_df_description",
	function(object){
		cat("A fhir_df_description with the following elements: \n\n")
		cat(paste0("fhir_resource_type: ", as.character(df_desc@resource), "\n\n"))
		cat("fhir_columns: \n"); show(df_desc@cols)
		cat("\n\nfhir_style: \n");	show(df_desc@style)
		cat("\n")
	}
)
