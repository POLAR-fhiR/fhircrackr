
#' Virtual superclass for [fhir_df_list-class] and [fhir_dt_list-class]
#'
#' @slot names Character vector containing the names of the tables
#' @slot indexed Logical vector indicating which of the tables has indices for multiple entries
#' @slot design An object of class [fhir_design-class] that was used to create the table list
#' @include fhir_design.R
setClass(
	"fhir_table_list",
	contains = c("VIRTUAL", "list"),
	slots = c(
		names = "character",
		indexed = "logical",
		design = "fhir_design"
	)
)

setValidity(
	"fhir_table_list",
	function(object){
		messages <- c()
		if(length(object) != length(object@indexed)) {
			messages <- c(messages, "Slot indexed has to have the same length as the list.")
		}
		if(length(object) != length(object@indexed)) {
			messages <- c(messages, "Slot names has to have the same length as the list.")
		}
		if(length(object) != length(object@design)) {
			messages <- c(messages,
						  "The number of df_descriptions in the design doesn't correspond to the number of data.frames")
		}
		if(any(!names(object@design) %in% object@names)){
			messages <- c(messages,
						  "The names in the design don't correspond to the names of the data.frames")
		}
		if(0 < length(messages)) {messages} else {TRUE}
	}
)

#' List of data.frames as returned by [fhir_crack()]
#'
#' Objects of this class are returned by [fhir_crack()] when `data.table=FALSE` (the default).
#' They behave like an ordinary named list of data.frames but have some additional information
#' in the slots `indexed` and `design`.
#'
#' @slot names Character vector containing the names of the data.frames
#' @slot indexed Logical vector indicating which of the data.frames contains indices for multiple entries
#' @slot design An object of class [fhir_design-class] that was used to create the df_list
#'
setClass(
	"fhir_df_list",
	contains = "fhir_table_list"
)

setValidity(
	"fhir_df_list",
	function(object){
		messages <- c()
		if(!all(sapply(object, is.data.frame))){"A fhir_df_list can only contain data.frames."}
		if(0 < length(messages)) {messages} else {TRUE}
	}
)

#' List of data.tables as returned by [fhir_crack()]
#'
#' Objects of this class are returned by [fhir_crack()] when `data.table=TRUE`.
#' They behave like an ordinary named list of data.tables but have some additional information
#' in the slots `indexed` and `design`.
#'
#' @slot names Character vector containing the names of the data.tables
#' @slot indexed Logical vector indicating which of the data.tables contains indices for multiple entries
#' @slot design An object of class [fhir_design-class] that was used to create the dt_list
#'
setClass(
	"fhir_dt_list",
	contains = "fhir_table_list"
)

setValidity(
	"fhir_dt_list",
	function(object){
		messages <- c()
		if(!all(sapply(object, is.data.table))){"A fhir_dt_list can only contain data.tables."}
		if(0 < length(messages)) {messages} else {TRUE}
	}
)
#' create fhir_df_list
#'
#' This function is only for internal use and should not be exported
#' An object of this class should **only** be created inside of [fhir_crack()]
#' @param df_list A named list of data.frames
#' @param design The design that was used to create the list
#'
#' @examples
#' df_desc1 <- fhir_df_description(resource = "Patient",
#'                     cols = c(name = "name/family",
#'                              gender = "gender",
#'                              id = "id"),
#'                     style = fhir_style(sep = "||",
#'                                        brackets = c("[", "]"),
#'                                        rm_empty_cols = FALSE
#'                             )
#'              )
#'
#' df_desc2 <- fhir_df_description(resource = "Observation",
#'                     cols = c("code/coding/system", "code/coding/code")
#'             )
#'
#' design <- fhir_design(list(Patients = df_desc1, Observations = df_desc2))
#'
#' dt_list <- list(Patients = data.frame(), Observations = data.frame())
#'
#' fhir_df_list(df_list, design)
#' @noRd
fhir_df_list <- function(df_list, design){
	indexed <- sapply(design, function(x){length(x@style@brackets)!=0})
	new("fhir_df_list", dt_list, indexed = indexed, design = design)
}
#' create fhir_dt_list
#'
#' This function is only for internal use and should not be exported
#' An object of this class should **only** be created inside of [fhir_crack()]
#' @param dt_list A named list of data.tables
#' @param design The design that was used to create the list
#'
#' @examples
#' df_desc1 <- fhir_df_description(resource = "Patient",
#'                     cols = c(name = "name/family",
#'                              gender = "gender",
#'                              id = "id"),
#'                     style = fhir_style(sep = "||",
#'                                        brackets = c("[", "]"),
#'                                        rm_empty_cols = FALSE
#'                             )
#'              )
#'
#' df_desc2 <- fhir_df_description(resource = "Observation",
#'                     cols = c("code/coding/system", "code/coding/code")
#'             )
#'
#' design <- fhir_design(list(Patients = df_desc1, Observations = df_desc2))
#'
#' dt_list <- list(Patients = data.table(), Observations = data.table())
#'
#' fhir_dt_list(dt_list, design)
#' @noRd
fhir_dt_list <- function(dt_list, design){
	indexed <- sapply(design, function(x){length(x@style@brackets)!=0})
	new("fhir_dt_list", dt_list, indexed = indexed, design = design)
}

#corresponding generic in fhir_design.R
setMethod(
	"fhir_design",
	signature = c(...="fhir_table_list"),
	function(..., names){
		args <- list(...)
		tab <- args[[1]]
		tab@design
	}
)

setMethod(
	"show",
	"fhir_dt_list",
	function(object){
		cat("A fhir_dt_list:\n")
		list <- S3Part(object, strictS3 = T)
		names(list) <- names(object)
		print(list)
	})

setMethod(
	"show",
	"fhir_df_list",
	function(object){
		cat("A fhir_df_list:\n")
		list <- S3Part(object, strictS3 = T)
		names(list) <- names(object)
		print(list)
	})