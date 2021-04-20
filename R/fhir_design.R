#' A S4 class containing a design for [fhir_crack()]
#'
#' A fhir_design is a named list of [fhir_df_desciption-class] objects. Each df_description
#' contains information on how to flatten one resource type which will result in one
#' data.frame. The fhir_design is passed to the function [fhir_crack()] along with a
#' list of bundles containing FHIR resources.
#'
#' @slot names The names of the df_descriptions. Those will also be the names of the
#' resulting data.frames.
#'
#' @include fhir_df_description.R
setClass(
	"fhir_design",
	contains = "list",
	slots = c(names="character")
)

setValidity(
	"fhir_design",
	function(object){
		messages <- c()
		if(length(object)!=length(object@names)){
			messages <- c(messages, "You need exactly one name for every df_description in a design.")
		}
		if(any(sapply(object, function(x){class(x)!="fhir_df_description"}))){
			messages <- c(messages, "A fhir_design can only contain fhir_df_descriptions")
		}
		if(0 < length(messages)) {messages} else {TRUE}
	}
)

#' Create a [fhir_design-class] object
#'
#' A [fhir_design-class] is a list of [fhir_df_description-class] objects and should be created
#' using the function described here. For backwards compatibility
#' it is for the moment also possible to build it from an old-style design as used in
#' `fhircrackr (< 1.0.0)`. See examples.
#'
#' @param ... One ore more [fhir_df_description-class] objects or a named list containing
#' [fhir_df_description-class] objects, see examples.
#' @param names The names of the df_descriptions. This argument is not necessary when df_descriptions are
#' provided in a named list and will be ignored in this case.
#' The names will also be the names of the resulting data.frames in [fhir_crack()].
#'
#' @examples
#'
#' ###Example 1 ####
#'
#' #create fhir_df_descriptions
#'
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
#' #create design
#'
#' #First option
#' design <- fhir_design(df_desc1, df_desc2, names = c("Patients", "Observations"))
#'
#' #second option
#' design <- fhir_design(list(Patients = df_desc1, Observations = df_desc2))
#'
#' #have a look at the design
#' design
#'
#' ###Example 2###
#' #This option will be deprecated at some point
#'
#' #old style design
#' old_design <- list(
#'                  Patients = list(
#'                     resource = "//Patient",
#'                     cols = list(
#'                        name = "name/family",
#'                        gender = "gender",
#'                        id = "id"),
#'                     style = list(
#'                        sep = "||",
#'                        brackets = c("[", "]"),
#'                        rm_empty_cols = FALSE
#'                     )
#'                  ),
#'                  Observations = list(
#'                     resource = "//Observation",
#'                     cols = list(
#'                        code.coding.system = "code/coding/system",
#'                        code.coding.code = "code/coding/code"
#'                     )
#'                  )
#'               )
#'
#' new_design <- fhir_design(old_design)
#'
#' #have a look at the design
#' new_design
#'

setGeneric(
	"fhir_design",
	function(..., names){
		standardGeneric("fhir_design")
	},
	signature = "..."
)

setMethod(
	"fhir_design",
	signature = c(...="fhir_df_description"),
	function(..., names){
		args <- list(...)
		new("fhir_design", args, names = names)
	}
)

setMethod(
	"fhir_design",
	signature = c(...="list"),
	function(...){
		args <- list(...)
		if(length(args) == 1){

			args <- unlist(args, recursive = F)

			if(all(sapply(args, is, "fhir_df_description"))){
				new("fhir_design", args, names  = attr(args, "names"))

			}else{
				message("The old style design (simple named list) will be deprecated at some point. ",
						"Please consider building your design as shown in the documentation for fhir_design(), ",
						"see ?fhir_design.")
				d <- fix_design(args)

				df_desc <-lapply(d, function(x){
					resource <- fhir_resource(gsub(paste0(esc("."),"|", esc("/")), "", x$resource))
					style <- fhir_style(sep = x$style$sep, brackets =x$style$brackets, rm_empty_cols = x$style$rm_empty_cols)
					fhir_df_description(resource, fhir_columns(x$cols), style)
				})

				new("fhir_design", df_desc, names = attr(d, "names"))
			}
		}else {
			stop("You can only provide one list to fhir_design()")
		}
	}
)

setMethod(
	"show",
	"fhir_design",
	function(object){
		cat(paste0("A fhir_design with ", length(object), " df_descriptions:\n"))
		lapply(1:length(object), function(i){
			df_desc <- object[[i]]
			cat("=====================================================\n")
			cat(paste0("Name: ", names(object)[i]))
			cat("\n\n")
			cat(paste0("Resource type: ", as.character(df_desc@resource), "\n\n"))
			cat("Columns: \n"); show(df_desc@cols)
			cat("\n\nStyle: \n");	show(df_desc@style)
			cat("\n")
			})

	})