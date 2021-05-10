#' A S4 class containing a design for [fhir_crack()]
#'
#' A fhir_design is a named list of [fhir_df_description-class] objects. Each df_description
#' contains information on how to flatten one resource type which will result in one
#' data.frame. The fhir_design is passed to the function [fhir_crack()] along with a
#' list of bundles containing FHIR resources.
#'
#' @slot .Data The list of `fhir_df_description` objects.
#' @slot names The names of the df_descriptions. Those will also be the names of the
#' resulting data.frames.
#' @include fhir_df_description.R
#' @seealso [fhir_df_description()], [fhir_crack()]
#' @export
#'
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
		#check df descriptions
	 	messages <- c(messages, unlist(sapply(object, function(o){
						 		v <- validObject(o, complete=T, test=T)
						 		if(is.character(v)){v}
						 		}))
	 		)
		if(0 < length(messages)) {messages} else {TRUE}
	}
)

#' Create a [fhir_design-class] object
#'
#' A `fhir_design` is a named list of `fhir_df_description` objects (See [fhir_df_description()])
#' and should be created using the function described here. The design is used in [fhir_crack()] to tell
#' the function how to flatten each resource type.
#'
#'
#' @details
#' For backwards compatibility it is for the moment also possible to build it from an
#' old-style design as used in `fhircrackr (< 1.0.0)`. See examples.
#'
#' If this function is given an object of class [fhir_df_list-class] or [fhir_dt_list-class], it will
#' extract the design that was used to create the respective list.
#'
#' A `fhir_design` looks for example like this:
#'
#' ```
#' A fhir_design with 2 df_descriptions:
#' =====================================================
#' Name: Patients
#'
#' Resource type: Patient
#'
#' Columns:
#' column name | xpath expression
#' ------------------------
#' name        | name/family
#' gender      | gender
#' id          | id
#'
#' Style:
#' sep: ||
#' brackets: '[' ']'
#' rm_empty_cols: FALSE
#' =====================================================
#' Name: Observations
#'
#' Resource type: MedicationAdministration
#'
#' Columns:
#' An empty fhir_columns object
#'
#' Style:
#' sep: ' '
#' brackets: character(0)
#' rm_empty_cols: TRUE
#' ```
#' See the examples for how to create this design.
#'
#' @param ... One ore more `fhir_df_description` objects or a named list containing
#' `fhir_df_description` objects, or an object of class [fhir_df_list-class]/[fhir_dt_list-class].
#' See [fhir_df_description()].
#' @param names Optional. The names of the df_descriptions. If no names are provided, the names of the object(s) that
#' were used in creating the design are taken as the names.
#' @docType methods
#' @seealso [fhir_df_description()], [fhir_crack()]
#' @rdname fhir_design-methods
#' @examples
#'
#' ####Example 1####
#'
#' ###create fhir_df_descriptions
#'
#' #most explicit, long form
#' pat <- fhir_df_description(resource = "Patient",
#'                     cols = c(name = "name/family",
#'                              gender = "gender",
#'                              id = "id"),
#'                     style = fhir_style(sep = "||",
#'                                        brackets = c("[", "]"),
#'                                        rm_empty_cols = FALSE
#'                             )
#'              )
#'  print(pat)
#'
#' #most reduced form:
#' #All columns are extracted, default style is assumed
#' meds <- fhir_df_description(resource = "MedicationAdministration")
#' print(meds)
#'
#' #create design
#' #First option: explicitly define names
#' design1 <- fhir_design(pat, meds, names = c("Patients", "Medications"))
#' print(design1)
#'
#' #Second option: Names are taken from the df_descriptions
#' design2 <- fhir_design(pat, meds)
#' print(design2)
#'
#' #don't do this, because names will be weird:
#' design2a <- fhir_design(fhir_df_description(resource = "MedicationAdministration"))
#' print(design2a)
#'
#' #Third option: named list
#' design3 <- fhir_design(list(Patients = pat, Medications = meds))
#' print(design3)
#'
#'
#' ####Example 2####
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
#'                  Medications = list(
#'                     resource = "//Medication"
#'                  )
#'               )
#'
#' new_design <- fhir_design(old_design)
#' print(new_design)
#'
#' ###Example 3###
#' ###Extract design from fhir_df_list/fhir_dt_list
#'
#' #unserialize and crack example bundles
#' med_bundles <- fhir_unserialize(medication_bundles)
#' dfs <- fhir_crack(med_bundles, design = design1)
#'
#' #extract design
#' fhir_design(dfs)
#'
#' @export
#'
setGeneric(
	"fhir_design",
	function(..., names){
		standardGeneric("fhir_design")
	},
	signature = "..."
)

#method for fhir_table_list in fhir_table_list.R
#' @rdname fhir_design-methods
#' @aliases fhir_design,fhir_df_description-method
setMethod(
	"fhir_design",
	signature = c(...="fhir_df_description"),
	function(..., names){
		argnames <- sapply(substitute(list(...))[-1], deparse)
		if(missing(names)){names <-argnames}
		args <- list(...)
		new("fhir_design", args, names = names)
	}
)

#' @rdname fhir_design-methods
#' @aliases fhir_design,list-method
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
					resource <- fhir_resource_type(gsub(paste0(esc("."),"|", esc("/")), "", x$resource))
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
		if(length(object)==0){
			cat("An empty fhir_design_object")
		}else{
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
			}


	})