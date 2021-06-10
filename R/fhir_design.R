#' A S4 class containing a design for [fhir_crack()]
#'
#' A fhir_design is a named list of [fhir_table_description-class] objects. Each table_description
#' contains information on how to flatten one resource type which will result in one
#' data.frame. The fhir_design is passed to the function [fhir_crack()] along with a
#' list of bundles containing FHIR resources.
#'
#' @slot .Data The list of `fhir_table_description` objects.
#' @slot names The names of the table_descriptions. Those will also be the names of the
#' resulting data.frames.
#' @include fhir_table_description.R
#' @seealso [fhir_table_description()], [fhir_crack()]
#' @export
#'
setClass(
	Class = "fhir_design",
	contains = "list",
	slots = c(names="character")
)

setValidity(
	Class = "fhir_design",
	method = function(object) {
		messages <- c()
		if(length(object) != length(object@names)) {
			messages <- c(messages, "You need exactly one name for every table_description in a design.")
		}
		if(any(sapply(object, function(x) {class(x) != "fhir_table_description"}))) {
			messages <- c(messages, "A fhir_design can only contain fhir_table_descriptions")
		}
		#check df descriptions
	 	messages <- c(
	 		messages,
	 		unlist(
	 			sapply(
	 				object,
	 				function(o) {
	 					v <- validObject(o, complete = TRUE, test = TRUE)
	 					if(is.character(v)) {v}
	 				}
	 			)
	 		)
	 	)
		if(0 < length(messages)) {messages} else {TRUE}
	}
)

#' Create a [fhir_design-class] object
#'
#' A `fhir_design` is a named list of `fhir_table_description` objects (See [fhir_table_description()])
#' and should be created using the function described here. The design is used in [fhir_crack()] to tell
#' the function how to flatten each resource type.
#'
#'
#' @details
#'
#' A `fhir_design` looks for example like this:
#'
#' ```
#' A fhir_design with 2 table_descriptions:
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
#' Name: MedicationAdministrations
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
#' The names of the table_descriptions are taken from the names of the arguments. If the table_descriptions are
#' created within the call to `fhir_design` and therefore have no names, the names will be created from the respective
#' resource type. See examples.
#'
#' For backwards compatibility it is for the moment also possible to build it from an
#' old-style design as used in `fhircrackr (< 1.0.0)`. See examples.
#'
#' If this function is given an object of class [fhir_df_list-class] or [fhir_dt_list-class], it will
#' extract the design that was used to create the respective list.
#'
#' @param ... One ore more `fhir_table_description` objects or a named list containing
#' `fhir_table_description` objects, or an object of class [fhir_df_list-class]/[fhir_dt_list-class].
#' See [fhir_table_description()].
#' @docType methods
#' @seealso [fhir_table_description()], [fhir_crack()]
#' @rdname fhir_design-methods
#' @examples
#'
#' ####Example 1####
#'
#' ###create fhir_table_descriptions first
#' #see ?fhir_table_description for explanation
#'
#' pat <- fhir_table_description(resource = "Patient",
#'                     cols = c(name = "name/family",
#'                              gender = "gender",
#'                              id = "id"),
#'                     style = fhir_style(sep = "||",
#'                                        brackets = c("[", "]"),
#'                                        rm_empty_cols = FALSE
#'                             )
#'              )
#'
#' meds <- fhir_table_description(resource = "MedicationAdministration")
#'
#' ###create design
#' #First option: Explicitly define names
#'
#' design1 <- fhir_design(Pats = pat, Medics = meds)
#' print(design1)
#'
#'
#' #Second option: Names are taken from the object names
#'
#' design2 <- fhir_design(pat, meds)
#' print(design2)
#'
#'
#' #Third option: Create table_description within fhir_design
#'
#' design3 <- fhir_design(fhir_table_description(resource = "MedicationAdministration"))
#' print(design3)
#'
#'
#' #Fourth option: Names are taken from named list
#'
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
#' med_bundles <- fhir_unserialize(bundles = medication_bundles)
#' dfs <- fhir_crack(bundles = med_bundles, design = design1)
#'
#' #extract design
#'
#' fhir_design(dfs)
#'
#' @export
#'
setGeneric(
	name = "fhir_design",
	def = function(...) {
		standardGeneric("fhir_design")
		#standardGeneric(f = "fhir_design")
	},
	signature = "..."
)

#method for fhir_table_list in fhir_table_list.R
#' @rdname fhir_design-methods
#' @aliases fhir_design,fhir_table_description-method
setMethod(
	f = "fhir_design",
	signature = c(... = "fhir_table_description"),
	definition = function(...) {
		args <- list(...)
		names <- paste0(sapply(args, function(x) {x@resource}), "s")
		name_index <- sapply(substitute(list(...))[-1], function(x) {class(x) == "name"})
		names[name_index] <- sapply(substitute(list(...))[-1], deparse)[name_index]
		names[names(args) != ""] <- names(args)[names(args) != ""]
		new(Class = "fhir_design", args, names = names)
	}
)

#' @rdname fhir_design-methods
#' @aliases fhir_design,list-method
setMethod(
	f = "fhir_design",
	signature = c(... = "list"),
	definition = function(...) {
		args <- list(...)
		if(length(args) == 1) {
			args <- unlist(args, recursive = FALSE)
			if(all(sapply(args, is, "fhir_table_description"))) {
				new(Class = "fhir_design", args, names  = attr(args, "names"))
			} else {
				message(
					"The old style design (simple named list) will be deprecated at some point. ",
					"Please consider building your design as shown in the documentation for fhir_design(), ",
					"see ?fhir_design."
				)
				d <- fix_design(design = args)
				df_desc <-lapply(
					d,
					function(x) {
						resource <- fhir_resource_type(string = gsub(paste0(esc("."),"|", esc("/")), "", x$resource))
						style <- fhir_style(sep = x$style$sep, brackets = x$style$brackets, rm_empty_cols = x$style$rm_empty_cols)
						fhir_table_description(resource = resource, cols = fhir_columns(x$cols), style = style)
					}
				)
				new(Class = "fhir_design", df_desc, names = attr(d, "names"))
			}
		} else {
			stop("You can only provide one list to fhir_design()")
		}
	}
)

setMethod(
	f = "show",
	signature = "fhir_design",
	definition = function(object){
		if(length(object) == 0) {
			cat("An empty fhir_design_object")
		} else {
			cat(paste0("A fhir_design with ", length(object), " table_descriptions:\n"))
			lapply(
				seq_len(length(object)),
				function(i) {
					df_desc <- object[[i]]
					cat("=====================================================\n")
					cat(paste0("Name: ", names(object)[i]))
					cat("\n\n")
					cat(paste0("Resource type: ", as.character(df_desc@resource), "\n\n"))
					cat("Columns: \n")
					show(df_desc@cols)
					cat("\n\nStyle: \n")
					show(df_desc@style)
					cat("\n")
				}
			)
		}
	}
)
