#Class definition
setClass("fhir_style",
		 slots = c(sep = "character",
		 		  brackets = "character",
		 		  rm_empty_cols = "logical"))

#Validity check
setValidity("fhir_style",
			method = function(object){
				messages <- c()

				if(length(object@sep) > 1){
					messages <- c(messages, "sep must be character of length 1")
				}
				if(!length(object@brackets) %in% c(0,2)){
					messages <- c(messages, "brackets must be character of length 2 or empty")
				}
				if(length(object@rm_empty_cols) > 1){
					messages <- c(messages, "remove_empty_columns must be logical of length 1")
				}

				if(length(messages)>0){messages}else{TRUE}
			}
)

#Constructor

#' Create fhir_style object
#'
#' This function creates an object of class `fhir_style`. It contains the three elements `sep`, `brackets` and `rm_empty_cols`

#' @param sep A string to separate pasted multiple entries. Defaults to `" "`
#' @param brackets  A character vector of length two defining the brackets surrounding indices for multiple entries, e.g. `c( "<", ">")`.
#' If this is empty (i.e. character of length 0), no indices will be added to multiple entries. Defaults to empty, meaning no indices.
#' @param rm_empty_cols Logical scalar. Remove empty columns? Defaults to `TRUE`.
#'
#' @export
#' @examples
#' fhir_style(sep = " ", brackets = c("[", "]"), rm_empty_cols = FALSE)


fhir_style <- function(sep=" ", brackets=character(), rm_empty_cols=TRUE){
 new("fhir_style", sep=sep, brackets=brackets, rm_empty_cols=rm_empty_cols)
}


#methods
setMethod("show", signature = "fhir_style",
		  function(object){
		  	sep <- ifelse(object@sep==" ", paste0("'", object@sep, "'"), object@sep)

		  	brackets <- ifelse(length(object@brackets)==0,
		  					   "character(0)",
		  					   paste0("'", object@brackets[1], "' '", object@brackets[2], "'"))

		  	cat(paste0(
		  		"fhir_style object with the following elements:\n\n",
		  		"sep: ", sep, "\n",
		  		"brackets: ", brackets, "\n",
		  		"rm_empty_cols: ", object@rm_empty_cols))

		  })
