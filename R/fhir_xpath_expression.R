
#' A

#definition
setClass("fhir_xpath_expression",
		 contains = "character")

#validity check
setValidity("fhir_xpath_expression",
			method = function(object){
				messages <- c()

				if(length(object)>1){
					messages <- c(messages, "A fhir_xpath_expression hs to be of lenght 1.")
				}

				#slightly hacky solution: use xml2 function and catch warning message
				#this will validate xpath expression with libxml2 (accessed by xml2)
				testbundle <- xml2::read_xml("<Bundle>   </Bundle>")

				tryCatch(
					xml2::xml_find_all(testbundle, object),
					warning = function(x) {
						if (grepl("Invalid expression", x))
							messages <<- c(messages, paste(esc(object),
								"is not a valid XPath expression."))
					}
				)

				if(length(messages)>0){messages}else{TRUE}
			})

#' Create fhir_xpath_expression
#'
#' This function takes a string, checks whether it is a valid XPath expression
#' and returns it as an fhir_xpath_expression object
#'
#' @param expression A string with the XPath expression
#' @return A XPath expression object
#' @examples
#' fhir_xpath_expression("//Patient")
#'
#' fhir_xpath_expression("name/given")
#'
fhir_xpath_expression <- function(expression){
	new("fhir_xpath_expression", expression)
}