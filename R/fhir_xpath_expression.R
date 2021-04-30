#' An S4 class for xpath_expressions
#' Objects of this class are essentially character vectors, but can only be valid
#' XPath (1.0) expressions.
#' They are mostly used in the `fhir_columns` class.
#' @export

#definition
setClass(
	"fhir_xpath_expression",
	contains = "character"
)

#validity check
setValidity(
	"fhir_xpath_expression",
	method = function(object) {
		if(length(object)==0){return(TRUE)}

		messages <- c()

		#slightly hacky solution: use xml2 function and catch warning message
		#this will validate xpath expression with libxml2 (accessed by xml2)
		testbundle <- xml2::read_xml("<Bundle><Resource><item value='1'/></Resource></Bundle>")
		for(i in 1:length(object)){
			tryCatch(
				xml2::xml_find_all(testbundle, object[i]),
				warning = function(x) {
					if (grepl("Invalid expression", x)) {
						messages <<- c(messages, paste(esc(object[i]),"is not a valid XPath expression."))
					}
				}
			)
		}
		if(0 < length(messages)) {messages} else {TRUE}
	}
)

#' Create fhir_xpath_expression
#'
#' This function takes a character vector, checks whether it contains valid XPath (1.0) expressions
#' and returns it as an fhir_xpath_expression object. These objects are used in `fhir_parameters` objects.
#'
#' @param expression A character vector of the XPath expressions
#' @return A XPath expression object
#' @examples
#' fhir_xpath_expression(c("//Patient", "name/given"))
#'
#' @export
#'
fhir_xpath_expression <- function(expression){
	new("fhir_xpath_expression", expression)
}