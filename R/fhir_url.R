#Class definition
setClass(
	"fhir_url",
	contains = "VIRTUAL",
	slots = c(base = "character")
)

#Validity check
setValidity(
	"fhir_url",
	method = function(object){
		messages <- c()
		if(1 < length(object@base)) {
			messages <- c(messages, paste0("The base for a fhir_url has to be a character of length 1."))
		}
		if(0 < length(messages)) {messages} else {TRUE}
	}
)

#no constructor for virtual class
