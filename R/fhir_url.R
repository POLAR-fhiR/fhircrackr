#Class definition
setClass("fhir_url",
		 contains = "VIRTUAL",
		 slots=c(base = "character"))

#Validity check
setValidity("fhir_url",
			method = function(object){
				messages <- c()

				if(length(object@base) > 1 ){
					messages <- c(messages, paste0("The base for a fhir_url has to be a character of length 1."))
				}

				if(length(messages)>0){messages}else{TRUE}

			}
)

#no constructor for virtual class
