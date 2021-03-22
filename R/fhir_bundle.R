#Class definition
setClass("fhir_bundle",
		 contains = "VIRTUAL")


#' An S4 class to represent a FHIR bundle in xml form
#'
setClass("fhir_bundle_xml",
		 contains = c("fhir_bundle", "xml_document", "xml_node"))

setValidity("fhir_bundle_xml",
			function(object){
				messages <- c()

				if(xml2::xml_name(object)!="Bundle"){
					messages <- c(messages, "This xml doesn't seem to represent a bundle, its name is not 'Bundle'.")
				}

			if(length(messages)>0){messages}else{TRUE}
			})



setMethod("show", "fhir_bundle_xml",
		  function(object){
		  	cat(paste0("A fhir_bundle_xml object with ", length(xml2::xml_find_all(object, "entry")), " entries:\n\n"))
		  	print(object)
		  })

#' An S4 class to represent a FHIR bundle in serialized form

setClass("fhir_bundle_serialized",
			contains = c("fhir_bundle", "raw"))
