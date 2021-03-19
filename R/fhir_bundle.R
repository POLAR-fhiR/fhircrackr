#Class definition
setClass("fhir_bundle",
		 contains = "VIRTUAL",
		 slots=c(bundle = "ANY"))

#no constructor/no validity check for this virtual class