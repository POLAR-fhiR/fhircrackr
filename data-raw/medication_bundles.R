## code to prepare `medication_bundles` dataset goes here
search_request  <- paste0(
	"https://hapi.fhir.org/baseR4/", #server endpoint
	"MedicationStatement?", #look for MedicationsStatements
	"code=http://snomed.info/ct|429374003", #only choose resources with this loinc code
	"&_include=MedicationStatement:subject") #include the corresponding Patient resources

bundles <- fhir_search(search_request, max.bundles = 3)

medication_bundles<-lapply(bundles, xml2::xml_serialize, connection=NULL)

usethis::use_data(medication_bundles, overwrite = TRUE)
