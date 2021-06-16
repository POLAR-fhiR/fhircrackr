## code to prepare `medication_bundles` dataset goes here
search_request  <- fhir_url("https://hapi.fhir.org/baseR4",
							resource = "MedicationStatement",
							parameters = c("code" = "http://snomed.info/ct|429374003",
										   "_include" = "MedicationStatement:subject"))

bundles <- fhir_search(search_request, max_bundles = 3)

medication_bundles<-fhir_serialize(bundles)

usethis::use_data(medication_bundles, overwrite = TRUE)
