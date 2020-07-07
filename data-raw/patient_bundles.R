## code to prepare `patient_bundles` dataset goes here

bundles <- fhir_search(request="http://fhir.hl7.de:8080/baseDstu3/Patient?", max_bundles=2, verbose = 0)

patient_bundles<-fhir_serialize(bundles)

usethis::use_data(patient_bundles, overwrite = TRUE)
