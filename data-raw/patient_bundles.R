## code to prepare `patient_bundles` dataset goes here

bundles <- fhir_search(request="https://server.fire.ly/Patient", max_bundles=2, verbose = 0)

patient_bundles<-fhir_serialize(bundles)

usethis::use_data(patient_bundles, overwrite = TRUE)
