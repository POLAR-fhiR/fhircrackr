#donwload empty bundle
request <- fhir_url(url = "https://server.fire.ly",
					resource = "Patient",
					parameters = c(gender="bla"))

#download bundles
bundles <- fhir_search(request, max_bundles = 1)

example_bundles_empty <- fhir_serialize(bundles)

usethis::use_data(example_bundles_empty, overwrite = TRUE)

