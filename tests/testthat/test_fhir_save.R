

testthat::test_that(
	"fhir_save and fhir_load work", {
		directory <- paste0(tempdir(), "/bundles")

		fhir_save( fhir_unserialize(patient_bundles), directory)
		testthat::expect_length(dir(directory),2)

		myBundles <- fhir_load(directory =  directory )
		names(myBundles) <- NULL
		expect_equal(myBundles, fhir_unserialize(patient_bundles))

	}
)


