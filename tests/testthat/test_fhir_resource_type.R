testthat::test_that(
	"fhir_resource_type creates object correctly", {

		pat <- fhir_resource_type("patient")

		testthat::expect_s4_class(object = pat, class = "fhir_resource_type")
		testthat::expect_true(pat %in% existing_resource_types)

	}
)

testthat::test_that(
	"fhir_resource_type throws error when more than one resource is provided", {

		testthat::expect_error(fhir_resource_type(c("patient", "medication")))


	}
)
