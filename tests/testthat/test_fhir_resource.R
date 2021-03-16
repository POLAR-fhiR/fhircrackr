testthat::test_that(
	"fhir_resource creates object correctly", {

		pat <- fhir_resource("patient")

		testthat::expect_s4_class(object = pat, class = "fhir_resource")
		testthat::expect_true(pat %in% existing_resource_types)

	}
)

testthat::test_that(
	"fhir_resource throws error when more than one resource is provided", {

		testthat::expect_error(fhir_resource(c("patient", "medication")))


	}
)
