
testthat::test_that(
	"fhir_resource_type creates object correctly", {

		testthat::expect_s4_class(object = fhir_resource_type(string = "Patient"), class = "fhir_resource_type")
		testthat::expect_true(fhir_resource_type(string = "Patient") %in% existing_resource_types)
	}
)

testthat::test_that(
	"fhir_resource_type throws error when more than one resource is provided", {

		testthat::expect_error(fhir_resource_type(string = c("Patient", "Medication")))
	}
)
