
testthat::test_that(
	"fhir_bundle_xml is built correctly", {

		pat <- xml2::xml_unserialize(patient_bundles[[1]])
		b <- fhir_bundle_xml(pat)

		testthat::expect_s4_class(b, "fhir_bundle_xml")
		testthat::expect_s4_class(b, "xml_document")
		testthat::expect_s4_class(b, "xml_node")
	}
)

testthat::test_that(
	"fhir_bundle_xml is built correctly", {

		b <- fhir_bundle_serialized(patient_bundles[[1]])

		testthat::expect_s4_class(b, "fhir_bundle_serialized")
		testthat::expect_s4_class(b, "raw")

	}
)
