
testthat::test_that(
	"fhir_bundle_xml is built correctly", {
		pat <- xml2::xml_unserialize(patient_bundles[[1]])
		b <- fhir_bundle_xml(pat)
		testthat::expect_s4_class(b, "fhir_bundle_xml")
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

testthat::test_that(
	"fhir_bundle_xml throws error for incorrect input", {
		testthat::expect_error(fhir_bundle_xml(patient_bundles[[1]]))
		testthat::expect_error(fhir_bundle_xml(xml2::read_xml("<foo><bar /></foo>")))
	}
)

testthat::test_that(
	"fhir_bundle_serialized throws error for incorrect input", {
		pat <- xml2::xml_unserialize(patient_bundles[[1]])
		testthat::expect_error(fhir_bundle_serialized(pat))
	}
)

testthat::test_that(
	"fhir_serialize and fhir_unserialize work on fhir_bundle",{
		b <- fhir_bundle_xml(xml2::read_xml("<Bundle> <link>  <relation value=\"self\"/> <url value=\"selflink\"/> </link>
											<link><relation value=\"next\"/> <url value=\"nextlink\"/> </link></Bundle>"))
		b_serialized <- fhir_serialize(b)
		b_unserialized <- fhir_unserialize(b_serialized)
		testthat::expect_s4_class(b_serialized, "fhir_bundle_serialized")
		testthat::expect_s4_class(b_unserialized, "fhir_bundle_xml")
	}

)
