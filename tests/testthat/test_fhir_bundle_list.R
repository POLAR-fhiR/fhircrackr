
testthat::test_that(
	"fhir_bundle_list is built correctly", {
		b1 <- xml2::read_xml("<Bundle><Resource><item value='1'/></Resource></Bundle>")
		b2 <- xml2::read_xml("<Bundle><Resource><item value='2'/></Resource></Bundle>")

		bl <- fhir_bundle_list(list(b1, b2))
		testthat::expect_s4_class(bl, "fhir_bundle_list")
	}
)

testthat::test_that(
	"fhir_bundle_list() returns identical results for different input", {
		b1 <- xml2::read_xml("<Bundle><Resource><item value='1'/></Resource></Bundle>")
		b2 <- xml2::read_xml("<Bundle><Resource><item value='2'/></Resource></Bundle>")

		bl1 <- fhir_bundle_list(list(b1,fhir_bundle_xml(b2)))
		bl2 <- fhir_bundle_list(list(fhir_bundle_xml(b1), b2))

		r1 <- xml2::xml_serialize(b1, connection= NULL)
		r2 <- xml2::xml_serialize(b2, connection= NULL)

		bl3 <- fhir_bundle_list(list(r1, r2))
		bl4 <- fhir_bundle_list(list(fhir_bundle_serialized(r1), fhir_bundle_serialized(r2)))

		testthat::expect_identical(bl1, bl2)
		testthat::expect_identical(bl3, bl4)

	}
)

testthat::test_that(
	"fhir_bundle_list() throws error for incorrect input", {
		testthat::expect_error(fhir_bundle_list(c(1,2)))
		testthat::expect_error(fhir_bundle_list(list(1,2)))

	}
)
