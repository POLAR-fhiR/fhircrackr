
testthat::test_that(
	"fhir_xpath_expression produces valid objects", {

		testthat::expect_s4_class(fhir_xpath_expression("Bundle"), "fhir_xpath_expression")
		testthat::expect_s4_class(fhir_xpath_expression(c("Bundle", "./Resource")), "fhir_xpath_expression")
		testthat::expect_s4_class(fhir_xpath_expression("./item/*"), "fhir_xpath_expression")
		testthat::expect_s4_class(fhir_xpath_expression(".//item/*[@value='1']"), "fhir_xpath_expression")
	}
)

testthat::test_that(
	"fhir_xpath_expression throws error for invalid expressions", {
		testthat::expect_error(fhir_xpath_expression())
		testthat::expect_error(fhir_xpath_expression("\\Patient"))
		testthat::expect_error(fhir_xpath_expression("Patient@value"))
	}
)

testthat::test_that(
	"fhir_xpath_expression throws error for invalid types", {
		testthat::expect_error(fhir_xpath_expression(3))
	}
)
