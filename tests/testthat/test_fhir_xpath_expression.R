
testthat::test_that(
	"fhir_xpath_expression produces valid objects", {
		e1 <- fhir_xpath_expression("//Patient")
		e2 <- fhir_xpath_expression("name/given")
		testthat::expect_s4_class(e1, "fhir_xpath_expression")
		testthat::expect_s4_class(e1, "fhir_xpath_expression")
	}
)

testthat::test_that(
	"fhir_xpath_expression throws error for invalid expressions", {
		testthat::expect_error(fhir_xpath_expression("\\Patient"))
	}
)

testthat::test_that(
	"fhir_xpath_expression throws error for invalid types", {
		testthat::expect_error(fhir_xpath_expression(3))
	}
)
