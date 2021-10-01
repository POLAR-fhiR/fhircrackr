
testthat::test_that(
	"fhir_style is constructed correctly", {

		style_default <- fhir_style()
		testthat::expect_s4_class(style_default, "fhir_style")
		testthat::expect_s4_class(style_default, "fhir_style")
		testthat::expect_equal(style_default@sep, ':::')
		testthat::expect_equal(style_default@brackets, character(0))
		testthat::expect_false(style_default@rm_empty_cols)

		style <- fhir_style(sep = "|", brackets = c("[", "]"), rm_empty_cols = T)
		testthat::expect_s4_class(style, "fhir_style")
	}
)


testthat::test_that(
	"fhir_style throws error for invalid input", {

		testthat::expect_error(fhir_style(sep = 3))
		testthat::expect_error(fhir_style(brackets = c("[", NA)))
		testthat::expect_error(fhir_style(brackets = c(NA, "]")))
		testthat::expect_error(fhir_style(rm_empty_cols = 2))
	}
)
