
testthat::test_that(
	"methods for fhir_parameters() create identical results", {

		p1 <- fhir_parameters(paramstring = "gender=male&birthdate=le2000-01-01&_summary=count")
		p2 <- fhir_parameters(keys = c("gender", "birthdate", "_summary"),
							  values = c("male", "le2000-01-01", "count"))
		p3 <- fhir_parameters(paramlist = list(c("gender", "male"),
											   c("birthdate", "le2000-01-01"),
											   c("_summary", "count")))

		testthat::expect_identical(p1,p2)
		testthat::expect_identical(p2,p3)
	}
)

testthat::test_that(
	"errors are thrown for incorrect input", {

		testthat::expect_error(fhir_parameters(keys = c("a", "b"), values = c("d")))
		testthat::expect_error(fhir_parameters(paramlist = list(c("a", "b"), c("a"))))
		testthat::expect_error(fhir_parameters(paramlist = list(c(1, 2))))

	}
)
