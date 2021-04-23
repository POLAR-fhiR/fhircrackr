
testthat::test_that(
	"methods for fhir_body() create identical results", {
		b1 <- fhir_body(content = "gender=female&_summary=count", type="application/x-www-form-urlencoded")
		b2 <- fhir_body(content = list("gender"="female", "_summary"= "count"))
		testthat::expect_identical(b1, b2)
	}
)

testthat::test_that(
	"errors are thrown for incorrect input", {
		testthat::expect_error(fhir_body(content = "a"))
		testthat::expect_error(fhir_body(content = c("a", "b"), type="a"))
	}
)
