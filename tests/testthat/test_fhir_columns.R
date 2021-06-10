
testthat::test_that(
	"methods for fhir_columns() create identical results", {
		c1 <- fhir_columns(xpaths = c(code="code/coding/code", id = "id"))
		c2 <- fhir_columns(xpaths = c("code/coding/code", "id"), colnames = c("code", "id"))
		c3 <- fhir_columns(xpaths = list(code="code/coding/code", id = "id"))

		testthat::expect_identical(c1, c2)
		testthat::expect_identical(c3, c2)
	}
)

testthat::test_that(
	"names() works on fhir_columns", {
		c <- fhir_columns(xpaths = c(code="code/coding/code", id = "id"))

		testthat::expect_identical(names(c), c("code", "id"))

	}
)

testthat::test_that(
	"errors are thrown for incorrect input", {
		testthat::expect_error(fhir_columns(xpaths = list(c("a", "b"), c("a"))))
		testthat::expect_error(fhir_columns(xpaths = list(c(1, 2))))
	}
)
