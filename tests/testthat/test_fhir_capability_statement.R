testthat::test_that(
	"fhir_capability_statement() works", {

		testthat::skip_on_cran()

		caps <- fhir_capability_statement(url = "https://server.fire.ly", sep = " ~ ")

		testthat::expect_s3_class(caps[[1]], "data.frame")
		testthat::expect_s3_class(caps[[2]], "data.frame")
		testthat::expect_s3_class(caps[[3]], "data.frame")
	}
)


