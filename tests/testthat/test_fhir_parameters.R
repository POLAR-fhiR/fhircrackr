
testthat::test_that(
	"methods for fhir_parameters() create identical results", {

		p1 <- fhir_parameters("gender=male&birthdate=le2000-01-01&_summary=count")

		p2 <- fhir_parameters(
			list(
				c("gender", "male"),
				c("birthdate", "le2000-01-01"),
				c("_summary", "count")
				)
			)

		p3 <- fhir_parameters(
			new("fhir_key_value_pair", key="gender", value="male"),
		    new("fhir_key_value_pair", key="birthdate", value="le2000-01-01"),
		    new("fhir_key_value_pair", key="_summary", value="count"))

		testthat::expect_identical(p1,p2)
		testthat::expect_identical(p3,p2)
	}
)

testthat::test_that(
	"errors are thrown for incorrect input", {

		testthat::expect_error(fhir_parameters(list(c("a", "b"), c("a"))))
		testthat::expect_error(fhir_parameters(list(c(1, 2))))

	}
)
