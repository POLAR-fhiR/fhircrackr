
testthat::test_that(
	"fhir_url is built correctly", {
		u1 <- fhir_url(
			url = "http://hapi.fhir.org/baseR4",
		    resource = "Patient")

		u2 <- fhir_url(
			url = "http://hapi.fhir.org/baseR4",
			resource = "Patient",
			parameters = list("gender"= "male", "_summary" = "count"))

		testthat::expect_s4_class(u1, "fhir_url")
		testthat::expect_s4_class(u2, "fhir_url")
	}
)

testthat::test_that(
	"fhir_url saves request as current request", {
		u1 <- fhir_url(
			url = "http://hapi.fhir.org/baser4",
			resource = "Patient")

		testthat::expect_identical(u1, fhircrackr_env$current_request)
	}
)


testthat::test_that(
	"different methods create identical objects", {

		u1 <- fhir_url(
			url = "http://hapi.fhir.org/baser4",
			resource = "Patient",
			parameters =list("gender"= "male", "_summary" = "count"))


		u2 <- fhir_url(
			url = "http://hapi.fhir.org/baser4",
			resource = "Patient",
			parameters = "gender=male&_summary=count")

		testthat::expect_identical(u1,u2)


	}
)
