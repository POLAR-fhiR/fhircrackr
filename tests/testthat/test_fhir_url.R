
testthat::test_that(
	"fhir_search_url is built correctly", {
		u1 <- fhir_search_url(
			base = "http://hapi.fhir.org/baser4",
		    resource = "Patient")

		u2 <- fhir_search_url(
			base = "http://hapi.fhir.org/baser4",
			resource = "Patient",
			parameters = list("gender"= "male", "_summary" = "count"))

		testthat::expect_s4_class(u1, "fhir_search_url")
		testthat::expect_s4_class(u2, "fhir_search_url")
	}
)

testthat::test_that(
	"different methods create identical objects", {

		u1 <- fhir_search_url(
			base = "http://hapi.fhir.org/baser4",
			resource = "Patient",
			parameters =list("gender"= "male", "_summary" = "count"))


		u2 <- fhir_search_url(
			base = "http://hapi.fhir.org/baser4",
			resource = "Patient",
			parameters = "gender=male&_summary=count")

		testthat::expect_identical(u1,u2)


	}
)
