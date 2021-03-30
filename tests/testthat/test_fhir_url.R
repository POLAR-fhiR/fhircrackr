
testthat::test_that(
	"fhir_search_url is built correctly", {
		u1 <- fhir_search_url(
			base = "http://hapi.fhir.org/baser4",
		    resource = "Patient")

		u2 <- fhir_search_url(
			base = "http://hapi.fhir.org/baser4",
			resource = "Patient",
			parameters = list(c("gender", "male"), c("_summary", "count")))

		testthat::expect_s4_class(u1, "fhir_search_url")
		testthat::expect_s4_class(u2, "fhir_search_url")
	}
)

testthat::test_that(
	"different methods create identical objects", {

		u2 <- fhir_search_url(
			base = "http://hapi.fhir.org/baser4",
			resource = "Patient",
			parameters = list(c("gender", "male"), c("_summary", "count")))

		u3 <- fhir_search_url(
			base = "http://hapi.fhir.org/baser4",
			resource = "Patient",
			parameters = fhir_parameters(fhir_key_value_pair("gender", "male"),
										 fhir_key_value_pair("_summary", "count")))

		u4 <- fhir_search_url(
			base = "http://hapi.fhir.org/baser4",
			resource = "Patient",
			parameters = "gender=male&_summary=count")

		testthat::expect_identical(u2,u3)
		testthat::expect_identical(u3,u4)

	}
)