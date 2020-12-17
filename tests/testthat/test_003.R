testthat::test_that(
	"fhir_build_request builds a proper url", {

		testthat::skip_on_cran()

		r <- fhir_build_request(fhir_base("https://hapi.fhir.org/baseR4"),
						   fhir_resource("patient"),
						   fhir_key_value("gender", "female"),
						   fhir_key_value("_count", "10"))

		bundles <- fhir_search(r, max_bundles = 1)

		testthat::expect_false(is.null(bundles))

	}
)

testthat::test_that(
	"fhir_build_request works with dissect_request", {

		l <- fhircrackr:::dissect_request("https://hapi.fhir.org/baseR4/Patient?gender=female&_count=10")

		r <- fhir_build_request(l)

		testthat::expect_length(l,4)
		testthat::expect_identical(r, "https://hapi.fhir.org/baseR4/Patient?gender=female&_count=10")

	}
)

testthat::test_that(
	"fhir_current_request() and fhir_update_request() work", {

		r <- fhir_build_request(fhir_base("https://hapi.fhir.org/baseR4"),
								fhir_resource("patient"),
								fhir_key_value("gender", "female"),
								fhir_key_value("_count", "10"))

		testthat::expect_identical(r, fhir_current_request())

		fhir_update_request(fhir_key_value("gender", "male"), append = F, return_request = F)

		testthat::expect_identical(fhir_current_request(), "https://hapi.fhir.org/baseR4/Patient?gender=male")

		fhir_update_request(fhir_key_value("_count", "10"), append = T, return_request = F)

		testthat::expect_identical(fhir_current_request(),"https://hapi.fhir.org/baseR4/Patient?gender=male&_count=10")

	}
)

testthat::test_that(
	"fhir_search works when no request provided",{

		r <- fhir_build_request(fhir_base("https://hapi.fhir.org/baseR4"),
								fhir_resource("patient"),
								fhir_key_value("gender", "female"),
								fhir_key_value("_count", "10"))

		fhir_search(max_bundles = 1)

	}

)
