testthat::test_that(
	"fhir_search downloads a valid bundle list", {

		testthat::skip_on_cran()

		bundles <- fhir_search( request = "https://server.fire.ly/Patient", max_bundles = 2, verbose = 0 )

		testthat::expect_s4_class(bundles, "fhir_bundle_list")
		testthat::expect_length(bundles,2)
	}
)
