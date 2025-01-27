testthat::test_that(
	"fhir_search downloads a valid bundle list", {

		testthat::skip_on_cran()

		bundles <- fhir_search( request = "https://server.fire.ly/Patient", max_bundles = 2, verbose = 0 )

		testthat::expect_s4_class(bundles, "fhir_bundle_list")
		testthat::expect_length(bundles,2)
	}
)

testthat::test_that(
	"fhir_search doesn't stop when stop_on_error = 0", {

		#build mock url
		url <- paste0("fhir.base.com", "/Patient")

		# Enable webmockr
		webmockr::enable()

		# mock a 401 response
		webmockr::stub_request("get", url) %>%
			webmockr::to_return(status = 401, body = '{"error": "Unauthorized"}')

		pat <- suppressWarnings(fhir_search(request = url,
						   delay_between_attempts = c(1,2),
						   stop_on_error = 0))


		# Disable webmockr when done
		webmockr::disable()

		testthat::expect_s4_class(pat, "fhir_bundle_list")
		testthat::expect_length(pat,0)
	}
)

testthat::test_that(
	"fhir_search stops on specified error", {

		#build mock url
		url <- paste0("fhir.base.com", "/Patient")

		# Enable webmockr
		webmockr::enable()

		# mock a 401 response
		webmockr::stub_request("get", url) %>%
			webmockr::to_return(status = 401, body = '{"error": "Unauthorized"}')

		expect_error(
				fhir_search(
					request = url,
					delay_between_attempts = 1,
					stop_on_error = 401)
				)

		# Disable webmockr when done
		webmockr::disable()
	}
)

testthat::test_that(
	"fhir_search stops with stop_on_error = 1", {

		#build mock url
		url <- paste0("fhir.base.com", "/Patient")

		expect_error(
			fhir_search(
				request = url,
				delay_between_attempts = 1,
				stop_on_error = 1)
		)

	}
)
