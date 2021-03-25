
testthat::test_that(
	"fhir_key_value_pair is created correctly", {
		kv <- fhir_key_value_pair(key = "gender", value = "male")
		testthat::expect_s4_class(kv, "fhir_key_value_pair")
	}
)
