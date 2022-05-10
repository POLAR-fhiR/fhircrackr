
testthat::test_that(
	"fhir_melt produces correct output",{
		expect_snapshot_value({
			bundles <- fhir_unserialize(example_bundles3)
			d <- fhir_crack(
				bundles,
				design = fhir_table_description(
					resource = "Patient",
					brackets = c("[", "]")
				),
				verbose = 0)
			fhir_melt(d, columns =  fhir_common_columns(d, "address"),brackets = c("[", "]"), sep = ":::")
		},
		style = "json2"
		)
	}
)
