
testthat::test_that(
	"fhir_cast produces correct output",{
		expect_snapshot_value({
			bundles <- fhir_unserialize(example_bundles3)
			d <- fhir_crack(
				bundles,
				design = fhir_table_description(
					resource = "Patient",
					brackets = c("[", "]")
				),
				verbose = 0)
			fhir_cast(d,brackets = c("[", "]"), sep = ":::", verbose = 0)
		},
		style = "json2"
		)
	}
)


testthat::test_that(
	"fhir_cast and crack to wide are identical",{
		#real data
		bundles <- fhir_unserialize(patient_bundles)
		d <- fhir_crack(
			bundles,
			design = fhir_table_description(
				resource = "Patient",
				brackets = c("[", "]")
			),
			data.table = T,
			verbose = 0)

		d2 <- fhir_crack(
			bundles,
			design = fhir_table_description(
				resource = "Patient",
				brackets = c("[", "]"),
				format = "wide"
			),
			data.table = T,
			verbose = 0)

		d3 <- fhir_cast(d,brackets = c("[", "]"), sep = ":::", verbose = 0)

		data.table::setcolorder(d3, neworder = names(d2))
		expect_equal(d2, d3)

		#example data
		bundles <- fhir_unserialize(patient_bundles)
		d <- fhir_crack(
			bundles,
			design = fhir_table_description(
				resource = "Patient",
				brackets = c("[", "]")
			),
			data.table = T,
			verbose = 0)

		d2 <- fhir_crack(
			bundles,
			design = fhir_table_description(
				resource = "Patient",
				brackets = c("[", "]"),
				format = "wide"
			),
			data.table = T,
			verbose = 0)

		d3 <- fhir_cast(d,brackets = c("[", "]"), sep = ":::", verbose = 0)

		data.table::setcolorder(d3, neworder = names(d2))
		expect_equal(d2, d3)
	}
)
