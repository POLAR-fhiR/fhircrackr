testthat::test_that(
	"fhir_collapse works on address.line",{
		expect_snapshot_value({
		bundles <- fhir_unserialize(example_bundles6)
		d <- fhir_crack(bundles,
						design = fhir_table_description(
							resource = "Patient",
							brackets = brackets,
							sep = sep),
						verbose = 0
		)
		fhir_collapse(d, columns = "address.line", sep = "|", brackets = c("[", "]"))
		}, style = "json2")

	})

testthat::test_that(
	"fhir_collapse works on name.given",{
		testthat::expect_snapshot_value({
		bundles <- fhir_unserialize(example_bundles7)
		d <- fhir_crack(bundles,
						design = fhir_table_description(
							resource = "Patient",
							brackets = c("[", "]"),
							sep = "|"),
						verbose = 0
		)
		fhir_collapse(d, columns = "name.given", sep = "|", brackets = c("[", "]"))
		}, style = "json2")

	})