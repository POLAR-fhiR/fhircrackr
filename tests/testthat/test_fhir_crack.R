
testthat::test_that(
	"fhir_crack compact with filtered values and automatic column names produces correct output",{
		expect_snapshot_value({
			b <- fhir_unserialize(bundles = example_bundles3)
			fhir_crack(b, fhir_table_description(
				resource = "Patient",
				cols = c(
					"address[use[@value='home']]/city",
					"address[use[@value='work']]/city",
					"address/use",
					"address/country"
				)
			), verbose = 0)
		},
		style = "json2"
		)
	}
)

testthat::test_that(
	"fhir_crack compact with filtered values and given column names produces correct output",{
		expect_snapshot_value({
			b <- fhir_unserialize(bundles = example_bundles3)
			fhir_crack(b, fhir_table_description(
				resource = "Patient",
				cols = c(
					home_city = "address[use[@value='home']]/city",
					work_city = "address[use[@value='work']]/city",
					 use = "address/use",
					country = "address/country"
				)
			), verbose = 0)
		},
		style = "json2"
		)
	}
)

testthat::test_that(
	"fhir_crack compact with similar colnames produces correct output",{
		expect_snapshot_value({
			b <- fhir_unserialize(bundles = example_bundles3)
			fhir_crack(b, fhir_table_description(
				resource = "Patient",
				cols = c(
					x = "id",
					x1 = "address/city",
					x12 = "address/use"
				)
			), verbose = 0)
		},
		style = "json2"
		)
	}
)

testthat::test_that(
	"fhir_crack compact with filtered values and brackets produces correct output",{
		expect_snapshot_value({
			b <- fhir_unserialize(bundles = example_bundles3)
			fhir_crack(b, fhir_table_description(
				resource = "Patient",
				cols = c(
					"address[use[@value='home']]/city",
					"address[use[@value='work']]/city",
					"address/use",
					 "address/country"
				),
				brackets = c("[", "]")
			), verbose = 0)
		},
		style = "json2"
		)
	}
)

testthat::test_that(
	"fhir_crack wide with filtered values and brackets produces correct output",{
		expect_snapshot_value({
			b <- fhir_unserialize(bundles = example_bundles3)
			fhir_crack(
				b,
				fhir_table_description(
					resource = "Patient",
					cols = c(
						"address[use[@value='home']]/city",
						"address[use[@value='work']]/city",
						"address/use",
						"address/country"
					)
				),
				brackets = c("[", "]"),
				format = "wide", verbose = 0
			)
		},
		style = "json2"
		)
	}
)

testthat::test_that(
	"fhir_crack wide with similar colnames produces correct output",{
		expect_snapshot_value({
			b <- fhir_unserialize(bundles = example_bundles3)
			fhir_crack(b, fhir_table_description(
				resource = "Patient",
				cols = c(
					x = "id",
					x1 = "address/city",
					x12 = "address/use"
				),
				brackets = c("[", "]"),
				format = "wide"
			), verbose = 0)
		},
		style = "json2"
		)
	}
)


testthat::test_that(
	"fhir_crack compact with filtered values and keep_attr produces correct output",{
		expect_snapshot_value({
			b <- fhir_unserialize(bundles = example_bundles3)
			fhir_crack(b, fhir_table_description(
				resource = "Patient",
				cols = c(
					"address[use[@value='home']]/city",
					"address[use[@value='work']]/city",
					"address/use",
					"address/country"
				),
				brackets = c("[", "]"),
				keep_attr = TRUE
			), verbose = 0)
		},
		style = "json2"
		)
	}
)


testthat::test_that(
	"fhir_crack compact with filtered values produces correct output",{
		expect_snapshot_value({
			b <- fhir_unserialize(bundles = example_bundles3)
			fhir_crack(b, fhir_table_description(
				resource = "Patient",
				cols = c(
					"address[use[@value='home']]/city",
					"address[use[@value='work']]/city",
					"address/use",
					"address/country"
				)
			), verbose = 0)
		},
		style = "json2"
		)
	}
)

testthat::test_that(
	"fhir_crack compact all columns produces correct output",{
		expect_snapshot_value({
			bundles <- fhir_unserialize(example_bundles3)
			fhir_crack(
				bundles,
				design = fhir_table_description(
					resource = "Patient"
				),
				verbose = 0)
		},
		style = "json2"
		)
	}
)


testthat::test_that(
	"fhir_crack compact given columns produces correct output",{
		expect_snapshot_value({
			bundles <- fhir_unserialize(example_bundles3)
			fhir_crack(
				bundles,
				design = fhir_table_description(
					resource = "Patient",
					cols = c("id", "name/given", "address/city")
				),
				verbose = 0)
		},
		style = "json2"
		)
	}
)

testthat::test_that(
	"fhir_crack wide given columns produces correct output",{
		expect_snapshot_value({
			bundles <- fhir_unserialize(example_bundles3)
			fhir_crack(
				bundles,
				design = fhir_table_description(
					resource = "Patient",
					cols = c("id", "name/given", "address/city"),
					format = "wide",
					brackets = c("[", "]")
				),
				verbose = 0)
		},
		style = "json2"
		)
	}
)

testthat::test_that(
	"fhir_crack wide all columns produces correct output",{
		expect_snapshot_value({
			bundles <- fhir_unserialize(example_bundles3)
			fhir_crack(
				bundles,
				design = fhir_table_description(
					resource = "Patient",
					format = "wide",
					brackets = c("[", "]")
				),
				verbose = 0)
		},
		style = "json2"
		)
	}
)

testthat::test_that(
	"fhir_crack produces correct output with two tables",{
		expect_snapshot_value({
			bundles <- fhir_unserialize(example_bundles3)
			fhir_crack(
				bundles,
				design = fhir_design(
					fhir_table_description(
						resource = "Patient",
						format = "wide",
						brackets = c("[", "]")
					),
					fhir_table_description(
						resource = "Observation"
					)
				),
				verbose = 0)
		},
		style = "json2"
		)
	}
)

testthat::test_that(
	"fhir_crack()  with data.table=TRUE returns data.tables", {

		bundles <- fhir_unserialize(medication_bundles)

		t1 <- fhir_crack(
			bundles,
			design = fhir_table_description(
				resource = "Patient"
			),
			verbose = 0,
			data.table = T)

		t2 <- fhir_crack(
			bundles,
			design = fhir_table_description(
				resource = "Patient",
				cols = c("id", "gender")
			),
			verbose = 0,
			data.table = T)

		t3 <- fhir_crack(
			bundles,
			design = fhir_design(
				pat = 	fhir_table_description(
					resource = "Patient",
					cols = c("id", "gender")
				),
				med = 	fhir_table_description(
					resource = "MedicationStatement"
				)
			)
			,
			verbose = 0,
			data.table = T)

		testthat::expect_s3_class(t1, "data.table")
		testthat::expect_s3_class(t2, "data.table")
		testthat::expect_s3_class(t3$pat, "data.table")
		testthat::expect_s3_class(t3$med, "data.table")
	}
)