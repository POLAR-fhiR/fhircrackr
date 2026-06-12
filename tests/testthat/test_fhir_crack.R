
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
	"fhir_crack given columns handles bundles with a single unindexed entry", {
		bundle <- xml2::read_xml(
			"<Bundle>
				<type value='searchset'/>
				<entry>
					<resource>
						<Patient>
							<id value='id1'/>
							<gender value='female'/>
							<address>
								<city value='Amsterdam'/>
							</address>
							<address>
								<city value='Rome'/>
							</address>
						</Patient>
					</resource>
				</entry>
			</Bundle>"
		)
		bundle_list <- fhir_bundle_list(list(fhir_bundle_xml(bundle)))
		cols <- c(id = "id", gender = "gender", city = "address/city")

		compact <- fhir_crack(
			bundles = bundle_list,
			design = fhir_table_description("Patient", cols = cols),
			verbose = 0,
			data.table = TRUE
		)
		testthat::expect_equal(compact$id, "id1")
		testthat::expect_equal(compact$gender, "female")
		testthat::expect_equal(compact$city, "Amsterdam:::Rome")

		wide <- fhir_crack(
			bundles = bundle_list,
			design = fhir_table_description(
				"Patient",
				cols = cols,
				format = "wide",
				brackets = c("[", "]")
			),
			verbose = 0,
			data.table = TRUE
		)
		testthat::expect_equal(wide$`[1]id`, "id1")
		testthat::expect_equal(wide$`[1]gender`, "female")
		testthat::expect_equal(wide$`[1.1]city`, "Amsterdam")
		testthat::expect_equal(wide$`[2.1]city`, "Rome")
	}
)

testthat::test_that(
	"fhir_crack preserves nodes selected by overlapping columns", {
		bundles <- fhir_unserialize(example_bundles3)
		cols <- c(
			any_city = "address/city",
			home_city = "address[use[@value='home']]/city"
		)

		compact <- fhir_crack(
			bundles = bundles,
			design = fhir_table_description(resource = "Patient", cols = cols),
			verbose = 0,
			data.table = TRUE
		)
		testthat::expect_equal(
			compact$home_city,
			c("Amsterdam", "Rome", "Berlin")
		)
		testthat::expect_equal(
			compact$any_city,
			c("Amsterdam", "Rome:::Stockholm", "Berlin:::London")
		)

		wide <- fhir_crack(
			bundles = bundles,
			design = fhir_table_description(
				resource = "Patient",
				cols = cols,
				format = "wide",
				brackets = c("[", "]")
			),
			verbose = 0,
			data.table = TRUE
		)
		testthat::expect_equal(wide$`[1.1]home_city`, c("Amsterdam", "Rome", "Berlin"))
		testthat::expect_equal(wide$`[1.1]any_city`, c("Amsterdam", "Rome", "Berlin"))
		testthat::expect_equal(wide$`[2.1]any_city`, c(NA, "Stockholm", NA))
		testthat::expect_equal(wide$`[3.1]any_city`, c(NA, NA, "London"))
	}
)

testthat::test_that(
	"fhir_crack compact given columns deduplicates repeated resource ids", {
		bundles <- fhir_unserialize(example_bundles3)
		duplicated_bundles <- fhir_bundle_list(c(bundles, bundles))

		compact <- fhir_crack(
			bundles = duplicated_bundles,
			design = fhir_table_description(
				resource = "Patient",
				cols = c(id = "id", city = "address/city")
			),
			verbose = 0,
			data.table = TRUE
		)

		testthat::expect_equal(nrow(compact), 3L)
		testthat::expect_equal(compact$id, c("id1", "id2", "id3"))
		testthat::expect_equal(
			compact$city,
			c("Amsterdam", "Rome:::Stockholm", "Berlin:::London")
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
