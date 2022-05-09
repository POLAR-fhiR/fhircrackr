testthat::test_that(
	"fhir_crack() returns data.tables", {

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

testthat::test_that(
	"Crack to wide returns appropriate dimensions", {

		brackets <- c("[", "]")
		bundles <- fhir_unserialize(example_bundles3)

		t1 <- fhir_crack(
			bundles,
			design = fhir_table_description(
				resource = "Patient",
				brackets = brackets,
				format = "wide"
			),
			verbose = 0,
			data.table = T)

		t2 <- fhir_crack(
			bundles,
			design = fhir_table_description(
				resource = "Patient",
				cols = c("id", "address/type", "address/use", "address/city", "address/country"),
				format = "wide",
				brackets = brackets
			),
			verbose = 0)

		testthat::expect_equal(ncol(t1), 15)
		testthat::expect_equal(ncol(t2), 13)
	}
)

testthat::test_that(
	"Crack compactreturns appropriate dimensions", {

		brackets <- c("[", "]")
		bundles <- fhir_unserialize(example_bundles3)

		t1 <- fhir_crack(
			bundles,
			design = fhir_table_description(
				resource = "Patient",
				brackets = brackets,
				format = "compact"
			),
			verbose = 0,
			data.table = T)

		t2 <- fhir_crack(
			bundles,
			design = fhir_table_description(
				resource = "Patient",
				cols = c("id", "address/type", "address/use", "address/city", "address/country"),
				format = "compact",
				brackets = brackets
			),
			verbose = 0)

		testthat::expect_equal(ncol(t1), 6)
		testthat::expect_equal(ncol(t2), 5)
	}
)
