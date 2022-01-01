testthat::test_that(
	"fhir_table_description is built correctly", {
		d1 <- fhir_table_description(
			resource = "Patient"
		)
		d2 <- fhir_table_description(
			resource = "Patient",
			cols     = c(
				id     = "id",
				name   = "name/family",
				gender = "gender"
			),
			sep           = "||",
			brackets      = c("[", "]"),
			rm_empty_cols = FALSE,
			format        = 'wide',
			keep_attr     = FALSE
		)
		d3 <- fhir_table_description(
			resource = "Patient",
			cols     = c(
				"id",
				"name/family",
				"gender"
			)
		)
		d4 <- fhir_table_description(
			resource = "Patient",
			cols     = fhir_columns(
				c(
					"id",
					"name/family",
					"gender"
				)
			)
		)
		d5 <- fhir_table_description(
			resource = "Patient",
			cols     = fhir_columns(
				list(
					"id",
					"name/family",
					"gender"
				)
			)
		)
		testthat::expect_warning(
			d6 <- fhir_table_description(
				resource = "Patient",
				cols     = fhir_columns(
					list(
						"id",
						"name/family",
						"gender"
					)
				),
				keep_attr = TRUE
			)
		)
		testthat::expect_s4_class(d1, "fhir_table_description")
		testthat::expect_s4_class(d2, "fhir_table_description")
		testthat::expect_s4_class(d3, "fhir_table_description")
		testthat::expect_s4_class(d4, "fhir_table_description")
		testthat::expect_s4_class(d5, "fhir_table_description")
		testthat::expect_s4_class(d6, "fhir_table_description")
	}
)

testthat::test_that(
	"methods for fhir_table_description() create identical results", {
		d1 <- fhir_table_description(
			resource = "Patient",
			cols     = c(
				id     = "id",
				name   = "name/family",
				gender = "gender"
			)
		)
		d2 <- fhir_table_description(
			resource = "Patient",
			cols     = list(
				id     = "id",
				name   = "name/family",
				gender = "gender"
			)
		)
		d3 <- fhir_table_description(
			resource = "Patient",
			cols     = fhir_columns(
				c(
					id     = "id",
					name   = "name/family",
					gender = "gender"
				)
			)
		)
		d4 <- fhir_table_description(
			resource = "Patient",
			cols     = fhir_columns(
				list(
					id     = "id",
					name   = "name/family",
					gender = "gender"
				)
			)
		)
		testthat::expect_identical(d1, d2)
		testthat::expect_identical(d1, d3)
		testthat::expect_identical(d1, d4)
	}
)
