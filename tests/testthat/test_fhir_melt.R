
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

testthat::test_that(
	"fhir_melt preserves row expansion with missing values", {
		indexed_data <- data.table::data.table(
			x = c(NA_character_, "[1]a|[2]b"),
			y = c("[1]A", NA_character_)
		)

		result <- fhir_melt(
			indexed_data_frame = indexed_data,
			columns = c("x", "y"),
			brackets = c("[", "]"),
			sep = "|",
			all_columns = TRUE
		)

		expected <- data.table::data.table(
			x = c(NA_character_, "[]a", "[]b"),
			y = c("[]A", NA_character_, NA_character_),
			resource_identifier = c(1L, 2L, 2L)
		)
		data.table::setkey(expected, resource_identifier)

		testthat::expect_identical(result, expected)
	}
)

testthat::test_that(
	"fhir_melt preserves nested indices within the same row", {
		indexed_data <- data.table::data.table(
			x = "[1.1]a|[1.2]b|[2.1]c",
			y = "[1]Y|[2]Z"
		)

		result <- fhir_melt(
			indexed_data_frame = indexed_data,
			columns = c("x", "y"),
			brackets = c("[", "]"),
			sep = "|",
			all_columns = TRUE
		)

		expected <- data.table::data.table(
			x = c("[1]a|[2]b", "[1]c"),
			y = c("[]Y", "[]Z"),
			resource_identifier = c(1L, 1L)
		)
		data.table::setkey(expected, resource_identifier)

		testthat::expect_identical(result, expected)
	}
)
