
testthat::test_that(
	"fhir_df_description is built correctly", {
		d1 <- fhir_df_description(resource = "Patient",
		                    cols = c(name = "name/family",
		                             gender = "gender",
		                             id = "id"),
		                    style = fhir_style(sep = "||",
		                                       brackets = c("[", "]"),
		                                       rm_empty_cols = FALSE
		                    )
		)
		d2 <- fhir_df_description(resource = "Patient",
		                    cols = c("name/family",
		                                "gender",
		                                "id")
		)
		d3 <- fhir_df_description(resource = "Patient",
								  cols = fhir_columns(c("name/family",
								  		 "gender",
								  		 "id"))
		)
		testthat::expect_s4_class(d1, "fhir_df_description")
		testthat::expect_s4_class(d2, "fhir_df_description")
		testthat::expect_s4_class(d3, "fhir_df_description")
	}
)

testthat::test_that(
	"methods for fhir_df_description() create identical results", {
		d1 <- fhir_df_description(resource = "Patient",
		                    cols = c(name = "name/family",
		                             gender = "gender",
		                             id = "id")
		)
		d2 <- fhir_df_description(resource = "Patient",
		                    cols = list(name = "name/family",
		                             gender = "gender",
		                             id = "id")
		)

		testthat::expect_identical(d1, d2)
	}
)
