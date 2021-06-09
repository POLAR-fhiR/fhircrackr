
testthat::test_that(
	"fhir_dt_list is built correctly", {
		df_desc1 <- fhir_table_description(resource = "Patient",
		                    cols = c(name = "name/family",
		                             gender = "gender",
		                             id = "id"),
		                    style = fhir_style(sep = "||",
		                                       brackets = c("[", "]"),
		                                       rm_empty_cols = FALSE
		                            )
		             )

		df_desc2 <- fhir_table_description(resource = "Observation",
		                    cols = c("code/coding/system", "code/coding/code")
		            )

		design <- fhir_design(list(Patients = df_desc1, Observations = df_desc2))

		dt_list <- list(Patients = data.table(), Observations = data.table())

		fdtl <- fhir_dt_list(dt_list, design)

		testthat::expect_s4_class(fdtl, "fhir_dt_list")
	}
)


testthat::test_that(
	"fhir_df_list is built correctly", {
		df_desc1 <- fhir_table_description(resource = "Patient",
										cols = c(name = "name/family",
												 gender = "gender",
												 id = "id"),
										style = fhir_style(sep = "||",
														   brackets = c("[", "]"),
														   rm_empty_cols = FALSE
										)
		)

		df_desc2 <- fhir_table_description(resource = "Observation",
										cols = c("code/coding/system", "code/coding/code")
		)

		design <- fhir_design(list(Patients = df_desc1, Observations = df_desc2))

		df_list <- list(Patients = data.frame(), Observations = data.frame())

		fdfl <- fhir_df_list(df_list = df_list, design = design)

		testthat::expect_s4_class(fdfl, "fhir_df_list")
	}
)
