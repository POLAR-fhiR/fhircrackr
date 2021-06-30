
testthat::test_that(
	"fhir_design is built correctly", {
		d1 <- fhir_table_description(resource = "Patient",
								  cols = c(name = "name/family",
								  		 gender = "gender",
								  		 id = "id"),
								  style = fhir_style(sep = "||",
								  				   brackets = c("[", "]"),
								  				   rm_empty_cols = FALSE
								  )
		)
		d2 <- fhir_table_description(resource = "Patient",
								  cols = c("name/family",
								  		 "gender",
								  		 "id")
		)
		d3 <- fhir_table_description(resource = "Patient",
								  cols = fhir_columns(c("name/family",
								  					  "gender",
								  					  "id"))
		)

		design <- fhir_design(d1, d2, d3)

		old_design <- list(
		                 Patients = list(
		                    resource = "//Patient",
		                    cols = list(
		                       name = "name/family",
		                       gender = "gender",
		                       id = "id"),
		                    style = list(
		                       sep = "||",
		                       brackets = c("[", "]"),
		                       rm_empty_cols = FALSE
		                    )
		                 ),
		                 Observations = list(
		                    resource = "//Observation",
		                    cols = list(
		                       code.coding.system = "code/coding/system",
		                       code.coding.code = "code/coding/code"
		                    )
		                 )
		              )

		new_design <- fhir_design(old_design)

		testthat::expect_s4_class(design, "fhir_design")
		testthat::expect_s4_class(new_design, "fhir_design")

	}
)

testthat::test_that(
	"methods for fhir_design() create identical results", {
		d1 <- fhir_table_description(resource = "Patient",
								  cols = c(name = "name/family",
								  		 gender = "gender",
								  		 id = "id")
		)
		d2 <- fhir_table_description(resource = "Patient",
								  cols = list(name = "name/family",
								  			gender = "gender",
								  			id = "id")
		)

		design1 <-  fhir_design(d1, d2)
		design2 <-  fhir_design(list(d1=d1, d2=d2))

		testthat::expect_identical(design1, design2)
	}
)

testthat::test_that(
	"fhir_design gets names correctly", {
		d1 <- fhir_design(
			fhir_table_description(resource = "Patient",
								   cols = c(name = "name/family",
								   		 gender = "gender",
								   		 id = "id")
			),
			fhir_table_description(resource = "Medication",
								   cols = c(name = "code",
								   		 id = "id")
			)
		)

		d2 <- fhir_design(
			Pat = fhir_table_description(resource = "Patient",
										 cols = c(name = "name/family",
										 		 gender = "gender",
										 		 id = "id")
			),
			fhir_table_description(resource = "Medication",
								   cols = c(name = "code",
								   		 id = "id")
			)
		)

		d3 <- fhir_design(
			Pat = fhir_table_description(resource = "Patient",
										 cols = c(name = "name/family",
										 		 gender = "gender",
										 		 id = "id")
			),
			Med =			fhir_table_description(resource = "Medication",
										   cols = c(name = "code",
										   		 id = "id")
			)
		)

		testthat::expect_identical(names(d1), c("Patients", "Medications"))
		testthat::expect_identical(names(d2), c("Pat", "Medications"))
		testthat::expect_identical(names(d3), c("Pat", "Med"))
	}
)

testthat::test_that(
	"fhir_design throws an error when using not unique names otherwise not", {
		testthat::expect_error(
			fhir_design(
				fhir_table_description(
					resource = "Patient",
					cols = fhir_columns(c(
						name = "name/family",
						gender = "gender",
						id = "id"
					))
				),
				fhir_table_description(
					resource = "Patient"
				)
			)
		)

		Resources <- fhir_table_description(
			resource = "Patient"
		)

		Patients <- fhir_table_description(
			resource = "Patient"
		)

		testthat::expect_error(
			fhir_design(
				Resources,
				Resources
			)
		)

		testthat::expect_error(
			fhir_design(
				Patients,
				fhir_table_description(
					resource = "Patient"
				)
			)
		)

		testthat::expect_s4_class(
			fhir_design(
				Resources,
				Patients
			),
			"fhir_design"
		)

		testthat::expect_s4_class(
			fhir_design(
				Patients1 = Patients,
				Patients2 = Patients
			),
			"fhir_design"
		)
	}
)
