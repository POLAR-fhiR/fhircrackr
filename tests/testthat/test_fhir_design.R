
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
