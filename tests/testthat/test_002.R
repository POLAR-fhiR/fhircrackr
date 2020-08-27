##preparation

#correct: full description
df_desc1 <- list(
	resource = "//Patient",
	cols = list(gender = "gender",
				name = "name/family"),
	style = list(sep = " ",
				 brackets = " ",
				 rm_empty_cols = TRUE
	)
)

#correct: no style
df_desc2 <- list(
	resource = "//Patient",
	cols = list(gender = "gender",
				name = "name/family")
)

#correct: cols is character
df_desc3 <- list(
	resource = "//Patient",
	cols = ".//*"
)

#correct: no cols
df_desc5 <- list(
	resource = "//Patient"
)

df_desc4 <- list(
	"//Patient"
)

#correct: jumbled order
df_desc6 <- list(

	cols = list(gender = "gender",
				name = "name/family"),
	resource = "//Patient",
	style = list(sep = " ",
				 brackets = " ",
				 rm_empty_cols = TRUE
	)
)

#correct: no names but correct order
df_desc7 <- list(
	"//Patient",
	list(gender = "gender",
		name = "name/family"),
	list(sep = " ",
		 brackets = " ",
		 rm_empty_cols = TRUE
	)
)

#wrong: missing names and incorrect order
df_desc8 <- list(

	cols = list(gender = "gender",
				name = "name/family"),
	list(sep = " ",
		 brackets = " ",
		 rm_empty_cols = TRUE
	),
	"//Patient"
)

#wrong: empty description
df_desc9 <- list()

#wrong: non-xml
df_desc10 <- list(
	"\\Patient")

df_desc11 <- list(
	"//Patient",
	cols= list(gender="\\gender")
)

#wrong: incorrect types
df_desc12 <- list(
	resource = 3
)

df_desc13 <- list(
	resource = "//Patient",
	cols = data.frame()
)

df_desc14 <- list(
	resource = "//Patient",
	style = data.frame(
		sep = " ",
		brackets= 4,
		rm_empty_cols=T
	)
)

df_desc15 <- list(
	resource = "//Patient",
	style= list(
		sep = 3,
		brackets = FALSE
	)
)

df_desc16 <- data.frame(
	resource = "//Patient"
)

#correct design
design1 <- list(
	pat1 = df_desc1,
	pat2 = df_desc2,
	pat3 = df_desc3,
	pat4 = df_desc4,
	pat5 = df_desc5
)

##wrong: missing names
design2 <- list(
	pat1 = df_desc1,
	df_desc2,
	pat3 = df_desc3,
	pat4 = df_desc4
)

#invalid df_descriptions
design3 <- list(
	pat1 = df_desc1,
	pat3 = df_desc12,
	pat4 = df_desc4
)

#wrong: invalid type
design4 <- data.frame(
	pat1 = "//Patient"
)

design5 <- list(
	pat1 = df_desc1,
	pat2 = df_desc2,
	pat3 = df_desc3,
	pat4 = df_desc4,
	pat5 = df_desc5,
	pat6 = df_desc6,
	pat7 = df_desc7,
	pat8 = df_desc8,
	pat9 = df_desc9,
	pat10 = df_desc10,
	pat11 = df_desc11,
	pat12 = df_desc12,
	pat13 = df_desc13,
	pat14 = df_desc14,
	pat15 = df_desc15,
	pat16 = df_desc16
)

pat_bundles <- fhir_unserialize(patient_bundles)


#########################################################################################################
testthat::context("is_valid_df_desc()")

testthat::test_that(

	"is_valid_df_desc() identifies invalid df_descriptions", {

		testthat::expect_true(fhircrackr:::is_valid_df_desc(df_desc1)$valid)
		testthat::expect_true(fhircrackr:::is_valid_df_desc(df_desc2)$valid)
		testthat::expect_true(fhircrackr:::is_valid_df_desc(df_desc3)$valid)
		testthat::expect_true(fhircrackr:::is_valid_df_desc(df_desc4)$valid)
		testthat::expect_true(fhircrackr:::is_valid_df_desc(df_desc5)$valid)
		testthat::expect_true(fhircrackr:::is_valid_df_desc(df_desc6)$valid)
		testthat::expect_true(fhircrackr:::is_valid_df_desc(df_desc7)$valid)

		testthat::expect_false(fhircrackr:::is_valid_df_desc(df_desc8)$valid)
		testthat::expect_false(fhircrackr:::is_valid_df_desc(df_desc9)$valid)
		testthat::expect_false(fhircrackr:::is_valid_df_desc(df_desc10)$valid)
		testthat::expect_false(fhircrackr:::is_valid_df_desc(df_desc11)$valid)
		testthat::expect_false(fhircrackr:::is_valid_df_desc(df_desc12)$valid)
		testthat::expect_false(fhircrackr:::is_valid_df_desc(df_desc13)$valid)
		testthat::expect_false(fhircrackr:::is_valid_df_desc(df_desc14)$valid)
		testthat::expect_false(fhircrackr:::is_valid_df_desc(df_desc15)$valid)
		testthat::expect_false(fhircrackr:::is_valid_df_desc(df_desc16)$valid)

	}
)

testthat::context("is_valid_design()")
testthat::test_that(

	"is_valid_design() identifies invalid designs",{

		testthat::expect_true(suppressWarnings(fhircrackr:::is_valid_design(design1)[[1]]))
		testthat::expect_false(suppressWarnings(fhircrackr:::is_valid_design(design2)[[1]]))
		testthat::expect_false(suppressWarnings(fhircrackr:::is_valid_design(design3)[[1]]))
		testthat::expect_false(suppressWarnings(fhircrackr:::is_valid_design(design4)[[1]]))


	}
)

testthat::context("fhir_crack design warnings")

testthat::test_that(

	"design warnings are thrown in fhir_crack",{
		testthat::expect_warning(fhir_crack(pat_bundles, design1, verbose=0), regexp = "There are unnamed elements")
		testthat::expect_warning(fhir_crack(pat_bundles, design2, verbose=0), regexp = "Argument design should be a named list of data.frame descriptions")
		testthat::expect_warning(fhir_crack(pat_bundles, design3, verbose=0), regexp = "The following data.frame descriptions in your design seem to be invalid")
		testthat::expect_warning(fhir_crack(pat_bundles, design4, verbose=0), regexp = "Argument design has to be a list")
		testthat::expect_warning(fhir_crack(pat_bundles, design5, verbose=0), regexp = "The following data.frame descriptions in your design seem to be invalid")
	}
)

testthat::context("fhir_crack return_design")

testthat::test_that(

	"return_design works", {
		testthat::expect_length(suppressWarnings(fhir_crack(pat_bundles, design1, return_design = TRUE, verbose = 0)$design), 5)
		testthat::expect_null(suppressWarnings(fhir_crack(pat_bundles, design2, return_design = TRUE, verbose = 0)$design))
		testthat::expect_length(suppressWarnings(fhir_crack(pat_bundles, design3, return_design = TRUE, verbose = 0)$design), 3)
		testthat::expect_null(suppressWarnings(fhir_crack(pat_bundles, design4, return_design = TRUE, verbose = 0)$design))
		testthat::expect_length(suppressWarnings(fhir_crack(pat_bundles, design5, return_design = TRUE, verbose = 0)$design), 16)


	}
)