#########################################################################################################
testthat::context( "fhir_search()" )


testthat::test_that(
	"fhir_search downloads a valid bundle list", {

		testthat::skip_on_cran()

		bundles <- fhir_search( request = "https://server.fire.ly/Patient?", max_bundles = 2 )

		testthat::expect_false( is.null( bundles ) )
		testthat::expect_true( is.list( bundles ) )
		testthat::expect_true( 0 < length( bundles ) )
		testthat::expect_true( isClass( "xml_node", bundles[[ 1 ]] ) )
	}
)


#########################################################################################################
testthat::context( "fhir_save" )

directory <- tempdir()

fhir_save( fhir_unserialize(patient_bundles), directory)

testthat::test_that(
	"fhir_save stores all bundles as xml files in the required directory", {
		testthat::expect_true( 0 < length( dir( directory ) ) )
	}
)


#########################################################################################################
testthat::context( "fhir_load()" )

myBundles <- fhir_load( directory )

testthat::test_that(
	"fhir_load reads all bundles as xml files from the given directory", {
		testthat::expect_false( is.null( myBundles ) )
		testthat::expect_true( is.list( myBundles ) )
		testthat::expect_true( 0 < length( myBundles ) )
		testthat::expect_true( isClass( "xml_node", myBundles[[ 1 ]] ) )
	}
)




#########################################################################################################
testthat::context( "fhir_capability_statement()" )

testthat::test_that(
	"fhir_capability_statement() works", {

		testthat::skip_on_cran()

		caps <- fhir_capability_statement("https://server.fire.ly", sep = " ~ ")

		testthat::expect_false( is.null( caps ) )
		testthat::expect_true( is.list( caps ) )
		testthat::expect_true( is.data.frame( caps[[ 1 ]] ) )
		testthat::expect_true( is.data.frame( caps[[ 2 ]] ) )
		testthat::expect_true( is.data.frame( caps[[ 3 ]] ) )
	}
)

#########################################################################################################
testthat::context( "fhir_crack()" )

bundles <- fhir_unserialize(patient_bundles)

design <- fhir_design(
	Patient = fhir_table_description(resource = "Patient")
)

df <- fhir_crack(bundles = bundles, design)

testthat::test_that(
	"crack creates a valid data frame", {
		testthat::expect_false( is.null( df ) )
		testthat::expect_false( is.null( df$Patient ) )
		testthat::expect_true( is.data.frame( df$Patient ) )
		testthat::expect_equal( nrow( df$Patient), 40 )
	}
)

design <- fhir_design(

	Pat = fhir_table_description(
		resource = "Patient",
		cols = list(
			name = "name/family/@value"
		)
	)
)

dfs <- fhir_crack( bundles = myBundles, design = design, sep = "»" )

testthat::test_that(
	"fhir_crack creates all required data frames", {
		testthat::expect_equal( is.null( dfs ), F )
		testthat::expect_equal( is.list( dfs ), T )
		testthat::expect_equal( is.data.frame( dfs[[ 1 ]] ), T )
	}
)


unlink( "myBundles", recursive = T )


