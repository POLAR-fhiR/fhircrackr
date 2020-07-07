#########################################################################################################
testthat::context( "crack()" )

xmlfile <- xml2::read_xml( "specimen.xml" )

design <- list(

	Specimen = list(
		"//extension[@url='https://fhir.bbmri.de/StructureDefinition/StorageTemperature']",
		list(
			VCS  = "valueCodeableConcept/coding/system/@value",
			CODE = "valueCodeableConcept/coding/code/@value"
		)
	)
)

df <- fhir_crack(bundles = list(xmlfile), design)

testthat::test_that(
	"crack creates a valid data frame", {
		testthat::expect_false( is.null( df ) )
		testthat::expect_false( is.null( df$Specimen ) )
		testthat::expect_true( is.data.frame( df$Specimen ) )
		testthat::expect_equal( nrow( df$Specimen ), 1 )
		testthat::expect_equal( df$Specimen$VCS[ 1 ],  "https://fhir.bbmri.de/CodeSystem/StorageTemperature" )
		testthat::expect_equal( df$Specimen$CODE[ 1 ], "temperature2to10" )
	}
)


#########################################################################################################
testthat::context( "fhir_search()" )


testthat::test_that(
	"fhir_search downloads a valid bundle list", {

		testthat::skip_on_cran()

		bundles <- fhir_search( request = "https://hapi.fhir.org/baseR4/Patient?_pretty=true", max_bundles = 5 )

		testthat::expect_false( is.null( bundles ) )
		testthat::expect_true( is.list( bundles ) )
		testthat::expect_true( 0 < length( bundles ) )
		testthat::expect_true( isClass( "xml_node", bundles[[ 1 ]] ) )
	}
)


#########################################################################################################
testthat::context( "fhir_save" )

fhir_save( fhir_unserialize(patient_bundles), "myBundles" )

testthat::test_that(
	"fhir_save stores all bundles as xml files in the required directory", {
		testthat::expect_true( any( "myBundles" %in% dir( ) ) )
		testthat::expect_true( 0 < length( dir( "myBundles" ) ) )
	}
)


#########################################################################################################
testthat::context( "fhir_load()" )

myBundles <- fhir_load( "myBundles" )

testthat::test_that(
	"fhir_load reads all bundles as xml files from the given directory", {
		testthat::expect_false( is.null( myBundles ) )
		testthat::expect_true( is.list( myBundles ) )
		testthat::expect_true( 0 < length( myBundles ) )
		testthat::expect_true( isClass( "xml_node", myBundles[[ 1 ]] ) )
	}
)


#########################################################################################################
testthat::context( "fhir_crack()" )

design <- list(

	Pat = list(
		".//Patient",
		list(
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


#########################################################################################################
testthat::context( "fhir_capability_statement()" )

testthat::test_that(
	"fhir_capability_statement() works", {

		testthat::skip_on_cran()

		caps <- fhir_capability_statement( "https://hapi.fhir.org/baseR4", sep = " ~ ")

		testthat::expect_false( is.null( caps ) )
		testthat::expect_true( is.list( caps ) )
		testthat::expect_true( is.data.frame( caps[[ 1 ]] ) )
		testthat::expect_true( is.data.frame( caps[[ 2 ]] ) )
		testthat::expect_true( is.data.frame( caps[[ 3 ]] ) )
	}
)

unlink( "myBundles", recursive = T )
