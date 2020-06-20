#########################################################################################################
testthat::context( "crack()" )

xmlfile <- xml2::read_xml( "specimen.xml" )

#xml2::xml_ns_strip( xmlfile )

design <- list(

	Specimen = list(
		".//extension[@url='https://fhir.bbmri.de/StructureDefinition/StorageTemperature']",
		list(
			VCS  = "valueCodeableConcept/coding/system/@value",
			CODE = "valueCodeableConcept/coding/code/@value"
		)
	)
)

df <- fhiR::crack(bundles = list(xmlfile), design)

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

bundles <- fhiR::fhir_search( request = "https://vonk.fire.ly/R4/Patient?_pretty=true&_count=100000", max.bundles = 10 )

testthat::test_that(
	"fhir_search downloads a valid bundle list", {
		testthat::expect_equal( is.null( bundles ), F )
		testthat::expect_equal( is.list( bundles ), T )
		testthat::expect_equal( 0 < length( bundles ), T )
		testthat::expect_equal( isClass( "xml_node", bundles[[ 1 ]] ), T )
	}
)


#########################################################################################################
testthat::context( "save_bundles()" )

fhiR::save_bundles( bundles, "myBundles" )

testthat::test_that(
	"save_bundles stores all bundles as xml files in the required directory", {
		testthat::expect_equal( any( "myBundles" %in% dir( ) ), T )
		testthat::expect_equal( 0 < length( dir( "myBundles" ) ), T )
	}
)


#########################################################################################################
testthat::context( "load_bundles()" )

myBundles <- fhiR::load_bundles( "myBundles" )

testthat::test_that(
	"load_bundles reads all bundles as xml files from the given directory", {
		testthat::expect_equal( is.null( myBundles ), F )
		testthat::expect_equal( is.list( myBundles ), T )
		testthat::expect_equal( 0 < length( myBundles ), T )
		testthat::expect_equal( isClass( "xml_node", myBundles[[ 1 ]] ), T )
	}
)


#########################################################################################################
testthat::context( "crack()" )

design <- list(

	Pat = list(
		".//Patient",
		list(
			name = "name/family/@value"
		)
	)
)

dfs <- fhiR::crack( bundles = myBundles, design = design, sep = "»" )

testthat::test_that(
	"fhir2dfs creates all required data frames", {
		testthat::expect_equal( is.null( dfs ), F )
		testthat::expect_equal( is.list( dfs ), T )
		testthat::expect_equal( is.data.frame( dfs[[ 1 ]] ), T )
	}
)


#########################################################################################################
testthat::context( "capability_statement()" )

caps <- capability_statement( "https://hapi.fhir.org/baseR4", sep = " ~ ")

testthat::test_that(
	"capability_statement() works", {
		testthat::expect_false( is.null( caps ) )
		testthat::expect_true( is.list( caps ) )
		testthat::expect_true( is.data.frame( caps[[ 1 ]] ) )
		testthat::expect_true( is.data.frame( caps[[ 2 ]] ) )
		testthat::expect_true( is.data.frame( caps[[ 3 ]] ) )
	}
)

unlink( "myBundles", recursive = T )
