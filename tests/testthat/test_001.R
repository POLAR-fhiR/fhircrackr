#########################################################################################################
testthat::context( "xml2df()" )

xmlfile <- xml2::read_xml( "specimen.xml" )

#xml2::xml_ns_strip( xmlfile )

design <- list(

	Specimen = list(
		".//d1:extension[./@url='https://fhir.bbmri.de/StructureDefinition/StorageTemperature']",
		list(
			VCS  = "valueCodeableConcept/coding/system/@value",
			CODE = "valueCodeableConcept/coding/code/@value"
		)
	)
)


resource <- xml2::xml_find_all( xmlfile, design$Specimen[[ 1 ]] )

df <- xml2df( xml = resource, dsgn.df = design$Specimen )

testthat::test_that(
	"xml2df creates a valid data frame", {
		testthat::expect_equal( is.null( df ), F )
		testthat::expect_equal( is.data.frame( df ), T )
		testthat::expect_equal( nrow( df ), 1 )
		testthat::expect_equal( df$VCS[ 1 ],  "https://fhir.bbmri.de/CodeSystem/StorageTemperature" )
		testthat::expect_equal( df$CODE[ 1 ], "temperature2to10" )
	}
)


#########################################################################################################
testthat::context( "get_bundle()" )

design <- list(

	Pat = list(
		".//Patient",
		list(
			name = "name/family/@value"
		)
	)
)

bundle     <- get_bundle( "https://hapi.fhir.org/baseR4/Patient?_revinclude=*&_pretty=true&_count=10" )
bundle.tag <- xml2::xml_find_all( bundle, "/d1:Bundle" )

testthat::test_that(
	"get_bundle downloads a valid bundle", {
		testthat::expect_equal( is.null( bundle ), F )
		testthat::expect_equal( isClass( "xml_node", bundle ), T )
		testthat::expect_equal( is.list( bundle.tag ), T )
		testthat::expect_equal( substr( bundle.tag[[ 1 ]], 1, 7 ) == "<Bundle", T )
	}
)


#########################################################################################################
testthat::context( "fhir_search()" )

bundles <- fhir_search( request = "https://vonk.fire.ly/R4/Patient?_pretty=true&_count=100000", max.bundles = 10 )

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

save_bundles( bundles, "myBundles" )

testthat::test_that(
	"save_bundles stores all bundles as xml files in the required directory", {
		testthat::expect_equal( any( "myBundles" %in% dir( ) ), T )
		testthat::expect_equal( 0 < length( dir( "myBundles" ) ), T )
	}
)


#########################################################################################################
testthat::context( "load_bundles()" )

myBundles <- load_bundles( "myBundles" )

testthat::test_that(
	"load_bundles reads all bundles as xml files from the given directory", {
		testthat::expect_equal( is.null( myBundles ), F )
		testthat::expect_equal( is.list( myBundles ), T )
		testthat::expect_equal( 0 < length( myBundles ), T )
		testthat::expect_equal( isClass( "xml_node", myBundles[[ 1 ]] ), T )
	}
)


#########################################################################################################
testthat::context( "fhir2dfs()" )

design <- list(

	Pat = list(
		".//Patient",
		list(
			name = "name/family/@value"
		)
	)
)

dfs <- fhir2dfs( bundles = myBundles, design = design, sep = "»" )

testthat::test_that(
	"fhir2dfs creates all required data frames", {
		testthat::expect_equal( is.null( dfs ), F )
		testthat::expect_equal( is.list( dfs ), T )
		testthat::expect_equal( is.data.frame( dfs[[ 1 ]] ), T )
	}
)


#########################################################################################################
testthat::context( "capability_statement()" )

cnf <- capability_statement( "https://hapi.fhir.org/baseR4", sep = " ~ ", remove.empty.columns = T )

testthat::test_that(
	"capability_statement() works", {
		testthat::expect_equal( is.null( cnf ), F )
		testthat::expect_equal( is.data.frame( dfs[[ 1 ]] ), T )
	}
)

unlink( "myBundles", recursive = T )
