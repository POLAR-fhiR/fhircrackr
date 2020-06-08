#########################################################################################################
context( "xml2df()" )

xmlfile <- xml2::read_xml( "specimen.xml" )

xml2::xml_ns_strip( xmlfile )

design <- list(

	Specimen = list(
		".//extension[./@url='https://fhir.bbmri.de/StructureDefinition/StorageTemperature']",
		list(
			VCS  = "valueCodeableConcept/coding/system/@value",
			CODE = "valueCodeableConcept/coding/code/@value"
		)
	)
)


resource <- xml2::xml_find_all( xmlfile, design$Specimen[[ 1 ]] )

df <- fhiR::xml2df( xml = resource, dsgn.df =  design$Specimen )

test_that(
	"xml2df creates a valid data frame", {
		expect_equal( is.null( df ), F )
		expect_equal( is.data.frame( df ), T )
		expect_equal( nrow( df ), 1 )
		expect_equal( df$VCS[ 1 ],  "https://fhir.bbmri.de/CodeSystem/StorageTemperature" )
		expect_equal( df$CODE[ 1 ], "temperature2to10" )
	}
)


#########################################################################################################
context( "get.bundle()" )

design <- list(

	Pat = list(
		".//Patient",
		list(
			name = "name/family/@value"
		)
	)
)

bundle <- fhiR::get.bundle( "https://hapi.fhir.org/baseR4/Patient?_revinclude=*&_pretty=true&_count=10" )
xml2::xml_ns_strip( bundle )
bundle.tag <- xml2::xml_find_all( bundle, "/Bundle" )

test_that(
	"get.bundle downloads a valid bundle", {
		expect_equal( is.null( bundle ), F )
		expect_equal( isClass( "xml_node", bundle ), T )
		expect_equal( is.list( bundle.tag ), T )
		expect_equal( substr( bundle.tag[[ 1 ]], 1, 8 ) == "<Bundle>", T )
	}
)


#########################################################################################################
context( "get.bundles()" )

bundles <- fhiR::get.bundles( "https://vonk.fire.ly/R4/Patient?_pretty=true&_count=100000" )

test_that(
	"get.bundles downloads a valid bundle list", {
		expect_equal( is.null( bundles ), F )
		expect_equal( is.list( bundles ), T )
		expect_equal( 0 < length( bundles ), T )
		expect_equal( isClass( "xml_node", bundles[[ 1 ]] ), T )
	}
)


#########################################################################################################
context( "save.bundles()" )

fhiR::save.bundles( bundles, "myBundles" )

test_that(
	"save.bundles stores all bundles as xml files in the required directory", {
		expect_equal( any( "myBundles" %in% dir( ) ), T )
		expect_equal( 0 < length( dir( "myBundles" ) ), T )
	}
)


#########################################################################################################
context( "load.bundles()" )

myBundles <- fhiR::load.bundles( "myBundles" )

test_that(
	"load.bundles reads all bundles as xml files from the given directory", {
		expect_equal( is.null( myBundles ), F )
		expect_equal( is.list( myBundles ), T )
		expect_equal( 0 < length( myBundles ), T )
		expect_equal( isClass( "xml_node", myBundles[[ 1 ]] ), T )
	}
)


#########################################################################################################
context( "bundle2dfs()" )

design <- list(

	Pat = list(
		".//Patient",
		list(
			name = "name/family/@value"
		)
	)
)

dfs <- fhiR::bundle2dfs( bundle, design )

test_that(
	"bundle2dfs creates all required data frames", {
		expect_equal( is.null( dfs ), F )
		expect_equal( is.list( dfs ), T )
		expect_equal( is.data.frame( dfs[[ 1 ]] ), T )
	}
)


#########################################################################################################
context( "conformance()" )

cnf <- conformance( "https://hapi.fhir.org/baseR4" )

test_that(
	"conformance() works", {
		expect_equal( is.null( cnf ), F )
		expect_equal( is.data.frame( dfs[[ 1 ]] ), T )
	}
)

unlink( "myBundles", recursive = T )
