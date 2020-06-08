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

rm( list = ls( ) )

design <- list(

	Pat = list(
		".//Patient",
		list(
			name = "name/family/@value"
		)
	)
)

bundle <- fhiR::get.bundle( "https://vonk.fire.ly/R4/Patient?_pretty=true&_count=100000" )

test_that(
	"get.bundle downloads a valid bundle", {
		expect_equal( is.null( bundle ), F )
		expect_equal( isClass( "xml_node", bundle ), T )
	}
)

