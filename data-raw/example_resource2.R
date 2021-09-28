
resource <- xml2::read_xml(
	"<Patient>
		<id value = '1a2b3c'/>
		<name>
			<given value = 'Marie'/>
		</name>
		<gender value = 'female'/>
		<birthDate value = '1970-01-01'/>
	</Patient>"
)

resource <- fhir_resource_xml(resource)

example_resource2 <- fhir_serialize(resource)

usethis::use_data(example_resource2, overwrite = TRUE)
