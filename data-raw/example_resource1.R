
resource <- xml2::read_xml(
	"<Patient>
		<name>
			<given value = 'Marie'/>
		</name>
		<gender value = 'female'/>
		<birthDate value = '1970-01-01'/>
	</Patient>"
)

resource <- fhir_resource_xml(resource)

example_resource1 <- fhir_serialize(resource)

usethis::use_data(example_resource1, overwrite = TRUE)
