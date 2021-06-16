## code to prepare `example_bundles1` dataset goes here
bundle <- xml2::read_xml(
	"<Bundle>

		<Patient>
			<id value='id1'/>
			<address>
				<use value='home'/>
				<city value='Amsterdam'/>
				<type value='physical'/>
				<country value='Netherlands'/>
			</address>
			<name>
				<given value='Marie'/>
			</name>
		</Patient>

		<Patient>
			<id value='id3'/>
			<address>
				<use value='home'/>
				<city value='Berlin'/>
			</address>
			<address>
				<type value='postal'/>
				<country value='France'/>
			</address>
			<address>
				<use value='work'/>
				<city value='London'/>
				<type value='postal'/>
				<country value='England'/>
			</address>
			<name>
				<given value='Frank'/>
			</name>
			<name>
				<given value='Max'/>
			</name>
		</Patient>

	</Bundle>"
)

bundle <- fhir_bundle_xml(bundle)

bundle <- fhir_serialize(bundle)

example_bundles1 <- fhir_bundle_list(list(bundle))

usethis::use_data(example_bundles1, overwrite = TRUE)
