## code to prepare `example_bundles7` dataset goes here
bundle <- xml2::read_xml(
"<Bundle>
     <type value='searchset'/>
     <entry>
    	<resource>
			<Patient>
				<id value='id1'/>
				<name>
					<given value='Marie'/>
					<given value='Luise'/>
					<family value = 'Smith'/>
					<use value = 'official'/>
				</name>
				<name>
					<given value = 'Lea'/>
					<given value = 'Sophie'/>
					<given value = 'Anna'/>
					<family value = 'Baker'/>
					<use value = 'nickname'/>
				</name>
			</Patient>
		 </resource>
	  </entry>
	  <entry>
		<resource>
			<Patient>
				<id value='id2'/>
				<name>
					<given value='Max'/>
					<family value = 'Brown'/>
					<use value = 'official'/>
				</name>
				<name>
					<given value = 'Anton'/>
					<given value = 'John'/>
					<family value = 'Jones'/>
					<use value = 'nickname'/>
				</name>
			</Patient>
		</resource>
	  </entry>
</Bundle>"
)

bundle <- fhir_bundle_xml(bundle)

bundle <- fhir_serialize(bundle)

example_bundles7 <- fhir_bundle_list(list(bundle))

usethis::use_data(example_bundles7, overwrite = TRUE)