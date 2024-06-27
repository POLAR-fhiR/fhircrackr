## code to prepare `example_bundles6` dataset goes here
bundle <- xml2::read_xml(
"<Bundle>
     <type value='searchset'/>
     <entry>
    	<resource>
			<Patient>
				<id value='id1'/>
				<address>
					<line value='Example Street 1'/>
					<line value='c/o Baker'/>
					<city value = 'London'/>
					<use value = 'home'/>
				</address>
				<address>
					<line value = 'Some firm'/>
					<line value = 'Some Department'/>
					<line value = 'Some Lane 1'/>
					<city value = 'London'/>
					<use value = 'work'/>
				</address>
			</Patient>
		 </resource>
	  </entry>
	  <entry>
		<resource>
			<Patient>
				<id value='id2'/>
				<address>
					<line value='Rue example 3'/>
					<city value = 'Paris'/>
					<use value = 'home'/>
				</address>
				<address>
					<line value = 'La fabrique'/>
					<line value = 'Avenue 33'/>
					<city value = 'Paris'/>
					<use value = 'work'/>
				</address>
			</Patient>
		</resource>
	  </entry>
</Bundle>"
)

bundle <- fhir_bundle_xml(bundle)

bundle <- fhir_serialize(bundle)

example_bundles6 <- fhir_bundle_list(list(bundle))

usethis::use_data(example_bundles6, overwrite = TRUE)