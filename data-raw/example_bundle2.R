## code to prepare `example_bundles2` dataset goes here
bundle <- xml2::read_xml(
"<Bundle>
     <type value='searchset'/>

	 <entry>
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
	 </entry>

	 <entry>
		<Patient>
			<id value='id2'/>
			<address>
				<use value='home'/>
				<city value='Rome'/>
				<type value='physical'/>
				<country value='Italy'/>
			</address>
			<address>
				<use value='work'/>
				<city value='Stockholm'/>
				<type value='postal'/>
				<country value='Sweden'/>
			</address>
			<name>
				<given value='Susie'/>
			</name>
		</Patient>
	 </entry>

	 <entry>
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
	 </entry>

</Bundle>"
)

bundle <- fhir_bundle_xml(bundle)

bundle <- fhir_serialize(bundle)

example_bundles2 <- fhir_bundle_list(list(bundle))

usethis::use_data(example_bundles2, overwrite = TRUE)
