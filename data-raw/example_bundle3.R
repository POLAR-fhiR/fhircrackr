## code to prepare `example_bundles3` dataset goes here
bundle <- xml2::read_xml(
	"<Bundle>
     <type value='searchset'/>

	 <entry>
		 <resource>
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
		</resource>
	 </entry>

	 <entry>
		 <resource>
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
		</resource>
	 </entry>

	 <entry>
		 <resource>
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
		</resource>
	 </entry>

	 <entry>
		 <resource>
			<Observation>
				<id value = 'obs1'/>
				<code>
					<coding>
					   <system value='http://loinc.org'/>
					   <code value='29463-7'/>
					   <display value='Body Weight'/>
					</coding>
					<coding>
					   <system value='http://snomed.info/sct'/>
				 	   <code value='27113001'/>
					   <display value='Body weight'/>
					</coding>
				</code>
				<subject>
					<reference value = 'Patient/id2'/>
				</subject>
			</Observation>
		</resource>
	 </entry>

	</Bundle>"
)

bundle <- fhir_bundle_xml(bundle)

bundle <- fhir_serialize(bundle)

example_bundles3 <- fhir_bundle_list(list(bundle))

usethis::use_data(example_bundles3, overwrite = TRUE)