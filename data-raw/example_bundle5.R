## code to prepare `example_bundles5` dataset goes here
bundle <- xml2::read_xml(
	"<Bundle>
     <type value='searchset'/>

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

	 <entry>
		 <resource>
			<Observation>
				<id value = 'obs2'/>
				<code>
					<coding>
					   <system value='http://loinc.org'/>
					   <code value='8302-2'/>
					   <display value='Body Height'/>
					</coding>
					<coding>
					   <system value='http://snomed.info/sct'/>
				 	   <code value='50373000'/>
					   <display value='Body height measure'/>
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

example_bundles5 <- fhir_bundle_list(list(bundle))

usethis::use_data(example_bundles5, overwrite = TRUE)