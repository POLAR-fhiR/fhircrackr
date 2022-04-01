## code to prepare `example_bundles4` dataset goes here
bundle <- xml2::read_xml(
	"<Bundle>
     <type value='searchset'/>
	 <entry>
		 <resource>
			<Medication>
				<id value='1285'/>
			    <code>
			        <coding>
			            <system value='http://www.nlm.nih.gov/research/umls/rxnorm'/>
			            <code value='1594660'/>
			            <display value='Alemtuzumab 10mg/ml (Lemtrada)'/>
			        </coding>
			    </code>
			    <ingredient id='1'>
			    	<itemReference>
		        		<reference value='Substance/5463'/>
		    		</itemReference>
			    </ingredient>
			    <ingredient id='2'>
			    	<itemReference>
		        		<reference value='Substance/3401'/>
		    		</itemReference>
			    </ingredient>
			</Medication>
		</resource>
	 </entry>

	 <entry>
		 <resource>
            <Medication>
                <id value='45226'/>
                <code>
                    <coding>
                        <system value='http://snomed.info/sct'/>
                        <code value='373994007'/>
                        <display value='Prednisone 5mg tablet (Product)'/>
                    </coding>
                    <text value='Prednisone 5mg tablet (Product)'/>
                </code>
			    <ingredient id='1'>
			    	<itemReference>
		        		<reference value='Substance/6912'/>
		    		</itemReference>
			    </ingredient>
			    <ingredient id='2'>
			    	<itemReference>
		        		<reference value='Substance/3710'/>
		    		</itemReference>
			    </ingredient>
			</Medication>
		</resource>
	 </entry>

	</Bundle>"
)

bundle <- fhir_bundle_xml(bundle)

bundle <- fhir_serialize(bundle)

example_bundles4 <- fhir_bundle_list(list(bundle))

usethis::use_data(example_bundles4, overwrite = TRUE)
