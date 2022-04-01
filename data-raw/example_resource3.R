
resource <- xml2::read_xml(
	'<Medication>
	    <code>
	        <coding>
	            <system value="http://www.nlm.nih.gov/research/umls/rxnorm"/>
	            <code value="1594660"/>
	            <display value="Alemtuzumab 10mg/ml (Lemtrada)"/>
	        </coding>
	    </code>
	    <ingredient id="1">
	    	<itemReference>
        		<reference value="Substance/5463"/>
    		</itemReference>
	    </ingredient>
	    <ingredient id="2">
	    	<itemReference>
        		<reference value="Substance/3401"/>
    		</itemReference>
	    </ingredient>
	</Medication>
	'
)

resource <- fhir_resource_xml(resource)

example_resource3 <- fhir_serialize(resource)

usethis::use_data(example_resource3, overwrite = TRUE)
