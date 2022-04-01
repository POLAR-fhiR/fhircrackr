
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
	        <itemCodeableConcept>
	            <coding>
	                <system value="http://snomed.info/sct"/>
	                <code value="11713004"/>
	                <display value="Water (substance)"/>
	            </coding>
	        </itemCodeableConcept>
	    </ingredient>
	    <ingredient id="2">
	        <itemCodeableConcept>
	            <coding>
	                <system value="http://snomed.info/sct"/>
	                <code value="129472003"/>
	                <display value="Alemtuzamab (substance)"/>
	            </coding>
	        </itemCodeableConcept>
	    </ingredient>
	</Medication>
	'
)

resource <- fhir_resource_xml(resource)

example_resource3 <- fhir_serialize(resource)

usethis::use_data(example_resource3, overwrite = TRUE)
