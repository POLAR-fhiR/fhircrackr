## code to prepare `example_bundles4` dataset goes here
bundle <- xml2::read_xml(
	"<Bundle>
     <type value='searchset'/>
	 <entry>
		 <resource>
			<Medication>
				<id value='8266'/>
			    <code>
			        <coding>
			            <system value='http://www.nlm.nih.gov/research/umls/rxnorm'/>
			            <code value='1594660'/>
			            <display value='Alemtuzumab 10mg/ml (Lemtrada)'/>
			        </coding>
			    </code>
			    <ingredient id='1'>
			        <itemCodeableConcept>
			            <coding>
			                <system value='http://snomed.info/sct'/>
			                <code value='11713004'/>
			                <display value='Water (substance)'/>
			            </coding>
			        </itemCodeableConcept>
			    </ingredient>
			    <ingredient id='2'>
			        <itemCodeableConcept>
			            <coding>
			                <system value='http://snomed.info/sct'/>
			                <code value='129472003'/>
			                <display value='Alemtuzamab (substance)'/>
			            </coding>
			        </itemCodeableConcept>
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
                <ingredient>
                    <itemCodeableConcept>
                        <coding>
                            <system value='http://www.nlm.nih.gov/research/umls/rxnorm'/>
                            <code value='315266'/>
                            <display value='Acetaminophen 500 MG'/>
                        </coding>
                    </itemCodeableConcept>
                    <strength>
                        <numerator>
                            <value value='500'/>
                            <system value='http://unitsofmeasure.org'/>
                            <code value='mg'/>
                        </numerator>
                        <denominator>
                            <value value='1'/>
                            <system value='http://terminology.hl7.org/CodeSystem/v3-orderableDrugForm'/>
                            <code value='Tab'/>
                        </denominator>
                    </strength>
                </ingredient>
                <ingredient>
                    <itemCodeableConcept>
                        <coding>
                            <system value='http://www.nlm.nih.gov/research/umls/rxnorm'/>
                            <code value='901813'/>
                            <display value='Diphenhydramine Hydrochloride 25 mg'/>
                        </coding>
                    </itemCodeableConcept>
                    <strength>
                        <numerator>
                            <value value='25'/>
                            <system value='http://unitsofmeasure.org'/>
                            <code value='mg'/>
                        </numerator>
                        <denominator>
                            <value value='1'/>
                            <system value='http://terminology.hl7.org/CodeSystem/v3-orderableDrugForm'/>
                            <code value='Tab'/>
                        </denominator>
                    </strength>
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
