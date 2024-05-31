brackets = c("[", "]")
sep = "|"

testthat::test_that(
	"fhir_melt_all has the same result as multiple fhir_melt: example1",{
		bundles <- fhir_unserialize(example_bundles1)
		d <- fhir_crack(bundles,
						design = fhir_table_description(
							resource = "Patient",
							brackets = brackets,
							sep = sep),
						data.table = T,
						verbose = 0
		)

		d1 <- fhir_melt(d, columns = fhir_common_columns(d, "address"), brackets = brackets, sep = sep, all_columns = T)
		d1 <- fhir_melt(d1, columns = "name.given", brackets = brackets, sep = sep, all_columns = T)
		d1 <- fhir_rm_indices(d1, brackets = brackets)
		d1[, resource_identifier:=NULL]

		d2 <- fhir_melt_all(d, brackets = brackets, sep = sep)

		testthat::expect_identical(d1, d2)
	}
)


testthat::test_that(
	"fhir_melt_all has the same result as multiple fhir_melt: example5",{
		bundles <- fhir_unserialize(example_bundles5)
		d <- fhir_crack(bundles,
						design = fhir_table_description(
							resource = "Observation",
							brackets = brackets,
							sep = sep),
						data.table = T,
						verbose = 0
		)

		d1 <- fhir_melt(d, columns = fhir_common_columns(d, "code"), brackets = brackets, sep = sep, all_columns = T)
		d1 <- fhir_melt(d1, columns = fhir_common_columns(d, "code"), brackets = brackets, sep = sep, all_columns = T)
		d1 <- fhir_rm_indices(d1, brackets = brackets)
		d1[, resource_identifier:=NULL]

		d2 <- fhir_melt_all(d, brackets = brackets, sep = sep)

		testthat::expect_identical(d1, d2)
	}
)

testthat::test_that(
	"fhir_melt_all has the same result as multiple fhir_melt: example4",{
		bundles <- fhir_unserialize(example_bundles4)
		d <- fhir_crack(bundles,
						design = fhir_table_description(
							resource = "Medication",
							brackets = brackets,
							sep = sep),
						data.table = T,
						verbose = 0
		)

		d1 <- fhir_melt(d, columns = fhir_common_columns(d, "ingredient"), brackets = brackets, sep = sep, all_columns = T)
		d1 <- fhir_rm_indices(d1, brackets = brackets)
		d1[, resource_identifier:=NULL]

		d2 <- fhir_melt_all(d, brackets = brackets, sep = sep)

		testthat::expect_identical(d1, d2)
	}
)

## longer examples

bundle1 <- fhir_bundle_list(list(fhir_bundle_xml(xml2::read_xml(
	"<Bundle>
     <type value='searchset'/>
     <entry>
    	<resource>
			<Patient>
        <id value='mii-exa-person-patient-full'/>
        <meta>
          <profile value='https://www.medizininformatik-initiative.de/fhir/core/modul-person/StructureDefinition/Patient%7C2024.0.0'/>
        </meta>
        <name>
          <use value='official'/>
          <family value='Van-der-Dussen'/>
          <given value='Maja'/>
          <given value='Julia'/>
          <prefix value='Prof. Dr. med.'/>
        </name>
        <name>
          <given value='Maja2'/>
          <given value='Julia2'/>
          <use value='maiden'/>
          <family value='Haffer'/>
        </name>
        <identifier>
          <use value='usual'/>
          <type>
            <coding>
              <code value='MR'/>
              <system value='http://terminology.hl7.org/CodeSystem/v2-0203'/>
            </coding>
          </type>
          <system value='https://www.charite.de/fhir/sid/patienten'/>
          <value value='42285243'/>
          <assigner>
            <display value='Charité – Universitätsmedizin Berlin'/>
            <identifier>
              <value value='Charité'/>
              <system value='http://fhir.de/sid/arge-ik/iknr'/>
            </identifier>
          </assigner>
        </identifier>
        <identifier>
          <use value='usual'/>
          <type>
            <coding>
              <code value='GKV'/>
              <system value='http://fhir.de/CodeSystem/identifier-type-de-basis'/>
            </coding>
          </type>
          <system value='http://fhir.de/sid/gkv/kvid-10'/>
          <value value='Z234567890'/>
          <assigner>
            <identifier>
              <use value='official'/>
              <value value='109519005'/>
              <system value='http://fhir.de/sid/arge-ik/iknr'/>
            </identifier>
          </assigner>
        </identifier>
        <identifier>
          <use value='secondary'/>
          <type>
            <coding>
              <code value='PKV'/>
              <system value='http://fhir.de/CodeSystem/identifier-type-de-basis'/>
            </coding>
          </type>
          <value value='123456'/>
          <assigner>
            <display value='Signal Iduna'/>
          </assigner>
        </identifier>
        <gender value='other'/>
        <birthDate value='1998-09-19'/>
        <deceasedBoolean value='false'/>
        <address>
          <type value='both'/>
          <line value='Anna-Louisa-Karsch Str. 2'/>
          <city value='Berlin'/>
          <state value='DE-BE'/>
          <postalCode value='10178'/>
          <country value='DE'/>
        </address>
        <address>
          <line value='Härtelstr. 16-18'/>
          <city value='Leipzig'/>
          <state value='DE-BE'/>
          <postalCode value='04107'/>
          <country value='DE'/>
        </address>
        <managingOrganization>
          <reference value='Organization/Charite-Universitaetsmedizin-Berlin'/>
        </managingOrganization>
      </Patient>
    </resource>
  </entry>
</Bundle>"
))))

bundle2 <- fhir_bundle_list(list(fhir_bundle_xml(xml2::read_xml(
	'<Bundle xmlns="http://hl7.org/fhir">
  <entry>
    <fullUrl value="Encounter/Polar-WP1.1-01156-E-1"/>
    <resource>
      <Encounter xmlns="http://hl7.org/fhir">
        <id value="Polar-WP1.1-01156-E-1"/>
        <meta>
          <profile value="https://www.medizininformatik-initiative.de/fhir/core/modul-fall/StructureDefinition/KontaktGesundheitseinrichtung"/>
        </meta>
        <identifier>
          <type>
            <coding>
              <system value="http://terminology.hl7.org/CodeSystem/v2-0203"/>
              <code value="VN"/>
            </coding>
          </type>
          <system value="http://dummyurl"/>
          <value value="Polar-WP1.1-01156-E-1"/>
          <assigner>
            <identifier>
              <system value="https://www.medizininformatik-initiative.de/fhir/core/NamingSystem/org-identifier"/>
              <value value="POLARWP"/>
            </identifier>
          </assigner>
        </identifier>
        <status value="finished"/>
        <class>
          <system value="http://terminology.hl7.org/CodeSystem/v3-ActCode"/>
          <code value="IMP"/>
          <display value="inpatient encounter"/>
        </class>
        <subject>
          <reference value="Patient/Polar-WP1.1-01156"/>
        </subject>
        <period>
          <start value="2019-01-01T00:00:00+01:00"/>
          <end value="2019-01-05T00:00:00+01:00"/>
        </period>
        <diagnosis>
          <condition>
            <reference value="Condition/Polar-WP1.1-01156-CD-1"/>
          </condition>
          <use>
            <coding>
              <system value="http://terminology.hl7.org/CodeSystem/diagnosis-role"/>
              <code value="CM"/>
              <display value="Comorbidity diagnosis"/>
            </coding>
          </use>
        </diagnosis>
        <diagnosis>
          <condition>
            <reference value="Condition/Polar-WP1.1-01156-CD-2"/>
          </condition>
          <use>
            <coding>
              <system value="http://terminology.hl7.org/CodeSystem/diagnosis-role"/>
              <code value="CC"/>
              <display value="Chief complaint"/>
            </coding>
          </use>
        </diagnosis>
      </Encounter>
    </resource>
    <request>
      <method value="PUT"/>
      <url value="Encounter/Polar-WP1.1-01156-E-1"/>
    </request>
  </entry>
</Bundle>'
))))

testthat::test_that(
	"fhir_melt_all has the same result as multiple fhir_melt: long bundle 2",{
		d <- fhir_crack(bundle2,
						design = fhir_table_description(
							resource = "Encounter",
							brackets = brackets,
							sep = sep),
						data.table = T,
						verbose = 0
		)

		d1 <- fhir_melt(d, columns = fhir_common_columns(d, "diagnosis"), brackets = brackets, sep = sep, all_columns = T)
		d1 <- fhir_rm_indices(d1, brackets = brackets)
		d1[, resource_identifier:=NULL]

		d2 <- fhir_melt_all(d, brackets = brackets, sep = sep)

		testthat::expect_identical(d1, d2)
	}
)

testthat::test_that(
	"fhir_melt_all has the same result as multiple fhir_melt: long bundle 1",{
		d <- fhir_crack(bundle1,
						design = fhir_table_description(
							resource = "Patient",
							brackets = brackets,
							sep = sep),
						data.table = T,
						verbose = 0
		)

		d <- fhir_collapse(d, columns = "name.given", sep = sep, brackets = brackets)

		d1 <- fhir_melt(d, columns = fhir_common_columns(d, "address"), brackets = brackets, sep = sep, all_columns = T)
		d1 <- fhir_melt(d1, columns = fhir_common_columns(d, "identifier"), brackets = brackets, sep = sep, all_columns = T)
		d1 <- fhir_melt(d1, columns = fhir_common_columns(d, "name"), brackets = brackets, sep = sep, all_columns = T)
		d1 <- fhir_rm_indices(d1, brackets = brackets)
		d1[, resource_identifier:=NULL]

		d2 <- fhir_melt_all(d, brackets = brackets, sep = sep)

		testthat::expect_identical(d1, d2)
	}
)
