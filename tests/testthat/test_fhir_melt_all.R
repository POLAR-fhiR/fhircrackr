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
	"fhir_melt_all has the same result as multiple fhir_melt: example1. Here we have the column 'id' without any brackets.
	Check for presents of column 'id' after fhir_melt and fhir_melt_all",{
		bundles <- fhir_unserialize(example_bundles1)
		d <- fhir_crack(bundles,
						design = fhir_table_description(
							resource = "Patient",
							brackets = brackets,
							sep = sep),
						data.table = T,
						verbose = 0
		)

		# Create a regular expression using the brackets
		pattern <- paste0("\\", brackets[1], ".*\\", brackets[2])
		# Remove the content inside brackets in column 'id', including the brackets themselves
		d[, id := gsub(pattern, "", id)]

		d1 <- fhir_melt(d, columns = fhir_common_columns(d, "address"), brackets = brackets, sep = sep, all_columns = T)
		d1 <- fhir_melt(d1, columns = "name.given", brackets = brackets, sep = sep, all_columns = T)
		d1 <- fhir_rm_indices(d1, brackets = brackets)
		d1[, resource_identifier:=NULL]

		# Verify that 'id' column is still present after multiple melts
		testthat::expect_true("id" %in% colnames(d1), info = "Column 'id' should still be present after multiple fhir_melt operations.")

		# Apply fhir_melt_all and check that 'id' is preserved
		d2 <- fhir_melt_all(d, brackets = brackets, sep = sep)
		testthat::expect_true("id" %in% colnames(d2), info = "Column 'id' should be present after fhir_melt_all operation.")

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

testthat::test_that(
	"fhir_melt_all supports FHIR path column names with slash separators",{
		d <- data.table::data.table(
			id = c("[1]resource-a", "[1]resource-b"),
			`meta/profile` = c("[1]profile-a", "[1]profile-b"),
			`identifier/system` = c("[1]system-a|[2]system-b", "[1]system-c"),
			`identifier/value` = c("[1]value-a|[2]value-b", "[1]value-c"),
			`component/code/coding/system` = c("[1.1]loinc|[1.2]snomed|[2.1]ucum", "[1.1]loinc"),
			`component/code/coding/code` = c("[1.1]code-a|[1.2]code-b|[2.1]code-c", "[1.1]code-d"),
			`component/valueQuantity/value` = c("[1]1.0|[2]2.0", "[1]3.0")
		)

		get_path_columns <- function(data_frame, column_names_prefix) {
			pattern <- paste0("^", column_names_prefix, "($|/)")
			grep(pattern, names(data_frame), value = TRUE)
		}

		d1 <- fhir_melt(d, columns = get_path_columns(d, "identifier"), brackets = brackets, sep = sep, all_columns = T)
		d1 <- fhir_melt(d1, columns = get_path_columns(d1, "component"), brackets = brackets, sep = sep, all_columns = T)
		d1 <- fhir_melt(d1, columns = get_path_columns(d1, "component/code"), brackets = brackets, sep = sep, all_columns = T)
		d1 <- fhir_melt(d1, columns = get_path_columns(d1, "component/code/coding"), brackets = brackets, sep = sep, all_columns = T)
		d1 <- fhir_rm_indices(d1, brackets = brackets)
		d1[, resource_identifier:=NULL]

		d2 <- fhir_melt_all(d, brackets = brackets, sep = sep, column_name_separator = "/")

		testthat::expect_identical(d1, d2)
	}
)

testthat::test_that(
	"fhir_melt_all handles single slash-separated leaf columns",{
		repeating_leaf <- data.table::data.table(
			id = c("[1]resource-a", "[1]resource-b"),
			`note/text` = c("[1]note-a|[2]note-b", "[1]note-c")
		)
		expected_repeating_leaf <- data.table::data.table(
			id = c("resource-a", "resource-a", "resource-b"),
			`note/text` = c("note-a", "note-b", "note-c")
		)

		nested_leaf <- data.table::data.table(
			id = c("[1]resource-a", "[1]resource-b"),
			`component/code/coding/code` = c("[1.1]code-a|[1.2]code-b", "[1.1]code-c")
		)
		expected_nested_leaf <- data.table::data.table(
			id = c("resource-a", "resource-a", "resource-b"),
			`component/code/coding/code` = c("code-a", "code-b", "code-c")
		)

		testthat::expect_identical(
			fhir_melt_all(repeating_leaf, brackets = brackets, sep = sep, column_name_separator = "/"),
			expected_repeating_leaf
		)
		testthat::expect_identical(
			fhir_melt_all(nested_leaf, brackets = brackets, sep = sep, column_name_separator = "/"),
			expected_nested_leaf
		)
	}
)

testthat::test_that(
	"fhir_melt_all preserves sparse coding attributes by index",{
		d <- data.table::data.table(
			id = "[1]resource-a",
			`code/coding/code` = "[1.1]code-a|[1.2]code-b",
			`code/coding/system` = "[1.2]system-b",
			`code/coding/display` = "[1.1]display-a"
		)
		expected <- data.table::data.table(
			id = c("resource-a", "resource-a"),
			`code/coding/code` = c("code-a", "code-b"),
			`code/coding/system` = c(NA_character_, "system-b"),
			`code/coding/display` = c("display-a", NA_character_)
		)

		testthat::expect_identical(
			fhir_melt_all(d, brackets = brackets, sep = sep, column_name_separator = "/"),
			expected
		)
	}
)

testthat::test_that(
	"fhir_melt_all preserves collapsed slash-separated patient names",{
		d <- data.table::data.table(
			id = "[1]patient-a",
			`name/family` = "[1.1]Smith|[2.1]Baker",
			`name/given` = "[1.1]Marie|[1.2]Luise|[2.1]Lea|[2.2]Sophie|[2.3]Anna",
			`name/use` = "[1.1]official|[2.1]nickname"
		)
		expected <- data.table::data.table(
			id = c("patient-a", "patient-a"),
			`name/family` = c("Smith", "Baker"),
			`name/given` = c("Marie Luise ", "Lea Sophie Anna"),
			`name/use` = c("official", "nickname")
		)

		d <- fhir_collapse(d, columns = "name/given", sep = sep, brackets = brackets)

		testthat::expect_identical(
			fhir_melt_all(d, brackets = brackets, sep = sep, column_name_separator = "/"),
			expected
		)
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
