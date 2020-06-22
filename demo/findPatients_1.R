#This example searches for all FHIR Resources:Patient
#This example works only at the hapi.fhir.org server.


tables_design <- list(
	Patients = list(
		".//Patient",
		list(
			PID           = "id/@value",
			NAME.USE      = "name/use/@value",
			NAME.GIVEN    = "name/given/@value",
			NAME.FAMILY   = "name/family/@value",
			SEX           = "gender/@value",
			BIRTHDAY      = "birthDate/@value"
		)
	)
)


bundles <- fhircrackr::fhir_search("https://hapi.fhir.org/baseR4/Patient?", max.bundles=3)

list_of_tables <- fhircrackr::fhir_crack(bundles = bundles, design = tables_design, sep = "", add_ids = T)

list_of_tables$Patients





