#This example use fhir_search to find all FHIR Resources:MedicationStatement with snomed.code 429374003 (Simvastatin 40mg)
#Additional all FHIR Patient Resources that are related to the MedicationStatement Resource will be searched.
#This example works only at the hapi.fhir.org server.
#start this example with
# Rscript example.R

tables_design <- list(
	MedikationStatement = list(
		".//MedicationStatement",
		list(
			AID = "id/@value",
			STATUS.TEXT ="text/status/@value",
			STATUS = "status/@value",
			MEDICATION.SYSTEM  = "medicationCodeableConcept/coding/system/@value",
			MEDICATION.CODE    = "medicationCodeableConcept/coding/code/@value",
			MEDICATION.DISPLAY = "medicationCodeableConcept/coding/display/@value",
			DOSAGE  = "dosage/text/@value",
			PATIENT = "subject/reference/@value",
			LAST.UPDATE  = "meta/lastUpdated/@value"
		)
	),
	Patients = list(
		".//Patient",
		list( 
			PID           = "id/@value", 
			NAME.USE      = "name/use/@value", 
			NAME.GIVEN    = "name/given/@value", 
			NAME.FAMILY   = "name/family/@value",
			SEX           = "gender/@value", 
			BIRTHDAY       = "birthDate/@value" 
		)
	)
)

url  <- paste0(
  "https://hapi.fhir.org/baseR4/",
  "MedicationStatement?",
  "code=http://snomed.info/ct|429374003",
  "&_include=MedicationStatement:subject")

bundles <- fhiR::fhir_search(url)

list_of_tables <- fhiR::fhir2dfs(bundle = bundles, design = tables_design)

for(n in names(list_of_tables)) {
	
	write.table(list_of_tables[[ n ]], file = paste0(n, ".csv"), na = "", sep = ";", dec = ".", row.names = F, quote=F)
}

save(list_of_tables, file = "tables.RData")
cat("   - finish\n")







