#This example use fhir_search to find all FHIR Resources:MedicationStatement with snomed.code 429374003 (Simvastatin 40mg)
#Additional all FHIR Patient Resources that are related to the MedicationStatement Resource will be searched.
#This example works only at the hapi.fhir.org server.
#start this example with
#Rscript example.R -o result
#last change: 2020_06_16

#devtools::install_github("POLAR-fhiR/fhiR", quiet = T)
rm(list = ls( ))
out_dir <- "result"

endpoint <- "https://hapi.fhir.org/baseR4"

fhir_search_request  <- paste0(
	"MedicationStatement?",
	"code=http://snomed.info/ct|429374003",
	"&_include=MedicationStatement:subject",
	"&_format=xml",
	"&_pretty=true",
	"&_count=500000")

url <- fhiR::paste_paths(path1 = endpoint, path2 = fhir_search_request)

tables_design <- list(
	MedikationStatement = list(
		".//MedicationStatement",
		list(
			AID = "id/@value",
			STATUS.TEXT ="text/status/@value",
			STATUS = "status/@value",
			MEDIKATION.SYSTEM  = "medicationCodeableConcept/coding/system/@value",
			MEDIKATION.CODE    = "medicationCodeableConcept/coding/code/@value",
			MEDIKATION.ANZEIGE = "medicationCodeableConcept/coding/display/@value",
			DOSAGE  = "dosage/text/@value",
			PATIENT = "subject/reference/@value",
			LAST.UPDATE  = "meta/lastUpdated/@value"
		)
	),
	Patients = list(
		".//Patient",
		list( 
			PID             = "id/@value", 
			NAME.VERWENDUNG = "name/use/@value", 
			VORNAME         = "name/given/@value", 
			NACHNAME        = "name/family/@value",
			GESCHLECHT      = "gender/@value", 
			GEBURTSTAG      = "birthDate/@value" 
		)
	)
)


bundles <- fhiR::get_bundle(request = url, verbose = T)

list_of_tables <- fhiR::bundle2dfs(bundle = bundles, design = tables_design)


if(! dir.exists(out_dir)) {
  
  cat(paste0("   - create directory: ", out_dir, "...\n"))
  
  dir.create(out_dir, recursive = T)
  
} else cat(paste0("   - directory ", out_dir, " already exists. do nothing...\n"))

cat("   - write csv tables...\n")

for(n in names(list_of_tables)) {
	
	write.table(list_of_tables[[ n ]], file = paste0( "result/", n, ".csv" ), na = "", sep = ";", dec = ".", row.names = F, quote=F)
}

cat("   - write tables.RData...\n")
save(list_of_tables, file = "result/tables.RData")
cat("   - finish\n")







