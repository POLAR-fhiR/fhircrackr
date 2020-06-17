#This example use fhir_search to find all FHIR Resources:MedicationStatement with snomed.code 429374003 (Simvastatin 40mg)
#Additional all FHIR Patient Resources that are related to the MedicationStatement Resource will be searched.
#This example works only at the hapi.fhir.org server.
#start this example with
#Rscript spec.R -o result
#last change: 2020_06_16
###
# endpoint of fhir r4 Servers
###

endpoint <- "https://hapi.fhir.org/baseR4"

devtools::install_github( "POLAR-fhiR/fhiR", quiet = T )

###
# fhir.search.request ohne Endpunktangabe
###
fhir.search.request <- paste0(
	"MedicationStatement?",
	"code=http://snomed.info/ct|429374003",
	"&_include=MedicationStatement:subject",
	"&_include=MedicationStatement:encounter",
	"&_format=xml",
	"&_pretty=true",
	"&_count=500000")

###http://www.whocc.no/atc 
# Welche Daten aus den Pages sollen wie in welchen Tabellen erzeugt werden
# Hier nur eine Tabelle Patient mit den Einträgen PID, Geschlecht und Geburtsdatum
###
tables.design <- list(
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
			DOSAGE.TIMING ="dosage/timing/repeat/frequency/@value",
			DOSAGE.PERIOD ="dosage/timing/repeat/period/@value",
			DOSAGE.PERIOD.UNIT ="dosage/timing/repeat/periodUnit/@value",
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

###
# filtere Daten in Tabellen vor dem Export ins Ausgabeverzeichnis
###
url     <- fhiR::paste.paths( path1 = endpoint, path2 = fhir.search.request )

bundles <- fhiR::get.bundles( request = url, max.bundles = max.bundles, verbose = T )

list.of.tables <- fhiR::bundles2dfs( bundles = bundles, design = tables.design, sep = separator )

cat( "   - 5.2 write csv tables...\n" )

for( n in names( list.of.tables ) ) {
	
	write.table( list.of.tables[[ n ]], file = paste0( n, ".csv" ), na = "", sep = ";", dec = ".", row.names = F, quote=F )
}

cat( "   - 5.3 write tables.RData...\n" )

save( list.of.tables, file = "tables.RData" )




post.processing <- function( lot ) {
	
	
    ###
    
    ###
	lot
}

#write.table(paste0(endpoint,"/",fhir.search.request), "fhir.search.request.txt",
#				   col.names =FALSE, row.names =FALSE, quote=FALSE)








